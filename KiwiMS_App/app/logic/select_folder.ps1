# app/logic/select_folder.ps1
#
# Shows the native Windows folder picker and writes the chosen path to -OutFile.
#
# This exists because shinyFiles walks the directory tree in the R process:
# traverseDirs() calls dir_ls() plus a dir.exists() stat on every entry, for
# every expanded node, on every click - and R is single threaded, so that walk
# blocks the whole Shiny app, not just the picker. On a corporate machine with
# redirected profile folders or mapped shares, each of those stats is a network
# round trip and the app appears to hang.
#
# The shell dialog does none of that in our process: it enumerates lazily, only
# for the node actually being opened, with the shell's own caching and timeouts
# for offline shares. R just waits for a path to appear in a file.
#
# Protocol: writes two lines to -OutFile - "OK" plus the path, or "CANCEL" on
# its own. Written to a sibling .part file and renamed, so a reader that polls
# for the file never observes a half written one.

param(
    [Parameter(Mandatory = $true)][string]$OutFile,
    [string]$InitialDir = '',
    [string]$Title = 'Select folder',
    [switch]$SelfTest
)

$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.Windows.Forms
Add-Type -AssemblyName System.Drawing

# The Vista-era common item dialog (the one with a path bar, Quick Access and
# type-ahead), rather than WinForms' FolderBrowserDialog, which is the old
# SHBrowseForFolder tree with no way to paste a UNC path. Declaring the COM
# interfaces by hand is the price of not depending on the WindowsAPICodePack.
# Every method must stay listed in vtable order even where unused, hence the
# members below that are never called.
#
# Add-Type compiles C# on the fly, which an AppLocker or WDAC policy can refuse
# - plausible on exactly the locked-down corporate machines this change is for.
# So treat the modern dialog as an upgrade, not a requirement, and fall back to
# WinForms' FolderBrowserDialog, which lives in a pre-compiled framework
# assembly and needs no compiler. $HaveModern records which one we got.
$HaveModern = $false
try {
    if (-not ('KiwiMS.FolderDialog' -as [type])) {
        Add-Type -Language CSharp `
            -ReferencedAssemblies System.Windows.Forms, System.Drawing `
            -TypeDefinition @'
using System;
using System.Runtime.InteropServices;

namespace KiwiMS {

  [ComImport, Guid("43826D1E-E718-42EE-BC55-A1E261C37BFE"),
   InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  public interface IShellItem {
    void BindToHandler(IntPtr pbc, ref Guid bhid, ref Guid riid, out IntPtr ppv);
    void GetParent(out IShellItem ppsi);
    void GetDisplayName(uint sigdnName, out IntPtr ppszName);
    void GetAttributes(uint sfgaoMask, out uint psfgaoAttribs);
    void Compare(IShellItem psi, uint hint, out int piOrder);
  }

  [ComImport, Guid("42F85136-DB7E-439C-85F1-E4075D135FC8"),
   InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  public interface IFileOpenDialog {
    [PreserveSig] int Show(IntPtr parent);
    void SetFileTypes(uint cFileTypes, IntPtr rgFilterSpec);
    void SetFileTypeIndex(uint iFileType);
    void GetFileTypeIndex(out uint piFileType);
    void Advise(IntPtr pfde, out uint pdwCookie);
    void Unadvise(uint dwCookie);
    void SetOptions(uint fos);
    void GetOptions(out uint pfos);
    void SetDefaultFolder(IShellItem psi);
    void SetFolder(IShellItem psi);
    void GetFolder(out IShellItem ppsi);
    void GetCurrentSelection(out IShellItem ppsi);
    void SetFileName([MarshalAs(UnmanagedType.LPWStr)] string pszName);
    void GetFileName([MarshalAs(UnmanagedType.LPWStr)] out string pszName);
    void SetTitle([MarshalAs(UnmanagedType.LPWStr)] string pszTitle);
    void SetOkButtonLabel([MarshalAs(UnmanagedType.LPWStr)] string pszText);
    void SetFileNameLabel([MarshalAs(UnmanagedType.LPWStr)] string pszLabel);
    void GetResult(out IShellItem ppsi);
    void AddPlace(IShellItem psi, int fdap);
    void SetDefaultExtension([MarshalAs(UnmanagedType.LPWStr)] string pszExt);
    void Close(int hr);
    void SetClientGuid(ref Guid guid);
    void ClearClientData();
    void SetFilter(IntPtr pFilter);
    void GetResults(out IntPtr ppenum);
    void GetSelectedItems(out IntPtr ppsai);
  }

  [ComImport, Guid("DC1C5A9C-E88A-4DDE-A5A1-60F82A20AEF7")]
  public class FileOpenDialogRCW { }

  public static class FolderDialog {

    const uint FOS_PICKFOLDERS      = 0x00000020;
    const uint FOS_FORCEFILESYSTEM  = 0x00000040;
    const uint FOS_PATHMUSTEXIST    = 0x00000800;
    const uint FOS_DONTADDTORECENT  = 0x02000000;
    const uint SIGDN_FILESYSPATH    = 0x80058000;
    const int  ERROR_CANCELLED      = unchecked((int)0x800704C7);

    [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = false)]
    static extern void SHCreateItemFromParsingName(
      [MarshalAs(UnmanagedType.LPWStr)] string pszPath, IntPtr pbc,
      ref Guid riid, [MarshalAs(UnmanagedType.Interface)] out object ppv);

    public struct RECT { public int Left, Top, Right, Bottom; }

    [DllImport("user32.dll")] static extern IntPtr GetWindow(IntPtr hWnd, uint uCmd);
    [DllImport("user32.dll")] static extern bool GetWindowRect(IntPtr hWnd, out RECT r);
    [DllImport("user32.dll")] static extern bool SetWindowPos(
      IntPtr hWnd, IntPtr after, int X, int Y, int cx, int cy, uint flags);

    const uint GW_ENABLEDPOPUP = 6;
    const uint SWP_NOSIZE = 0x0001, SWP_NOZORDER = 0x0004, SWP_NOACTIVATE = 0x0010;

    // The dialog is the enabled popup owned by our anchor, so it can be found
    // without enumerating every window on the desktop.
    //
    // Centring has to be done by hand: the shell restores the position and size
    // the dialog last had (per user, in the registry), and the owner window is
    // only a hint it may ignore entirely. A KiwiMS user who once dragged it into
    // a corner - or who inherited a stale position from a different monitor
    // layout - would otherwise keep getting it there.
    public static bool CenterOwnedDialog(IntPtr owner) {
      IntPtr dlg = GetWindow(owner, GW_ENABLEDPOPUP);
      if (dlg == IntPtr.Zero || dlg == owner) return false;

      RECT r;
      if (!GetWindowRect(dlg, out r)) return false;
      int w = r.Right - r.Left, h = r.Bottom - r.Top;
      if (w <= 0 || h <= 0) return false;

      // WorkingArea, not Bounds: never centre underneath the taskbar. Clamped so
      // a dialog larger than the monitor still has its title bar reachable.
      var area = System.Windows.Forms.Screen.FromHandle(owner).WorkingArea;
      int x = Math.Max(area.Left, area.Left + (area.Width - w) / 2);
      int y = Math.Max(area.Top, area.Top + (area.Height - h) / 2);

      return SetWindowPos(dlg, IntPtr.Zero, x, y, 0, 0,
                          SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
    }

    static IFileOpenDialog Create(string initialDir, string title) {
      var dlg = (IFileOpenDialog)(new FileOpenDialogRCW());
      dlg.SetOptions(FOS_PICKFOLDERS | FOS_FORCEFILESYSTEM |
                     FOS_PATHMUSTEXIST | FOS_DONTADDTORECENT);
      if (!String.IsNullOrEmpty(title)) dlg.SetTitle(title);

      // A starting folder is a convenience, never a reason to fail: the saved
      // path may sit on a share that is gone since it was saved.
      if (!String.IsNullOrEmpty(initialDir)) {
        try {
          var iid = typeof(IShellItem).GUID;
          object item;
          SHCreateItemFromParsingName(initialDir, IntPtr.Zero, ref iid, out item);
          dlg.SetFolder((IShellItem)item);
        } catch { }
      }
      return dlg;
    }

    // Verifies the vtable layout without opening a window, so the interop can
    // be regression tested from a headless script.
    public static bool SelfTest() {
      var dlg = Create(null, "selftest");
      uint opts;
      dlg.GetOptions(out opts);
      Marshal.ReleaseComObject(dlg);
      return (opts & FOS_PICKFOLDERS) != 0;
    }

    public static string Show(string initialDir, string title, IntPtr owner) {
      var dlg = Create(initialDir, title);
      try {
        int hr = dlg.Show(owner);
        if (hr == ERROR_CANCELLED) return null;
        if (hr != 0) throw Marshal.GetExceptionForHR(hr);

        IShellItem res;
        dlg.GetResult(out res);
        IntPtr pszPath;
        res.GetDisplayName(SIGDN_FILESYSPATH, out pszPath);
        try {
          return Marshal.PtrToStringUni(pszPath);
        } finally {
          Marshal.FreeCoTaskMem(pszPath);
          Marshal.ReleaseComObject(res);
        }
      } finally {
        Marshal.ReleaseComObject(dlg);
      }
    }
  }
}
'@
    }
    $HaveModern = [KiwiMS.FolderDialog]::SelfTest()
}
catch {
    $HaveModern = $false
}

if ($SelfTest) {
    if ($HaveModern) { Write-Output 'SELFTEST OK (modern)'; exit 0 }
    Write-Output 'SELFTEST OK (legacy fallback only)'; exit 2
}

# Written by a background process while the browser has focus, so the dialog
# needs an owner that is itself topmost or it opens behind the browser window -
# which would look exactly like the freeze this replaces.
#
# The owner is made invisible with Opacity, NOT by parking it off-screen and not
# by hiding it: the common item dialog can position itself relative to its owner,
# so an owner at (-2000,-2000) or one hidden via the child's STARTUPINFO risks
# putting the dialog somewhere the user cannot see it - indistinguishable from
# "the dialog never opened". Centred and fully transparent is safe either way.
$anchor = New-Object System.Windows.Forms.Form
$anchor.TopMost = $true
$anchor.ShowInTaskbar = $false
$anchor.FormBorderStyle = 'None'
$anchor.Size = New-Object System.Drawing.Size(1, 1)
$anchor.StartPosition = 'CenterScreen'
$anchor.Opacity = 0

# Both dialogs below are modal and run their own message loop on this thread,
# and that loop dispatches WM_TIMER - so a WinForms timer started here keeps
# ticking while the dialog is up. That is the only opening we get to reposition
# it: the call to show it does not return until the user is finished.
$state = [pscustomobject]@{ Done = $false; Ticks = 0 }
$timer = New-Object System.Windows.Forms.Timer
$timer.Interval = 40
$timer.Add_Tick({
        $state.Ticks++
        if ($state.Done -or $state.Ticks -gt 125) {
            # ~5s. The dialog is normally found on the first tick; giving up
            # keeps a shell that never opened one from spinning forever.
            $timer.Stop()
            return
        }
        if ([KiwiMS.FolderDialog]::CenterOwnedDialog($anchor.Handle)) {
            $state.Done = $true
            $timer.Stop()
        }
    })

$result = $null
try {
    $anchor.Show()
    $anchor.Activate()
    # Covers both branches: either dialog is an owned popup of the anchor.
    $timer.Start()

    if ($HaveModern) {
        $result = [KiwiMS.FolderDialog]::Show($InitialDir, $Title, $anchor.Handle)
    }
    else {
        # SHBrowseForFolder under the hood: an older tree-style picker, but still
        # the shell's own, so it stays lazy per node and never blocks R.
        $fbd = New-Object System.Windows.Forms.FolderBrowserDialog
        try {
            $fbd.Description = $Title
            $fbd.ShowNewFolderButton = $true
            if ($InitialDir -and (Test-Path -LiteralPath $InitialDir)) {
                $fbd.SelectedPath = $InitialDir
            }
            if ($fbd.ShowDialog($anchor) -eq [System.Windows.Forms.DialogResult]::OK) {
                $result = $fbd.SelectedPath
            }
        }
        finally { $fbd.Dispose() }
    }
}
finally {
    $timer.Stop()
    $timer.Dispose()
    $anchor.Close()
    $anchor.Dispose()
}

$payload = if ([string]::IsNullOrEmpty($result)) { "CANCEL`n" } else { "OK`n$result`n" }

# UTF-8 without BOM: paths routinely carry umlauts here, and the R side reads
# this with encoding="UTF-8", which does not strip a BOM.
$part = "$OutFile.part"
[System.IO.File]::WriteAllText($part, $payload, (New-Object System.Text.UTF8Encoding($false)))
[System.IO.File]::Move($part, $OutFile)
