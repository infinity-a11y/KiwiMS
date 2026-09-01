#define KiwiMSLogFile "{localappdata}\KiwiMS\kiwims_setup.log"

; The app version is read at compile time from the single source of truth,
; KiwiMS_App\resources\version.txt (first line, "version=x.y.z").
; Never hard-code it here - bump it with KiwiMS_App\dev\set-version.ps1.
#define VersionFilePath AddBackslash(SourcePath) + "KiwiMS_App\resources\version.txt"
#define VersionHandle FileOpen(VersionFilePath)
#if VersionHandle == 0
  #error Cannot open KiwiMS_App\resources\version.txt
#endif
#define VersionLine FileRead(VersionHandle)
#expr FileClose(VersionHandle)
#define AppVer Trim(Copy(VersionLine, Pos("=", VersionLine) + 1, Len(VersionLine)))
#if AppVer == ""
  #error Could not parse a version from KiwiMS_App\resources\version.txt
#endif

[Setup]
AppName=KiwiMS
AppId=KiwiMS
AppVersion={#AppVer}
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiMS
DefaultGroupName=KiwiMS
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=KiwiMS-Windows-x86_64
SetupIconFile=setup\favicon.ico
UninstallDisplayIcon={app}\favicon.ico
WizardImageFile=setup\kiwims_banner.bmp
WizardSmallImageFile=setup\kiwims_small.bmp
PrivilegesRequired=none
PrivilegesRequiredOverridesAllowed=commandline
WizardStyle=modern
SetupLogging=yes
CloseApplications=no
RestartApplications=no

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Files]
Source: "KiwiMS_App\KiwiMS.exe"; DestDir: "{app}";
Source: "KiwiMS_App\env_kiwims\*"; DestDir: "{app}\env_kiwims"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "KiwiMS_App\.Renviron"; DestDir: "{app}";
Source: "KiwiMS_App\.Rprofile"; DestDir: "{app}";
Source: "KiwiMS_App\.renvignore"; DestDir: "{app}";
Source: "KiwiMS_App\app.R"; DestDir: "{app}";
Source: "KiwiMS_App\config.yml"; DestDir: "{app}";
Source: "KiwiMS_App\renv.lock"; DestDir: "{app}";
Source: "KiwiMS_App\renv\*"; DestDir: "{app}\renv"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "KiwiMS_App\rhino.yml"; DestDir: "{app}";
Source: "KiwiMS_App\R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "KiwiMS_App\app\*"; DestDir: "{app}\app"; Excludes: "report"; Flags: ignoreversion recursesubdirs createallsubdirs;
Source: "KiwiMS_App\app\report\*"; DestDir: "{code:GetReportDir}"; Flags: ignoreversion recursesubdirs createallsubdirs uninsremovereadonly
Source: "KiwiMS_App\resources\*"; DestDir: "{app}\resources"; Flags: ignoreversion recursesubdirs createallsubdirs;
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "setup\VC_redist.x64.exe"; DestDir: "{tmp}"; Flags: deleteafterinstall
Source: "setup\config.ps1"; DestDir: "{tmp}"; Flags: deleteafterinstall
Source: "LICENSE"; DestDir: "{app}";

[InstallDelete]
; Inno overwrites files but never removes ones the new payload no longer contains, so an
; in-place upgrade merges two releases. That matters here: the conda environment changes
; between versions (plotly 5's 27,200-file validators tree, wx, pythonwin all vanished in
; one release), and a stale numpy or scipy module left behind stays importable alongside
; its replacement. conda-unpack cannot detect it either - every file in the new manifest
; is present, so it reports success on a merged environment. Clear these first.
;
; Runs before any file is copied, and only costs time on an upgrade - on a fresh install
; the directories do not exist. User data lives outside {app} and is untouched.
Type: filesandordirs; Name: "{app}\env_kiwims"
Type: filesandordirs; Name: "{app}\renv"
Type: filesandordirs; Name: "{app}\R-Portable"
Type: filesandordirs; Name: "{app}\app"
Type: filesandordirs; Name: "{app}\resources"

[UninstallDelete]
Type: filesandordirs; Name: "{app}"
Type: files;          Name: "{localappdata}\KiwiMS\*.log"
Type: files;          Name: "{localappdata}\KiwiMS\last_cluster_log.txt"
Type: dirifempty;     Name: "{localappdata}\KiwiMS"
Type: dirifempty;     Name: "{userdocs}\KiwiMS\report"
Type: dirifempty;     Name: "{userdocs}\KiwiMS"
Type: dirifempty;     Name: "{commondocs}\KiwiMS\report"
Type: dirifempty;     Name: "{commondocs}\KiwiMS"

[CustomMessages]
Description_Launch=Launch KiwiMS
RemoveUserData=Do you also want to remove your KiwiMS settings and log files?%n%nChoose No to keep them for a future installation.
de.RemoveUserData=Möchten Sie auch Ihre KiwiMS-Einstellungen und Protokolldateien entfernen?%n%nWählen Sie Nein, um sie für eine spätere Installation zu behalten.
ScopeTitle=Select Installation Type
ScopeSub=Who should this application be installed for?
ScopeDesc=Choose how you want to install KiwiMS.
ScopeAllUsers=System-wide for all users (requires admin)
ScopeCurrUser=Current user only
de.Description_Launch=KiwiMS starten
de.ScopeTitle=Installationstyp auswählen
de.ScopeSub=Für wen soll diese Anwendung installiert werden?
de.ScopeDesc=Wählen Sie aus, wie Sie KiwiMS installieren möchten.
de.ScopeAllUsers=Systemweit für alle Benutzer (erfordert Admin)
de.ScopeCurrUser=Nur für den aktuellen Benutzer

[Run]
Filename: "{app}\KiwiMS.exe"; Description: "{cm:Description_Launch}"; Flags: postinstall skipifsilent shellexec;

[Icons]
Name: "{group}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"
Name: "{userdesktop}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"

[Code]
const
  LegacyUninstallKey = 'Software\Microsoft\Windows\CurrentVersion\Uninstall\KiwiMS_is1';

var
  InstallScopePage: TInputOptionWizardPage;
  SelectedScope: string;
  InstallationFailed: Boolean;

procedure UpdateProgress(Position: Integer);
begin
  if (not WizardSilent) and (WizardForm <> nil) then
    WizardForm.ProgressGauge.Position := Position * WizardForm.ProgressGauge.Max div 100;
end;

procedure UpdateStatus(Msg: string);
begin
  if (not WizardSilent) and (WizardForm <> nil) then
    WizardForm.StatusLabel.Caption := Msg;
end;

function GetInstallScope(Param: string): string;
begin
  if WizardSilent then
  begin
    if IsAdminInstallMode then Result := 'allusers' else Result := 'currentuser';
  end
  else Result := SelectedScope;
end;

function GetReportDir(Param: string): string;
begin
  if GetInstallScope('') = 'allusers' then
    Result := ExpandConstant('{commondocs}\KiwiMS\report')
  else
    Result := ExpandConstant('{userdocs}\KiwiMS\report');
end;

function GetLegacyUninstaller(): string;
var
  UninstallString, InstallLocation: string;
begin
  Result := '';
  if not RegQueryStringValue(HKCU, LegacyUninstallKey, 'InstallLocation', InstallLocation) then
    Exit;
  if CompareText(RemoveBackslashUnlessRoot(RemoveQuotes(Trim(InstallLocation))),
                 ExpandConstant('{localappdata}\KiwiMS')) <> 0 then
    Exit;
  if RegQueryStringValue(HKCU, LegacyUninstallKey, 'UninstallString', UninstallString) then
    Result := RemoveQuotes(Trim(UninstallString));
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Uninstaller, LegacyDir: string;
  ResultCode, Waited: Integer;
begin
  Result := '';
  Uninstaller := GetLegacyUninstaller();
  if (Uninstaller = '') or (not FileExists(Uninstaller)) then Exit;

  UpdateStatus('Removing the previous KiwiMS installation ...');
  if not Exec(Uninstaller, '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART', '',
              SW_HIDE, ewWaitUntilTerminated, ResultCode) then Exit;

  Waited := 0;
  while (Waited < 600) and RegKeyExists(HKCU, LegacyUninstallKey) do
  begin
    Sleep(500);
    Waited := Waited + 1;
  end;

  LegacyDir := ExpandConstant('{localappdata}\KiwiMS');
  DelTree(LegacyDir + '\env_kiwims', True, True, True);
  DelTree(LegacyDir + '\R-Portable', True, True, True);
  DelTree(LegacyDir + '\renv', True, True, True);
  DelTree(LegacyDir + '\app', True, True, True);
  DelTree(LegacyDir + '\resources', True, True, True);
end;

procedure InitializeWizard;
begin
  SelectedScope := 'currentuser';
  InstallationFailed := False;
  if not WizardSilent then
  begin
    InstallScopePage := CreateInputOptionPage(wpWelcome, CustomMessage('ScopeTitle'), CustomMessage('ScopeSub'), CustomMessage('ScopeDesc'), True, False);
    InstallScopePage.Add(CustomMessage('ScopeAllUsers'));
    InstallScopePage.Add(CustomMessage('ScopeCurrUser'));
    InstallScopePage.Values[1] := True;
  end;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if (not WizardSilent) and (InstallScopePage <> nil) and (CurPageID = InstallScopePage.ID) then
  begin
    if InstallScopePage.Values[0] then
    begin
      SelectedScope := 'allusers';
      WizardForm.DirEdit.Text := ExpandConstant('{commonpf}\KiwiMS');
    end else
    begin
      WizardForm.DirEdit.Text := ExpandConstant('{localappdata}\Programs\KiwiMS');
      SelectedScope := 'currentuser';
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  ResultCode: Integer;
  LogFile, PsArgs, UnpackCmd, CompileCmd: string;
begin
  LogFile := ExpandConstant('{#KiwiMSLogFile}');

  if CurStep = ssPostInstall then
  begin
    // Step 1: Run config.ps1 — initialises the log file and user data directory
    UpdateStatus('Configuring KiwiMS...');
    UpdateProgress(80);
    PsArgs := '-NonInteractive -ExecutionPolicy Bypass -File "' + ExpandConstant('{tmp}\config.ps1') + '"'
            + ' -basePath "' + ExpandConstant('{app}') + '"'
            + ' -userDataPath "' + ExpandConstant('{localappdata}\KiwiMS') + '"'
            + ' -envName "kiwims"'
            + ' -logFile "' + LogFile + '"'
            + ' -installScope "' + SelectedScope + '"';
    Exec('powershell.exe', PsArgs, '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

    // Step 1.5: Install VC++ 2015-2022 Redistributable — required by Python and
    // conda-unpack.exe (VCRUNTIME140.dll). Exit code 1638 means already installed.
    UpdateStatus('Installing Visual C++ runtime...');
    UpdateProgress(82);
    Exec(ExpandConstant('{tmp}\VC_redist.x64.exe'),
         '/quiet /norestart', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
    SaveStringToFile(LogFile,
      '[INFO] VC++ redistributable exit code: ' + IntToStr(ResultCode) + #13#10, True);

    // Step 2: Run conda-unpack.exe via PowerShell so output is captured in the log
    UpdateStatus('Finalizing portable environment ...');
    UpdateProgress(90);
    UnpackCmd := '& ' + Chr(39) + ExpandConstant('{app}\env_kiwims\Scripts\conda-unpack.exe') + Chr(39)
               + ' *>&1 | Add-Content -Path ' + Chr(39) + LogFile + Chr(39)
               + '; exit $LASTEXITCODE';
    PsArgs := '-NonInteractive -ExecutionPolicy Bypass -Command ' + Chr(34) + UnpackCmd + Chr(34);

    if not Exec('powershell.exe', PsArgs, ExpandConstant('{app}'), SW_HIDE, ewWaitUntilTerminated, ResultCode)
       or (ResultCode <> 0) then
    begin
      SaveStringToFile(LogFile,
        '[ERROR] conda-unpack.exe failed with exit code: ' + IntToStr(ResultCode) + #13#10, True);
      MsgBox('Failed to finalize the environment. Please check write permissions.', mbError, MB_OK);
      InstallationFailed := True;
    end else
    begin
      SaveStringToFile(LogFile, '[OK] conda-unpack.exe completed successfully.' + #13#10, True);
    end;

    // Step 2.5: Pre-compile the Python environment to bytecode.
    //
    // Python validates a cached .pyc against the mtime of its .py, and that check
    // does not survive being installed. The installer writes destination timestamps
    // at a coarser resolution than the source tree carries, so roughly 4,500 of the
    // ~9,800 shipped .pyc files land one second off their source and are discarded
    // on first import; a further ~3,700 are already stale in the packed environment,
    // where conda stamped the .py files with the env-creation date while the .pyc
    // came from the upstream package build. About 90% of the environment therefore
    // recompiles from source, in every process that imports it. Measured on the
    // UniDec import chain (1,589 modules): 6.2 s recompiling versus 1.8 s cached,
    // paid by every worker in the pool, concurrently, on every run.
    //
    // --invalidation-mode unchecked-hash writes bytecode stamped with the source
    // hash rather than its mtime, and never revalidated. That makes it immune to
    // however a copy treats timestamps, and it matters most for an all-users
    // install: %ProgramFiles% is read-only for a normal user, so Python cannot
    // silently repair the cache on first run the way it can under %LOCALAPPDATA%,
    // and without this the full compile cost is paid on every run forever.
    //
    // Ordering is load-bearing. This must run AFTER conda-unpack: hash-based
    // bytecode is never rechecked, so a source file conda-unpack rewrites has to be
    // compiled afterwards or the rewritten prefix is silently ignored. Stale
    // bytecode cannot survive an upgrade either, because [InstallDelete] clears
    // env_kiwims before the new payload is written.
    //
    // -W ignore drops some sixty upstream SyntaxWarnings for invalid escape
    // sequences in UniDec, multiplierz and pyteomics; the modules they come from
    // compile correctly, and the warnings would otherwise fill the install log.
    // -x skips the three files no Python 3 can compile - contrib\netpubsub.py,
    // contrib\wx_monitor.py (both Python 2 relics inside pubsub) and
    // multiplierz\mzTools\chargeTransform.py - none of which is reachable from
    // `import unidec` or could ever be imported. With those excluded a clean run
    // exits 0, so a non-zero code in the log is a real signal rather than noise.
    // Either way the install continues: worst case the environment starts slower.
    UpdateStatus('Pre-compiling the Python environment ...');
    UpdateProgress(94);
    CompileCmd := '& ' + Chr(39) + ExpandConstant('{app}\env_kiwims\python.exe') + Chr(39)
                + ' -W ignore -m compileall -q -f -j 0 --invalidation-mode unchecked-hash'
                + ' -x ' + Chr(39) + '(netpubsub|wx_monitor|chargeTransform)\.py$' + Chr(39)
                + ' ' + Chr(39) + ExpandConstant('{app}\env_kiwims\Lib') + Chr(39)
                + ' *>&1 | Add-Content -Path ' + Chr(39) + LogFile + Chr(39)
                + '; exit $LASTEXITCODE';
    PsArgs := '-NonInteractive -ExecutionPolicy Bypass -Command ' + Chr(34) + CompileCmd + Chr(34);

    if not Exec('powershell.exe', PsArgs, ExpandConstant('{app}'), SW_HIDE, ewWaitUntilTerminated, ResultCode) then
      SaveStringToFile(LogFile,
        '[WARN] compileall could not be started; the first deconvolution will be slow.' + #13#10, True)
    else if ResultCode = 0 then
      SaveStringToFile(LogFile,
        '[OK] compileall regenerated the environment bytecode.' + #13#10, True)
    else
      SaveStringToFile(LogFile,
        '[WARN] compileall exited ' + IntToStr(ResultCode) +
        '; a module failed to compile and will be recompiled on every run.' + #13#10, True);

    // Remove conda-meta/ so reticulate does not detect env_kiwims as a conda environment
    UpdateStatus('Cleaning up environment metadata ...');
    DelTree(ExpandConstant('{app}\env_kiwims\conda-meta'), True, True, True);
    SaveStringToFile(LogFile, '[OK] conda-meta removed.' + #13#10, True);

    UpdateProgress(100);
    if InstallationFailed then Abort;
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  UserDataDir: string;
begin
  if CurUninstallStep <> usPostUninstall then Exit;

  UserDataDir := ExpandConstant('{localappdata}\KiwiMS');
  if not DirExists(UserDataDir) then Exit;

  if SuppressibleMsgBox(CustomMessage('RemoveUserData'), mbConfirmation,
                        MB_YESNO or MB_DEFBUTTON2, IDNO) = IDYES then
  begin
    DelTree(UserDataDir, True, True, True);
    // Log directory used by releases before logs moved under {localappdata}.
    DelTree(ExpandConstant('{userdocs}\KiwiMS\logs'), True, True, True);
    DelTree(ExpandConstant('{userdocs}\KiwiMS'), False, False, True);
  end;
end;