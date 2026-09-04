#-----------------------------#
# Script Initialization
#-----------------------------#

$appRoot = if ($PSScriptRoot) {
    $PSScriptRoot
}
else {
    Split-Path -Parent ([System.Diagnostics.Process]::GetCurrentProcess().MainModule.FileName)
}
Set-Location $appRoot

# Get version info from the single source of truth. Never hard-code a fallback
# version here - a stale literal is worse than admitting the file is missing.
$versionPath = Join-Path $appRoot "resources\version.txt"
$versionFile = "unknown"
if (Test-Path $versionPath) {
    $line = Get-Content -Path $versionPath | Where-Object { $_ -match '^\s*version\s*=' } | Select-Object -First 1
    if ($line) { $versionFile = ($line -replace '^\s*version\s*=', '').Trim() }
}

# Headless check
$Headless = $args -contains "--headless"

Write-Host ""
Write-Host "██╗  ██╗ ██╗            ██╗  ███╗   ███╗  ██████╗ " -ForegroundColor DarkGreen
Write-Host "██║ ██╔╝ ╚═╝            ╚═╝  ████╗ ████║ ██╔════╝ " -ForegroundColor DarkGreen
Write-Host "█████╔╝  ██╗ ██╗    ██╗ ██╗  ██╔████╔██║ ╚█████╗  " -ForegroundColor DarkGreen
Write-Host "██╔═██╗  ██║ ██║ █╗ ██║ ██║  ██║╚██╔╝██║  ╚═══██╗ " -ForegroundColor DarkGreen
Write-Host "██║  ██╗ ██║ ╚███╔███╔╝ ██║  ██║ ╚═╝ ██║ ██████╔╝ " -ForegroundColor DarkGreen
Write-Host "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝  ╚═╝     ╚═╝ ╚═════╝  " -ForegroundColor DarkGreen
Write-Host ""
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray
Write-Host "         Welcome to KiwiMS ($versionFile)          " -ForegroundColor White
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray

#-----------------------------#
# Path & Log Configuration
#-----------------------------#
$logDirectory = "$env:LOCALAPPDATA\KiwiMS"
$logFile = Join-Path $logDirectory "launch.log"
$urlFile = Join-Path $logDirectory "current_url.txt"
$volumeFile = Join-Path $logDirectory "volumes.txt"

if (-Not (Test-Path $logDirectory)) { New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null }

# launch.log is shared by every instance, so append and tag each line with the
# launcher PID rather than truncating it. It used to be overwritten on every
# start, which meant a second launch erased the record of the first.
function Write-LaunchLog([string] $Message) {
    "$(Get-Date) - [$PID] $Message" | Add-Content -Path $logFile -Encoding utf8
}

# Appending forever would grow without bound, so keep the tail.
if ((Test-Path $logFile) -and ((Get-Item $logFile).Length -gt 1MB)) {
    Get-Content $logFile -Tail 500 | Set-Content $logFile -Encoding utf8
}

Write-LaunchLog "INFO: Launcher initialized (portable), version $versionFile."

#-----------------------------#
# Single Instance Guard
#-----------------------------#
# Two launchers used to race each other: both truncated app_output.log, so the
# first could miss its own "Listening on" line and either time out after 120 s
# or open a browser pointed at the second instance. A named mutex makes the
# second launch hand the user back to the app that is already running.
#
# "Local\" rather than "Global\" on purpose: the scope we want is one app per
# interactive session, and creating a Global object requires
# SeCreateGlobalPrivilege, which a standard user does not hold.
#
# Headless launches are exempt - it is a manual debugging entry point, and
# blocking it would make it useless while an app is already up.
$instanceMutex = $null
if (-not $Headless) {
    $mutexCreated = $false
    $instanceMutex = New-Object System.Threading.Mutex($true, 'Local\KiwiMS_Launcher', [ref] $mutexCreated)

    if (-not $mutexCreated) {
        Write-LaunchLog "INFO: Refused to start - another instance already holds the lock."
        $runningUrl = if (Test-Path $urlFile) { (Get-Content $urlFile -TotalCount 1).Trim() } else { $null }

        # Deliberately not Start-Process-ing the URL here. That used to open a
        # second browser tab pointing at the same running instance - same port,
        # same session backend, so harmless, but two identical tabs look to a
        # user exactly like "two copies running", which is the confusion this
        # guard exists to prevent in the first place. Telling them where it
        # already is and letting them switch to it themselves reads as correct
        # instead of broken.
        Write-Host ""
        if ($runningUrl) {
            Write-Host "KiwiMS is already running - switch to its browser tab:" -ForegroundColor Yellow
            Write-Host "  $runningUrl" -ForegroundColor Gray
        }
        else {
            # No URL yet means the other instance is still starting up.
            Write-Host "KiwiMS is already starting up in another window." -ForegroundColor Yellow
            Write-Host "Give it a moment - the browser will open by itself." -ForegroundColor Yellow
        }

        $instanceMutex.Dispose()
        Start-Sleep -Seconds 4
        exit 0
    }
}

# Define Local Engine Paths
$RPortablePath = Join-Path $appRoot "R-Portable\bin\Rscript.exe"
$localPython = Join-Path $appRoot "env_kiwims\python.exe"

# Verification checks
if (-not (Test-Path $RPortablePath)) {
    $errorMsg = "ERROR: R-Portable not found at $RPortablePath"
    Write-LaunchLog $errorMsg
    Write-Host $errorMsg -ForegroundColor Red
    if (-not $Headless) { pause }
    exit 1
}

#-----------------------------#
# Environment Setup & Launch
#-----------------------------#
Write-Host "Initializing environment..." -ForegroundColor Yellow

try {
    Write-LaunchLog "INFO: Launching via R-Portable: $RPortablePath"

    # Set Critical Environment Variables to force isolation
    $env:R_HOME = Join-Path $appRoot "R-Portable"
    $env:PYTHONHOME = $null
    $env:RETICULATE_PYTHON = Join-Path $appRoot "env_kiwims\python.exe"
    # Prevent renv from auto-snapshotting on startup
    $env:RENV_CONFIG_AUTO_SNAPSHOT = "FALSE"
    # An installed KiwiMS has a frozen library, so renv's project consistency
    # check can only ever confirm what the installer just wrote. It costs ~3 s
    # of the launch by stat-ing renv.lock against every installed DESCRIPTION,
    # which is most of the wait before "Listening on" appears.
    $env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
    # Never build the system-library sandbox. It links (or, where links are not
    # permitted, copies) all of R-Portable\library into %LOCALAPPDATA% on the
    # first launch. The project .Renviron sets this too; repeated here so a
    # damaged install still starts quickly.
    $env:RENV_CONFIG_SANDBOX_ENABLED = "FALSE"

    # R's stdout/stderr, one pair of files per launcher. These used to be fixed
    # names truncated at every start, so a concurrent launch would erase the
    # output another instance was still polling for its "Listening on" line.
    $appLog = Join-Path $logDirectory "app_output_$PID.log"
    $appErrLog = Join-Path $logDirectory "app_error_$PID.log"
    "" | Set-Content $appLog -Encoding utf8
    "" | Set-Content $appErrLog -Encoding utf8

    # Bound the per-instance logs: keep the six newest files (this run's pair
    # plus the two before it) and drop the rest, so a user who launched twice
    # can still hand over the relevant pair without them piling up forever.
    Get-ChildItem -Path $logDirectory -Filter "app_*_*.log" -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending |
        Select-Object -Skip 6 |
        Remove-Item -Force -ErrorAction SilentlyContinue

    # Enumerate the drives the file pickers offer as roots, and hand the result
    # to R through a file. R used to do this itself at every session start by
    # shelling out to PowerShell for a WMI query - a cold subprocess plus a WMI
    # warm-up on the critical path, with no timeout and no fallback, so a
    # machine where the query was slow or blocked by policy started slowly or
    # silently ended up with no drives at all in the picker.
    #
    # [System.IO.DriveInfo]::GetDrives() wraps the Win32 GetLogicalDrives()
    # bitmask: it reads the mount table and touches no filesystem, so it cannot
    # block on a drive that is not there.
    #
    # IsReady is the opposite - it does touch the device. That is fine and cheap
    # for local media (it is how an empty card reader slot or optical drive is
    # detected), but it must never be called on a network drive: a disconnected
    # mapping makes it hang on an SMB reconnect for tens of seconds. Network
    # drives are therefore listed unprobed, and only fail when actually opened.
    try {
        $volumeLines = foreach ($drive in [System.IO.DriveInfo]::GetDrives()) {
            $type = $drive.DriveType.ToString()
            if ($type -eq 'NoRootDirectory') { continue }

            $include = if ($type -eq 'Network') { $true } else { $drive.IsReady }
            if (-not $include) { continue }

            # "C:\" -> "C:/", the form shinyFiles expects for a root.
            $path = $drive.Name -replace '\\$', '/'
            $label = $drive.Name.TrimEnd('\')
            "$label`t$path"
        }

        # ASCII, not utf8: drive names are always ASCII, and Set-Content -Encoding
        # utf8 on PowerShell 5.1 writes a BOM that R would read as part of the
        # first label, yielding a root literally named "<BOM>C:".
        $volumeLines | Set-Content -Path $volumeFile -Encoding ascii
        Write-LaunchLog "INFO: Volumes detected: $($volumeLines -join ' ')"
    }
    catch {
        # A stale file would be worse than none - R falls back to probing.
        Remove-Item $volumeFile -Force -ErrorAction SilentlyContinue
        Write-LaunchLog "WARN: Volume detection failed: $($_.Exception.Message)"
    }

    # Timed so launch.log records where a slow start went. The first launch
    # after an install is expected to be the slowest: Windows Defender scans
    # every R and Python binary the first time it is opened, and the install
    # tree is roughly 3.6 GB across 60,000 files.
    $startupTimer = [System.Diagnostics.Stopwatch]::StartNew()

    # Start R in a background process (non-blocking).
    $shinyCmd = "shiny::runApp('app.R', launch.browser = FALSE)"
    $rProcess = Start-Process -FilePath $RPortablePath `
        -ArgumentList "--no-save", "--no-restore", "-e", "`"$shinyCmd`"" `
        -WorkingDirectory $appRoot `
        -RedirectStandardOutput $appLog `
        -RedirectStandardError  $appErrLog `
        -NoNewWindow -PassThru

    Write-LaunchLog "INFO: R process started (PID $($rProcess.Id))"

    if (-not $Headless) {
        # Poll app_output.log for Shiny's "Listening on <url>" line, then open
        # the browser via Start-Process (uses the system default browser handler).
        Write-Host "Waiting for app to start..." -ForegroundColor Yellow
        $appUrl = $null
        $maxWait = 120  # seconds
        $elapsed = 0
        while ($elapsed -lt $maxWait -and $null -eq $appUrl) {
            Start-Sleep -Milliseconds 500
            $elapsed += 0.5
            if ($rProcess.HasExited) {
                throw "R process exited unexpectedly (code $($rProcess.ExitCode)). See: $appErrLog"
            }
            # Shiny prints "Listening on <url>" to stderr; check both logs.
            $lines = (Get-Content $appLog    -ErrorAction SilentlyContinue) +
            (Get-Content $appErrLog -ErrorAction SilentlyContinue)
            foreach ($line in $lines) {
                if ($line -match "Listening on (http://\S+)") {
                    $appUrl = $Matches[1]; break
                }
            }
        }

        if ($appUrl) {
            Write-LaunchLog "INFO: App listening at $appUrl after $([math]::Round($startupTimer.Elapsed.TotalSeconds, 1)) s"

            # Record the live URL so a second launch can reopen this instance
            # instead of starting a rival one. Removed again in the finally
            # block below, so a stale URL cannot outlive the process.
            Set-Content -Path $urlFile -Value $appUrl -Encoding utf8

            Write-Host "Opening browser: $appUrl" -ForegroundColor Green
            Start-Process $appUrl

            # Warm the Python side while the user is still choosing files. The
            # deconvolution worker pool is the first thing to touch python.exe,
            # and on a fresh machine it pays for all of it at once: Defender's
            # first scan of the environment, matplotlib's font cache under
            # %USERPROFILE%\.matplotlib, and multiplierz's one-off
            # %USERPROFILE%\.multiplierz set-up. Both caches are written at import
            # time with no locking, so every worker starting at the same moment
            # races the others for them.
            #
            # Started after the browser opens, and detached, on purpose: run any
            # earlier it would compete with R for disk while Defender is still
            # scanning R's own DLLs, which is exactly the wait the timer above
            # measures.
            #
            # python.exe needs the conda DLL directories on PATH to import the
            # compiled extensions; without them the import dies with 0xC06D007E,
            # "the specified module could not be found". The workers need no such
            # thing, because reticulate loads Python in-process and manages the
            # DLL search itself. The arguments are passed as one pre-quoted string
            # because Start-Process joins ArgumentList entries with spaces without
            # quoting them, which would split "import unidec" in two.
            if (Test-Path $localPython) {
                try {
                    $envRoot = Split-Path -Parent $localPython
                    $originalPath = $env:PATH
                    $env:PATH = "$envRoot;$envRoot\Library\bin;$originalPath"
                    $env:PYTHONNOUSERSITE = "1"
                    Start-Process -FilePath $localPython `
                        -ArgumentList '-W ignore -c "import unidec"' `
                        -WindowStyle Hidden | Out-Null
                    $env:PATH = $originalPath
                    Write-LaunchLog "INFO: Python pre-warm started."
                }
                catch {
                    Write-LaunchLog "WARN: Python pre-warm not started: $($_.Exception.Message)"
                }
            }
        }
        else {
            Write-LaunchLog "WARN: App URL not detected within $maxWait s. Check: $appLog"
            Write-Host "App may still be starting. Check $appLog" -ForegroundColor Yellow
        }
    }

    Write-Host "App is running. Close this window to stop the application." -ForegroundColor Green
    $rProcess.WaitForExit()
    # Exit code 0  → clean shutdown (e.g. user closed the browser tab).
    if ($rProcess.ExitCode -ne 0 -and $null -eq $appUrl) {
        throw "R process exited with code $($rProcess.ExitCode). See: $appErrLog"
    }
}
catch {
    Write-LaunchLog "CRITICAL ERROR: $($_.Exception.Message)"
    Write-Host ""
    Write-Host "FAILED TO START" -ForegroundColor Red
    Write-Host "Error: $($_.Exception.Message)"
    Write-Host "Detailed logs: $logFile"
    if (-not $Headless) { pause }
    exit 1
}
finally {
    # Both of these belong to the lock holder only. A headless run never writes
    # the URL file, so it must not delete the interactive instance's copy.
    if ($null -ne $instanceMutex) {
        # The URL is only meaningful while this process is alive. Leaving it
        # behind would send the next launch to a dead port.
        Remove-Item $urlFile -Force -ErrorAction SilentlyContinue

        # Windows releases an abandoned mutex on its own when a process dies, so
        # this only matters for a clean exit - but releasing it before the handle
        # is collected keeps the next launch from waiting on nothing.
        try { $instanceMutex.ReleaseMutex() } catch {}
        $instanceMutex.Dispose()
    }
}
