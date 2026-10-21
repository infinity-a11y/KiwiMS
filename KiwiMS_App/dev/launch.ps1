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
# Number of browser pages connected to the running app, written by the app
# (main.R) to the path it is handed in KIWIMS_SESSION_FILE
$sessionFile = Join-Path $logDirectory "active_sessions.txt"

if (-Not (Test-Path $logDirectory)) { New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null }

# launch.log is shared by every instance, so append and tag each line with the
# launcher PID rather than truncating it. It used to be overwritten on every
# start, which meant a second launch erased the record of the first. Once this
# start has its session folder (below), its lines go to launcher.log there too.
$sessionLauncherLog = $null
function Write-LaunchLog([string] $Message) {
    $line = "$(Get-Date) - [$PID] $Message"
    $line | Add-Content -Path $logFile -Encoding utf8
    if ($sessionLauncherLog) {
        $line | Add-Content -Path $sessionLauncherLog -Encoding utf8 -ErrorAction SilentlyContinue
    }
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
        # The app keeps running when the browser drops its connection (a tab
        # put to sleep, standby), so it can be up with no page open. Then
        # there is no tab to switch to, and opening one is what the user
        # wants - the exception to the rule above.
        $openPages = $null
        if (Test-Path $sessionFile) {
            $openPages = (Get-Content $sessionFile -TotalCount 1 -ErrorAction SilentlyContinue)
            if ($openPages) { $openPages = $openPages.Trim() }
        }

        Write-Host ""
        if ($runningUrl -and $openPages -eq "0") {
            Write-LaunchLog "INFO: Instance running without an open page - opening $runningUrl"
            Write-Host "KiwiMS is already running - opening it in your browser:" -ForegroundColor Yellow
            Write-Host "  $runningUrl" -ForegroundColor Gray
            Start-Process $runningUrl
        }
        elseif ($runningUrl) {
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
    # Only the lock holder's app reports its open pages; a headless run must
    # not overwrite the count of the interactive instance
    if (-not $Headless) {
        Remove-Item $sessionFile -Force -ErrorAction SilentlyContinue
        $env:KIWIMS_SESSION_FILE = $sessionFile
    }

    # Every start logs into a session folder of its own (see app/logic/
    # logging.R): <log folder>\<date>\KiwiMS_<date>_id<n>, holding the session
    # log the app writes, R's console output, this start's launcher lines and
    # the output of its deconvolution runs. The launcher makes it, as it opens
    # the console logs before R starts. The log folder is fixed, in the user's
    # profile where KiwiMS can always write (it used to be a setting).
    $logRoot = Join-Path $logDirectory "logs"
    $today = Get-Date -Format "yyyy-MM-dd"
    do {
        $sessionId = Get-Random -Minimum 1000 -Maximum 10000
        $sessionName = "KiwiMS_${today}_id$sessionId"
        $sessionDir = Join-Path (Join-Path $logRoot $today) $sessionName
    } while ((Test-Path $sessionDir) -or (Test-Path "$sessionDir.log"))
    New-Item -ItemType Directory -Force -Path $sessionDir | Out-Null
    $sessionLauncherLog = Join-Path $sessionDir "launcher.log"
    $env:KIWIMS_SESSION_DIR = $sessionDir
    Write-LaunchLog "INFO: Session folder $sessionDir"

    # R's console, in the session folder. Fixed names truncated at every start
    # used to let a concurrent launch erase the output another instance was
    # still polling for its "Listening on" line. R sends its messages,
    # warnings and errors to its output (see $shinyCmd), so console.log has
    # all of it in the order it happened; console_stderr.log gets only what
    # bypasses R's console (native code, a hard crash) and stays empty
    # otherwise.
    $appLog = Join-Path $sessionDir "console.log"
    $appErrLog = Join-Path $sessionDir "console_stderr.log"
    "" | Set-Content $appLog -Encoding utf8
    "" | Set-Content $appErrLog -Encoding utf8
    $env:KIWIMS_APP_LOG = $appLog
    $env:KIWIMS_APP_ERR_LOG = $appErrLog

    # The console logs grow with every deconvolution (R reports its progress
    # there), so only the ten newest sessions keep theirs; the session logs
    # themselves stay
    Get-ChildItem -Path $logRoot -Directory -ErrorAction SilentlyContinue |
        Get-ChildItem -Directory -Filter "KiwiMS_*_id*" -ErrorAction SilentlyContinue |
        Sort-Object CreationTime -Descending |
        Select-Object -Skip 10 |
        ForEach-Object {
            Get-ChildItem -Path $_.FullName -Filter "console*.log" -ErrorAction SilentlyContinue |
                Remove-Item -Force -ErrorAction SilentlyContinue
        }
    # Console logs of the earlier layout (one pair per launcher PID in this
    # folder, where nothing writes them any more) and its deconvolution logs:
    # kept two weeks after an update, in case they are asked for, then dropped
    Get-ChildItem -Path $logDirectory -File -ErrorAction SilentlyContinue |
        Where-Object {
            $_.Name -match '^(app_(output|error)_\d+\.log|deconvolution\.log|last_cluster_log\.txt)$' -and
            $_.LastWriteTime -lt (Get-Date).AddDays(-14)
        } |
        Remove-Item -Force -ErrorAction SilentlyContinue

    # Timed so launch.log records where a slow start went. The first launch
    # after an install is expected to be the slowest: Windows Defender scans
    # every R and Python binary the first time it is opened, and the install
    # tree is roughly 3.6 GB across 60,000 files.
    $startupTimer = [System.Diagnostics.Stopwatch]::StartNew()

    # R's own messages, warnings and errors in English, whatever the language
    # of Windows: the logs read the same on every machine, and the Help
    # dialog recognises an error in them (app/logic/support_bundle.R).
    $env:LANGUAGE = "en"

    # Start R in a background process (non-blocking). R's messages go to its
    # output from the first line on, package start-up messages included.
    $shinyCmd = "sink(stdout(), type = 'message'); shiny::runApp('app.R', launch.browser = FALSE)"
    $rProcess = Start-Process -FilePath $RPortablePath `
        -ArgumentList "--no-save", "--no-restore", "-e", "`"$shinyCmd`"" `
        -WorkingDirectory $appRoot `
        -RedirectStandardOutput $appLog `
        -RedirectStandardError  $appErrLog `
        -NoNewWindow -PassThru

    Write-LaunchLog "INFO: R process started (PID $($rProcess.Id))"

    if (-not $Headless) {
        # Poll console.log for Shiny's "Listening on <url>" line, then open
        # the browser via Start-Process (uses the system default browser handler).
        Write-Host "Waiting for app to start..." -ForegroundColor Yellow
        $appUrl = $null
        # A normal start takes ~10 s, but the first one after an install was
        # measured at ~80 s on a fresh VM; 120 s left too little headroom for a
        # slower machine, which then never got its browser tab.
        $maxWait = 300  # seconds
        $elapsed = 0
        $hintShown = $false
        while ($elapsed -lt $maxWait -and $null -eq $appUrl) {
            Start-Sleep -Milliseconds 500
            $elapsed += 0.5
            # Nothing else moves on screen during a slow start, so say why
            # before it starts to look hung.
            if (-not $hintShown -and $elapsed -ge 15) {
                Write-Host "The first start after installing or updating KiwiMS can take a minute or two" -ForegroundColor DarkGray
                Write-Host "while Windows checks the program files. Later starts are much faster." -ForegroundColor DarkGray
                $hintShown = $true
            }
            if ($rProcess.HasExited) {
                throw "R process exited unexpectedly (code $($rProcess.ExitCode)). See: $appLog"
            }
            # Shiny prints "Listening on <url>" as a message, so to console.log;
            # both logs are checked, in case R's messages did not get there.
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
    # Exit code 0  → clean shutdown (e.g. user closed the browser tab; a
    # connection the browser dropped on its own leaves the app running).
    if ($rProcess.ExitCode -ne 0 -and $null -eq $appUrl) {
        throw "R process exited with code $($rProcess.ExitCode). See: $appLog"
    }
}
catch {
    Write-LaunchLog "CRITICAL ERROR: $($_.Exception.Message)"
    Write-Host ""
    Write-Host "FAILED TO START" -ForegroundColor Red
    Write-Host "Error: $($_.Exception.Message)"
    Write-Host "Detailed logs: $logFile"

    # The app's Support Package needs the app; without it the launcher packs
    # the logs itself, so a failed start can be reported in one file
    try {
        $supportZip = Join-Path $logDirectory ("KiwiMS_support_" + (Get-Date -Format "yyyy-MM-dd_HHmm") + ".zip")
        # This start's session folder (launcher.log, console logs, the session
        # log if R got that far), or the shared launch log alone when the
        # start failed before the folder was made
        $supportFiles = @()
        if ($sessionDir -and (Test-Path $sessionDir)) {
            $supportFiles += @(Get-ChildItem -Path $sessionDir -File -ErrorAction SilentlyContinue)
        }
        if (Test-Path $logFile) {
            $supportFiles += @(Get-Item $logFile)
        }
        if ($supportFiles.Count -gt 0) {
            Compress-Archive -Path $supportFiles.FullName -DestinationPath $supportZip -Force
            Write-LaunchLog "INFO: Support package written: $supportZip"
            Write-Host ""
            Write-Host "Please send this file along with your report:" -ForegroundColor Yellow
            Write-Host "  $supportZip" -ForegroundColor Gray
        }
    }
    catch {
        Write-LaunchLog "WARN: Support package not written: $($_.Exception.Message)"
    }
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
        Remove-Item $sessionFile -Force -ErrorAction SilentlyContinue

        # Windows releases an abandoned mutex on its own when a process dies, so
        # this only matters for a clean exit - but releasing it before the handle
        # is collected keeps the next launch from waiting on nothing.
        try { $instanceMutex.ReleaseMutex() } catch {}
        $instanceMutex.Dispose()
    }
}
