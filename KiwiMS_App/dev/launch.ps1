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

# Get version info
$versionPath = Join-Path $appRoot "resources\version.txt"
$versionFile = if (Test-Path $versionPath) { Get-Content -Path $versionPath | Select-Object -First 1 } else { "0.7.0" }

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

if (-Not (Test-Path $logDirectory)) { New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null }

# Clear previous logs
"$(Get-Date) - INFO: Launcher Initialized (Portable)." | Out-File $logFile

# Define Local Engine Paths
$RPortablePath = Join-Path $appRoot "R-Portable\bin\Rscript.exe"
$localPython = Join-Path $appRoot "env_kiwims\python.exe"

# Verification checks
if (-not (Test-Path $RPortablePath)) {
    $errorMsg = "ERROR: R-Portable not found at $RPortablePath"
    $errorMsg | Add-Content $logFile
    Write-Host $errorMsg -ForegroundColor Red
    if (-not $Headless) { pause }
    exit 1
}

#-----------------------------#
# Environment Setup & Launch
#-----------------------------#
Write-Host "Initializing environment..." -ForegroundColor Yellow

try {
    "$(Get-Date) - INFO: Launching via R-Portable: $RPortablePath" | Add-Content $logFile

    # Set Critical Environment Variables to force isolation
    $env:R_HOME = Join-Path $appRoot "R-Portable"
    $env:PYTHONHOME = $null
    $env:RETICULATE_PYTHON = Join-Path $appRoot "env_kiwims\python.exe"
    # Prevent renv from auto-snapshotting on startup
    $env:RENV_CONFIG_AUTO_SNAPSHOT = "FALSE"

    # Separate log for R stdout/stderr so the launcher log is not truncated.
    $appLog = Join-Path $logDirectory "app_output.log"
    $appErrLog = Join-Path $logDirectory "app_error.log"
    "" | Set-Content $appLog
    "" | Set-Content $appErrLog

    # Start R in a background process (non-blocking).
    $shinyCmd = "shiny::runApp('app.R', launch.browser = FALSE)"
    $rProcess = Start-Process -FilePath $RPortablePath `
        -ArgumentList "--no-save", "--no-restore", "-e", "`"$shinyCmd`"" `
        -WorkingDirectory $appRoot `
        -RedirectStandardOutput $appLog `
        -RedirectStandardError  $appErrLog `
        -NoNewWindow -PassThru

    "$(Get-Date) - INFO: R process started (PID $($rProcess.Id))" | Add-Content $logFile

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
            "$(Get-Date) - INFO: App listening at $appUrl" | Add-Content $logFile
            Write-Host "Opening browser: $appUrl" -ForegroundColor Green
            Start-Process $appUrl
        }
        else {
            "$(Get-Date) - WARN: App URL not detected within $maxWait s. Check: $appLog" | Add-Content $logFile
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
    $msg = "$(Get-Date) - CRITICAL ERROR: $($_.Exception.Message)"
    $msg | Add-Content $logFile
    Write-Host ""
    Write-Host "FAILED TO START" -ForegroundColor Red
    Write-Host "Error: $($_.Exception.Message)"
    Write-Host "Detailed logs: $logFile"
    if (-not $Headless) { pause }
    exit 1
}