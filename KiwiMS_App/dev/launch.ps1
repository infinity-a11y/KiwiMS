#-----------------------------#
# Script Initialization
#-----------------------------#

# Get the directory where the .exe (or .ps1) is running.
# $PSScriptRoot is set correctly by both PowerShell (for .ps1) and ps2exe (for .exe).
# $MyInvocation.MyCommand.Path is null in compiled exes, so it cannot be used alone.
$appRoot = if ($PSScriptRoot) {
    $PSScriptRoot
} else {
    Split-Path -Parent ([System.Diagnostics.Process]::GetCurrentProcess().MainModule.FileName)
}
Set-Location $appRoot

# Get version info
$versionPath = Join-Path $appRoot "resources\version.txt"
$versionFile = if (Test-Path $versionPath) { Get-Content -Path $versionPath | Select-Object -First 1 } else { "0.5.1" }

# Headless check
$Headless = $args -contains "--headless"

Write-Host ""
Write-Host "██╗  ██╗ ██╗            ██╗    ███╗   ███╗  ██████╗ " -ForegroundColor DarkGreen
Write-Host "██║ ██╔╝ ╚═╝            ╚═╝    ████╗ ████║ ██╔════╝ " -ForegroundColor DarkGreen
Write-Host "█████╔╝  ██╗ ██╗    ██╗ ██╗    ██╔████╔██║ ╚█████╗  " -ForegroundColor DarkGreen
Write-Host "██╔═██╗  ██║ ██║ █╗ ██║ ██║    ██║╚██╔╝██║  ╚═══██╗ " -ForegroundColor DarkGreen
Write-Host "██║  ██╗ ██║ ╚███╔███╔╝ ██║    ██║ ╚═╝ ██║ ██████╔╝ " -ForegroundColor DarkGreen
Write-Host "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝    ╚═╝     ╚═╝ ╚═════╝  " -ForegroundColor DarkGreen
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
    $env:PYTHONHOME = Join-Path $appRoot "env_kiwims"
    $env:RETICULATE_PYTHON = Join-Path $appRoot "env_kiwims\python.exe"
    
    # Define the Shiny launch command
    $shinyCmd = "shiny::runApp('app.R', launch.browser = $(if ($Headless) { 'FALSE' } else { 'TRUE' }))"
    
    Write-Host "Starting application in browser..." -ForegroundColor Green

    # Execute Rscript directly
    & "$RPortablePath" --no-save --no-restore -e "$shinyCmd" *> $logFile 2>&1

    if ($LASTEXITCODE -ne 0) {
        throw "R Process exited with code $LASTEXITCODE"
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