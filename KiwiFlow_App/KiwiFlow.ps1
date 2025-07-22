# KiwiFlow.ps1

# Run to launch KiwiFlow App

#-----------------------------#
# Script Initialization
#-----------------------------#

$versionFile = Get-Content -Path "resources\version.txt" | Select-Object -First 1

Write-Host ""
Write-Host "██╗  ██╗ ██╗ ██╗    ██╗ ██╗ ███████╗ ██╗      ██████╗  ██╗    ██╗" -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host "██║ ██╔╝ ██║ ██║    ██║ ██║ ██╔════╝ ██║     ██║   ██╗ ██║    ██║" -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host "█████╔╝  ██║ ██║ █╗ ██║ ██║ █████╗   ██║     ██║   ██║ ██║ █╗ ██║" -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host "██╔═██╗  ██║ ██║███╗██║ ██║ ██╔══╝   ██║     ██║   ██║ ██║███╗██║" -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host "██║  ██╗ ██║ ╚███╔███╔╝ ██║ ██║      ██████╗  ██████╔╝ ╚███╔███╔╝" -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝ ╚═╝      ╚═════╝  ╚═════╝   ╚══╝╚══╝ " -ForegroundColor DarkGreen -BackgroundColor Black
Write-Host ""
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray  -BackgroundColor Black
Write-Host "       Welcome to KiwiFlow!         " -ForegroundColor White -BackgroundColor Black
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray  -BackgroundColor Black
Write-Host ""  -BackgroundColor Black
Write-Host "$versionFile"
Write-Host "Starting application... please wait." -ForegroundColor Yellow  -BackgroundColor Black
Write-Host ""  -BackgroundColor Black


# Source functions
. "$basePath\functions.ps1"

# Path declaration
$condaCmd = Find-CondaExecutable
$condaPrefix = [System.IO.Path]::GetDirectoryName([System.IO.Path]::GetDirectoryName($condaCmd))
$logDirectory = "$env:localappdata\KiwiFlow"
$logFile = Join-Path $logDirectory "launch.log"

# Create the log directory if it doesn't exist
if (-Not (Test-Path $logDirectory)) {
    New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null
}

Clear-Content -Path $logFile -ErrorAction SilentlyContinue

# Conda presence check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found. Exiting."
    Write-Host "Check error logfile at: $logFile"
    pause
    exit 1
}

try {
    # Log script start time
    "$(Get-Date) - INFO: KiwiFlow App Launcher Started." | Add-Content -Path $logFile

    # Log Conda and App paths
    "$(Get-Date) - INFO: Conda Prefix: $condaPrefix" | Add-Content -Path $logFile
    "$(Get-Date) - INFO: Conda Command: $condaCmd" | Add-Content -Path $logFile
    "$(Get-Date) - INFO: Application Root: $($PSScriptRoot)" | Add-Content -Path $logFile

    # Execute shiny app launch R script
    & $condaCmd run -n kiwiflow Rscript.exe -e "shiny::runApp('app.R', port = 3838, launch.browser = TRUE)" --vanilla *> $logFile 2>&1

    # Check exit code
    if ($LASTEXITCODE -ne 0) {
        "$(Get-Date) - ERROR: R Shiny app launch failed with exit code $LASTEXITCODE." | Add-Content -Path $logFile
        "$(Get-Date) - ERROR: Review '$logFile' for details on the error (e.g., 'address already in use')." | Add-Content -Path $logFile
        Write-Host "Check error logfile at: $logFile"
        pause
        exit $LASTEXITCODE
    }
    else {
        "$(Get-Date) - INFO: R Shiny app launched successfully." | Add-Content -Path $logFile
    }

}
catch {
    # Catch any unexpected errors during the script execution
    "$(Get-Date) - CRITICAL ERROR: An unexpected PowerShell error occurred: $($_.Exception.Message)" | Add-Content -Path $logFile
    "$(Get-Date) - CRITICAL ERROR: Stack Trace: $($_.ScriptStackTrace)" | Add-Content -Path $logFile
    Write-Host "Check error logfile at: $logFile"
    pause
    exit 1
}