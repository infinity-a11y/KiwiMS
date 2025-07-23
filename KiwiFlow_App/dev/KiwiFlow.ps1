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


# Conda find function
function Find-CondaExecutable {
    # 1. Check common default system-wide installation path (ProgramData)
    $defaultProgramDataPath = "$env:ProgramData\miniconda3\Scripts\conda.exe"
    if (Test-Path $defaultProgramDataPath) {
        Write-Host "Found conda.exe at default ProgramData path: $defaultProgramDataPath"
        return $defaultProgramDataPath
    }

    # 2. Check common default user-specific installation path (UserProfile)
    $defaultUserProfilePath = "$env:UserProfile\miniconda3\Scripts\conda.exe"
    if (Test-Path $defaultUserProfilePath) {
        Write-Host "Found conda.exe at default UserProfile path: $defaultUserProfilePath"
        return $defaultUserProfilePath
    }

    # 3. Check if conda.exe is in the system's PATH using Get-Command
    # This is often the most reliable if user installed Conda and added it to PATH.
    try {
        $condaCmdInPath = (Get-Command conda.exe -ErrorAction SilentlyContinue).Path
        if ($condaCmdInPath) {
            Write-Host "Found conda.exe in system PATH: $condaCmdInPath"
            return $condaCmdInPath
        }
    }
    catch {
        Write-Warning "Failed to find conda.exe in system PATH using Get-Command. Error: $($_.Exception.Message)"
    }

    # 4. Fallback for common Anaconda installation paths (if a user has Anaconda instead of Miniconda)
    $defaultProgramFilesAnacondaPath = "$env:ProgramFiles\Anaconda3\Scripts\conda.exe"
    if (Test-Path $defaultProgramFilesAnacondaPath) {
        Write-Host "Found conda.exe at default ProgramFiles Anaconda path: $defaultProgramFilesAnacondaPath"
        return $defaultProgramFilesAnacondaPath
    }

    $defaultUserProfileAnacondaPath = "$env:UserProfile\Anaconda3\Scripts\conda.exe"
    if (Test-Path $defaultUserProfileAnacondaPath) {
        Write-Host "Found conda.exe at default UserProfile Anaconda path: $defaultUserProfileAnacondaPath"
        return $defaultUserProfileAnacondaPath
    }

    Write-Host "ERROR: conda.exe not found in common locations or system PATH." -ForegroundColor Red
    return $null # Return null if conda.exe is not found anywhere
}

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