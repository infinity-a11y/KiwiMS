# KiwiMS.ps1 - Launcher
#-----------------------------#
# Script Initialization
#-----------------------------#

# Get version info
$versionFile = if (Test-Path "resources\version.txt") { Get-Content -Path "resources\version.txt" | Select-Object -First 1 }

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
# Conda Discovery Function
#-----------------------------#
function Find-CondaExecutable {
    $searchPaths = @(
        # Miniconda - System Wide
        "$env:ProgramData\miniconda3\Scripts\conda.exe",
        "$env:ProgramData\miniconda3\Library\bin\conda.exe",
        "$env:ProgramData\miniconda3\condabin\conda.bat",

        # Miniconda - User Specific
        "$env:LOCALAPPDATA\miniconda3\Scripts\conda.exe",
        "$env:LOCALAPPDATA\miniconda3\Library\bin\conda.exe",
        "$env:LOCALAPPDATA\miniconda3\condabin\conda.bat"
    )

    # Search through hardcoded common paths
    foreach ($path in $searchPaths) {
        if (Test-Path $path) {
            Write-Host "Found conda at: $path" -ForegroundColor Cyan
            return $path
        }
    }

    # Check if conda.exe is in the system PATH
    $condaInPath = Get-Command conda.exe, conda.bat -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($condaInPath) {
        Write-Host "Found conda in system PATH: $($condaInPath.Path)" -ForegroundColor Cyan
        return $condaInPath.Path
    }

    # Check Environment Variables
    if ($env:CONDA_EXE -and (Test-Path $env:CONDA_EXE)) {
        Write-Host "Found conda via CONDA_EXE: $env:CONDA_EXE" -ForegroundColor Cyan
        return $env:CONDA_EXE
    }

    Write-Host "ERROR: conda.exe not found." -ForegroundColor Red
    return $null
}

#-----------------------------#
# Path & Log Configuration
#-----------------------------#
$condaCmd = Find-CondaExecutable
$logDirectory = "$env:LOCALAPPDATA\KiwiMS"
$logFile = Join-Path $logDirectory "launch.log"

if (-Not (Test-Path $logDirectory)) { New-Item -ItemType Directory -Path $logDirectory -Force | Out-Null }

# Clear previous logs
"$(Get-Date) - INFO: Launcher Initialized." | Out-File $logFile

if (-not $condaCmd) {
    Write-Host "ERROR: Conda not found! Please reinstall KiwiMS." -ForegroundColor Red
    "$(Get-Date) - ERROR: Conda executable not found in system or user paths." | Add-Content $logFile
    if (-not $Headless) { pause }
    exit 1
}

# Define the path to R executables
$RPortablePath = ".\R-Portable\bin\Rscript.exe"

# Verification check to see if the files are actually there
if (-not (Test-Path $RPortablePath)) {
    "$(Get-Date) - ERROR: R-Portable not found at $RPortablePath" | Add-Content $logFile
    Write-Host "ERROR: R-Portable not found!" -ForegroundColor Red
    if (-not $Headless) { pause }
    exit 1
}

Write-Host "Starting application in default browser..." -ForegroundColor Yellow
# --- Launch the App ---
try {
    "$(Get-Date) - INFO: Launching via R-Portable: $RPortablePath" | Add-Content $logFile

    $shinyCmd = "shiny::runApp('app.R', launch.browser = $(if ($Headless) { 'FALSE' } else { 'TRUE' }))"
    
    & $condaCmd run -n kiwims "$RPortablePath" --no-save --no-restore -e "$shinyCmd" *> $logFile 2>&1

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