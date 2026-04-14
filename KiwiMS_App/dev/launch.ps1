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
# Port Selection
#-----------------------------#
# Port selection is handled by Shiny automatically (httpuv::randomPort).

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

Write-Host "Starting application in default browser..." -ForegroundColor Yellow

try {
    # Extract the base directory to find the 'kiwims' environment
    # Moving up from Scripts/conda.exe to the root prefix
    $condaPrefix = Split-Path (Split-Path $condaCmd -Parent) -Parent

    "$(Get-Date) - INFO: Conda Command: $condaCmd" | Add-Content $logFile
    "$(Get-Date) - INFO: Conda Prefix: $condaPrefix" | Add-Content $logFile

    # Launch the App
    # Use --no-capture-output to ensure logs flow into our file correctly
    $shinyCmd = "shiny::runApp('app.R', launch.browser = $(if ($Headless) { 'FALSE' } else { 'TRUE' }))"
    & $condaCmd run -n kiwims Rscript.exe -e "$shinyCmd" --vanilla *> $logFile 2>&1

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