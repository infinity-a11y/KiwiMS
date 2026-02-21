# KiwiMS.ps1 - Launcher
#-----------------------------#
# Script Initialization
#-----------------------------#

# Get version info
$versionFile = if (Test-Path "resources\version.txt") { Get-Content -Path "resources\version.txt" | Select-Object -First 1 }

# Headless check
$Headless = $args -contains "--headless"

Write-Output ""
Write-Output "██╗  ██╗ ██╗            ██╗    ███╗   ███╗  ██████╗ " -ForegroundColor DarkGreen
Write-Output "██║ ██╔╝ ╚═╝            ╚═╝    ████╗ ████║ ██╔════╝ " -ForegroundColor DarkGreen
Write-Output "█████╔╝  ██╗ ██╗    ██╗ ██╗    ██╔████╔██║ ╚█████╗  " -ForegroundColor DarkGreen
Write-Output "██╔═██╗  ██║ ██║ █╗ ██║ ██║    ██║╚██╔╝██║  ╚═══██╗ " -ForegroundColor DarkGreen
Write-Output "██║  ██╗ ██║ ╚███╔███╔╝ ██║    ██║ ╚═╝ ██║ ██████╔╝ " -ForegroundColor DarkGreen
Write-Output "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝    ╚═╝     ╚═╝ ╚═════╝  " -ForegroundColor DarkGreen
Write-Output ""
Write-Output "---------------------------------------------------" -ForegroundColor DarkGray
Write-Output "         Welcome to KiwiMS ($versionFile)          " -ForegroundColor White
Write-Output "---------------------------------------------------" -ForegroundColor DarkGray

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
            Write-Output "Found conda at: $path" -ForegroundColor Cyan
            return $path
        }
    }

    # Check if conda.exe is in the system PATH
    $condaInPath = Get-Command conda.exe, conda.bat -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($condaInPath) {
        Write-Output "Found conda in system PATH: $($condaInPath.Path)" -ForegroundColor Cyan
        return $condaInPath.Path
    }

    # Check Environment Variables
    if ($env:CONDA_EXE -and (Test-Path $env:CONDA_EXE)) {
        Write-Output "Found conda via CONDA_EXE: $env:CONDA_EXE" -ForegroundColor Cyan
        return $env:CONDA_EXE
    }

    Write-Output "ERROR: conda.exe not found." -ForegroundColor Red
    return $null
}

#-----------------------------#
# Port Check
#-----------------------------#
$port = 3838
Write-Output "Checking if port $port is available..." -ForegroundColor Yellow

$portProcess = Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue

if ($portProcess) {
    $pidToKill = $portProcess.OwningProcess
    Write-Output "Port $port is occupied by PID $pidToKill. Clearing it now..." -ForegroundColor Yellow
    try {
        Stop-Process -Id $pidToKill -Force -ErrorAction Stop
        Write-Output "Existing process terminated." -ForegroundColor Gray
        Start-Sleep -Seconds 2 # Time to release the socket
    } catch {
        Write-Output "Warning: Could not stop process $pidToKill." -ForegroundColor Red
    }
} else {
    Write-Output "Port $port is free." -ForegroundColor Gray
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
    Write-Output "ERROR: Conda not found! Please reinstall KiwiMS." -ForegroundColor Red
    "$(Get-Date) - ERROR: Conda executable not found in system or user paths." | Add-Content $logFile
    if (-not $Headless) { pause }
    exit 1
}

Write-Output "Starting application in default browser..." -ForegroundColor Yellow

try {
    # Extract the base directory to find the 'kiwims' environment
    # Moving up from Scripts/conda.exe to the root prefix
    $condaPrefix = Split-Path (Split-Path $condaCmd -Parent) -Parent

    "$(Get-Date) - INFO: Conda Command: $condaCmd" | Add-Content $logFile
    "$(Get-Date) - INFO: Conda Prefix: $condaPrefix" | Add-Content $logFile

    # Launch the App
    # Use --no-capture-output to ensure logs flow into our file correctly
    $shinyCmd = "shiny::runApp('app.R', port = 3838, launch.browser = $(if ($Headless) { 'FALSE' } else { 'TRUE' }))"
    & $condaCmd run -n kiwims Rscript.exe -e "$shinyCmd" --vanilla *> $logFile 2>&1

    if ($LASTEXITCODE -ne 0) {
        throw "R Process exited with code $LASTEXITCODE"
    }
}
catch {
    $msg = "$(Get-Date) - CRITICAL ERROR: $($_.Exception.Message)"
    $msg | Add-Content $logFile
    Write-Output ""
    Write-Output "FAILED TO START" -ForegroundColor Red
    Write-Output "Error: $($_.Exception.Message)"
    Write-Output "Detailed logs: $logFile"
    if (-not $Headless) { pause }
    exit 1
}