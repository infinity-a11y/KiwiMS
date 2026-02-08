# KiwiMS.ps1 - Launcher
#-----------------------------#
# Script Initialization
#-----------------------------#

# Get version info
$versionFile = if (Test-Path "resources\version.txt") { Get-Content -Path "resources\version.txt" | Select-Object -First 1 } else { "v1.0.0" }

Write-Host ""
Write-Host "██╗  ██╗ ██╗ ██╗    ██╗ ██╗ ███████╗ ██╗      ██████╗  ██╗    ██╗" -ForegroundColor DarkGreen
Write-Host "██║ ██╔╝ ██║ ██║    ██║ ██║ ██╔════╝ ██║     ██║   ██╗ ██║    ██║" -ForegroundColor DarkGreen
Write-Host "█████╔╝  ██║ ██║ █╗ ██║ ██║ █████╗   ██║     ██║   ██║ ██║ █╗ ██║" -ForegroundColor DarkGreen
Write-Host "██╔═██╗  ██║ ██║███╗██║ ██║ ██╔══╝   ██║     ██║   ██║ ██║███╗██║" -ForegroundColor DarkGreen
Write-Host "██║  ██╗ ██║ ╚███╔███╔╝ ██║ ██║      ██████╗  ██████╔╝ ╚███╔███╔╝" -ForegroundColor DarkGreen
Write-Host "╚═╝  ╚═╝ ╚═╝  ╚══╝╚══╝  ╚═╝ ╚═╝      ╚═════╝  ╚═════╝   ╚══╝╚══╝ " -ForegroundColor DarkGreen
Write-Host ""
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray
Write-Host "        Welcome to KiwiMS ($versionFile)         " -ForegroundColor White
Write-Host "---------------------------------------------------" -ForegroundColor DarkGray

#-----------------------------#
# Conda Discovery Function
#-----------------------------#
function Find-CondaExecutable {
    # 1. System-wide path (All Users)
    $allUsersPath = "$env:ProgramData\miniconda3\Scripts\conda.exe"
    if (Test-Path $allUsersPath) { return $allUsersPath }

    # 2. User-specific path (Current User - matching your installer logic)
    $currentUserPath = "$env:LOCALAPPDATA\miniconda3\Scripts\conda.exe"
    if (Test-Path $currentUserPath) { return $currentUserPath }

    # 3. Check system PATH
    $condaInPath = Get-Command conda.exe -ErrorAction SilentlyContinue
    if ($condaInPath) { return $condaInPath.Path }

    # 4. Fallbacks for Anaconda
    $anacondaPaths = @(
        "$env:ProgramFiles\Anaconda3\Scripts\conda.exe",
        "$env:UserProfile\Anaconda3\Scripts\conda.exe"
    )
    foreach ($p in $anacondaPaths) { if (Test-Path $p) { return $p } }

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
    pause
    exit 1
}

Write-Host "Using Conda at: $condaCmd" -ForegroundColor Gray
Write-Host "Starting application... please wait." -ForegroundColor Yellow

try {
    # Extract the base directory to ensure we can find the 'kiwims' environment
    # Moving up from Scripts/conda.exe to the root prefix
    $condaPrefix = Split-Path (Split-Path $condaCmd -Parent) -Parent

    "$(Get-Date) - INFO: Conda Command: $condaCmd" | Add-Content $logFile
    "$(Get-Date) - INFO: Conda Prefix: $condaPrefix" | Add-Content $logFile

    # Launch the App
    # Use --no-capture-output to ensure logs flow into our file correctly
    & $condaCmd run -n kiwims Rscript.exe -e "shiny::runApp('app.R', port = 3838, launch.browser = TRUE)" --vanilla *> $logFile 2>&1

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
    pause
    exit 1
}