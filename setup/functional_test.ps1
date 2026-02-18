#-----------------------------#
# Script Initialization
#-----------------------------#
param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue"
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "   KiwiMS Functional Launch Smoke Test    " -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan

# Source helper functions
if (Test-Path "$basePath\functions.ps1") { . "$basePath\functions.ps1" }

$condaCmd = Find-CondaExecutable
if (-not $condaCmd) {
    Write-Host "ERROR: Conda not found." -ForegroundColor Red
    Stop-Transcript; exit 1
}

#-----------------------------#
# 2. Execution (Asynchronous with Output)
#-----------------------------#
# Path handling for R (Forward Slashes)
$rSafePath = $basePath.Replace('\', '/')
$shinyCmd = "shiny::runApp('$rSafePath/app.R', port = 3838, launch.browser = FALSE)"

Write-Host "[Step] Starting app engine in background..." -ForegroundColor Yellow
Write-Host "Monitoring for 20 seconds. If a crash occurs, the error will appear below." -ForegroundColor Gray

# We use an argument list that ensures R output is NOT captured by Conda 
# but is instead allowed to flow to the host.
$processArgs = @("run", "-n", $envName, "--no-capture-output", "Rscript.exe", "-e", $shinyCmd, "--vanilla")

# Start-Process with -PassThru allows us to monitor it without blocking the script
$appProcess = Start-Process -FilePath $condaCmd -ArgumentList $processArgs `
    -NoNewWindow -PassThru

# Monitor loop
for ($i = 0; $i -lt 20; $i++) {
    Write-Host "." -NoNewline
    Start-Sleep -Seconds 1
    
    # Check if the process crashed
    if ($appProcess.HasExited) {
        Write-Host "`n[FAIL] R process crashed with Exit Code: $($appProcess.ExitCode)" -ForegroundColor Red
        Write-Host "Check the console output above for the specific R error message."
        Stop-Transcript
        exit 1
    }
}

#-----------------------------#
# 3. Cleanup & Result
#-----------------------------#
Write-Host "`n[SUCCESS] App engine remained stable for 20s." -ForegroundColor Green

if (-not $appProcess.HasExited) {
    Write-Host "Terminating test instance..." -ForegroundColor Gray
    Stop-Process -Id $appProcess.Id -Force -ErrorAction SilentlyContinue
}

Write-Host "==========================================" -ForegroundColor Cyan
Stop-Transcript