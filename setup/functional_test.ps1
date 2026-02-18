#-----------------------------#
# 1. Script Initialization
#-----------------------------#
param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue"
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "   KiwiMS Functional Test                 " -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan

#-----------------------------#
# Port Check
#-----------------------------#
$port = 3838
Write-Host "[Step 1] Checking if port $port is available..." -ForegroundColor Yellow

$portProcess = Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue

if ($portProcess) {
    $pidToKill = $portProcess.OwningProcess
    Write-Host "Port $port is occupied by PID $pidToKill. Clearing it now..." -ForegroundColor Yellow
    try {
        Stop-Process -Id $pidToKill -Force -ErrorAction Stop
        Write-Host "Existing process terminated." -ForegroundColor Gray
        Start-Sleep -Seconds 2 # Time to release the socket
    } catch {
        Write-Host "Warning: Could not stop process $pidToKill. Test may fail." -ForegroundColor Red
    }
} else {
    Write-Host "Port $port is free." -ForegroundColor Gray
}

#-----------------------------#
# 3. Execution Prep
#-----------------------------#
if (Test-Path "$basePath\functions.ps1") { . "$basePath\functions.ps1" }
$condaCmd = Find-CondaExecutable

$rSafePath = $basePath.Replace('\', '/')
$shinyCmd = "shiny::runApp('$rSafePath/app.R', port = $port, launch.browser = FALSE)"
$processArgs = @("run", "-n", $envName, "--no-capture-output", "Rscript.exe", "-e", "`"$shinyCmd`"", "--vanilla")

#-----------------------------#
# 4. Launch and Monitor
#-----------------------------#
Write-Host "[Step 2] Running Functional Smoke Test..." -ForegroundColor Yellow
Write-Host "Monitoring app stability for 15s..." -ForegroundColor Gray

$appProcess = Start-Process -FilePath $condaCmd -ArgumentList $processArgs -PassThru -NoNewWindow

for ($i=0; $i -lt 15; $i++) {
    Write-Host "." -NoNewline
    Start-Sleep -Seconds 1
    if ($appProcess.HasExited) {
        Write-Host "`n[FAIL] App crashed! Exit Code: $($appProcess.ExitCode)" -ForegroundColor Red
        Stop-Transcript; exit 1
    }
}

#-----------------------------#
# 5. Success & Cleanup
#-----------------------------#
Write-Host "`n[SUCCESS] App engine is stable." -ForegroundColor Green

if ($appProcess -and -not $appProcess.HasExited) {
    Write-Host "Closing test instance and cleaning up port..." -ForegroundColor Gray
    
    # Kill the process tree
    Stop-Process -Id $appProcess.Id -Force -ErrorAction SilentlyContinue

    # Clear port 3838
    $finalCheck = Get-NetTCPConnection -LocalPort 3838 -State Listen -ErrorAction SilentlyContinue
    if ($finalCheck) {
        Stop-Process -Id $finalCheck.OwningProcess -Force -ErrorAction SilentlyContinue
    }
}

Write-Host "Functional test complete."
Stop-Transcript