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

Write-Output "=========================================="
Write-Output "   KiwiMS Functional Test                 "
Write-Output "=========================================="

#-----------------------------#
# Port Check
#-----------------------------#
$port = 3838
Write-Output "[Step 1] Checking if port $port is available..."

$portProcess = Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue

if ($portProcess) {
    $pidToKill = $portProcess.OwningProcess
    Write-Output "Port $port is occupied by PID $pidToKill. Clearing it now..."
    try {
        Stop-Process -Id $pidToKill -Force -ErrorAction Stop
        Write-Output "Existing process terminated."
        Start-Sleep -Seconds 2 # Time to release the socket
    }
    catch {
        Write-Output "Warning: Could not stop process $pidToKill. Test may fail."
    }
}
else {
    Write-Output "Port $port is free."
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
Write-Output "[Step 2] Running Functional Smoke Test..."
Write-Output "Monitoring app stability for 15s..."

$appProcess = Start-Process -FilePath $condaCmd -ArgumentList $processArgs -PassThru -NoNewWindow

for ($i = 0; $i -lt 15; $i++) {
    Write-Output "." -NoNewline
    Start-Sleep -Seconds 1
    if ($appProcess.HasExited) {
        Write-Output "`n[FAIL] App crashed! Exit Code: $($appProcess.ExitCode)"
        Stop-Transcript; exit 1
    }
}

#-----------------------------#
# 5. Success & Cleanup
#-----------------------------#
Write-Output "`n[SUCCESS] App engine is stable."

if ($appProcess -and -not $appProcess.HasExited) {
    Write-Output "Closing test instance and cleaning up port..."

    # Kill the process tree
    Stop-Process -Id $appProcess.Id -Force -ErrorAction SilentlyContinue

    # Clear port 3838
    $finalCheck = Get-NetTCPConnection -LocalPort 3838 -State Listen -ErrorAction SilentlyContinue
    if ($finalCheck) {
        Stop-Process -Id $finalCheck.OwningProcess -Force -ErrorAction SilentlyContinue
    }
}

Write-Output "Functional test complete."
Stop-Transcript