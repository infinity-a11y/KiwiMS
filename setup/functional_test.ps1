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
# 3. Execution Prep
#-----------------------------#
$RPortablePath = Join-Path $basePath "R-Portable\bin\Rscript.exe"

$rSafePath = $basePath.Replace('\', '/')
$shinyCmd = "shiny::runApp('$rSafePath/app.R', launch.browser = FALSE)"

#-----------------------------#
# 4. Launch and Monitor
#-----------------------------#
Write-Output "[Step 2] Running Functional Smoke Test..."
Write-Output "Monitoring app stability for 15s..."

$appProcess = Start-Process -FilePath $RPortablePath -ArgumentList "--no-save", "--no-restore", "-e", "`"$shinyCmd`"" -PassThru -NoNewWindow

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
    Write-Output "Closing test instance..."
    Stop-Process -Id $appProcess.Id -Force -ErrorAction SilentlyContinue
}

Write-Output "Functional test complete."
Stop-Transcript