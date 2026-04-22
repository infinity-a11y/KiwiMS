# =============================================
# KiwiMS Compiler Script - CORRECTED VERSION
# (matches your original Win-PS2EXE settings exactly)
# No -noConsole → normal console application
# =============================================

# Go to the folder where this script is located
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $scriptDir

Write-Host "=============================================" -ForegroundColor Cyan
Write-Host "       Compiling KiwiMS.exe" -ForegroundColor Cyan
Write-Host "=============================================" -ForegroundColor Cyan
Write-Host ""

# Check if ps2exe is available
if (-not (Get-Command ps2exe -ErrorAction SilentlyContinue)) {
    if (-not (Get-Command Invoke-ps2exe -ErrorAction SilentlyContinue)) {
        Write-Host "[ERROR] PS2EXE module not found." -ForegroundColor Red
        Write-Host "Please run in PowerShell:" -ForegroundColor Yellow
        Write-Host "   Install-Module ps2exe -Scope CurrentUser -Force" -ForegroundColor Yellow
        Read-Host "Press Enter to exit"
        exit 1
    }
}

Write-Host "Starting compilation (console mode)..." -ForegroundColor Green
Write-Host ""

# === Correct command matching your unticked GUI options ===
ps2exe `
    -inputFile "dev\launch.ps1" `
    -outputFile "KiwiMS.exe" `
    -iconFile "resources\favicon.ico" `
    -version "0.5.1" `
    -product "KiwiMS" `
    -description "KiwiMS Launch" `
    -copyright "Marian Freisleben" `
    -STA

# Result
if ($LASTEXITCODE -eq 0) {
    Write-Host ""
    Write-Host "✅ Success! KiwiMS.exe has been created." -ForegroundColor Green
    Write-Host "   It is now a normal console application (black window will appear)." -ForegroundColor Green
}
else {
    Write-Host ""
    Write-Host "❌ Compilation failed. See error messages above." -ForegroundColor Red
}

Write-Host ""
Read-Host "Press Enter to close"