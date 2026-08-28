# =============================================
# KiwiMS Launcher Compiler
#
# Compiles dev\launch.ps1 into KiwiMS.exe with ps2exe.
# The version stamped into the exe comes from resources\version.txt,
# the single source of truth for the app version.
#
# Kept ASCII-only on purpose: this file is read by Windows PowerShell 5.1,
# which decodes BOM-less files as ANSI and would mangle non-ASCII output.
# =============================================

param(
    # Suppress the "Press Enter" prompts so dev\build_installer.ps1 can drive this
    # script unattended.
    [switch] $NoPause
)

# Go to the folder where this script is located
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $scriptDir

function Fail([string] $Message) {
    Write-Host ""
    Write-Host "[ERROR] $Message" -ForegroundColor Red
    if (-not $NoPause) { Read-Host "Press Enter to close" }
    exit 1
}

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
        if (-not $NoPause) { Read-Host "Press Enter to exit" }
        exit 1
    }
}

# --- Version -----------------------------------------------------------------
# Read the app version from the single source of truth. ps2exe only accepts a
# numeric dotted version for the Win32 file-version resource, so strip any
# pre-release suffix (0.8.0-rc1 -> 0.8.0) before handing it over.
$versionPath = Join-Path $scriptDir 'resources\version.txt'
if (-not (Test-Path $versionPath)) { Fail "Version file not found: $versionPath" }

$versionLine = Get-Content -Path $versionPath | Where-Object { $_ -match '^\s*version\s*=' } | Select-Object -First 1
if (-not $versionLine) { Fail "No 'version=' entry in $versionPath" }

$appVersion = ($versionLine -replace '^\s*version\s*=', '').Trim()
$fileVersion = if ($appVersion -match '^\d+(\.\d+){0,3}') { $Matches[0] } else { $null }
if (-not $fileVersion) { Fail "Could not parse a numeric version from '$appVersion'" }

Write-Host "Version: $appVersion (file version $fileVersion)" -ForegroundColor Gray
Write-Host "Starting compilation (console mode)..." -ForegroundColor Green
Write-Host ""

# --- Compile -----------------------------------------------------------------
# ps2exe is a module function, not a native executable, so it never sets
# $LASTEXITCODE. Detect success from the output file itself instead.
$outputFile = Join-Path $scriptDir 'KiwiMS.exe'
$before = if (Test-Path $outputFile) { (Get-Item $outputFile).LastWriteTimeUtc } else { [datetime]::MinValue }

$compileError = $null
try {
    ps2exe `
        -inputFile "dev\launch.ps1" `
        -outputFile "KiwiMS.exe" `
        -iconFile "resources\favicon.ico" `
        -version $fileVersion `
        -product "KiwiMS" `
        -description "KiwiMS Launch" `
        -copyright "Marian Freisleben" `
        -STA
}
catch {
    $compileError = $_
}

# --- Result ------------------------------------------------------------------
if ($compileError) { Fail "ps2exe threw: $($compileError.Exception.Message)" }
if (-not (Test-Path $outputFile)) { Fail "ps2exe produced no output file. See messages above." }
if ((Get-Item $outputFile).LastWriteTimeUtc -le $before) {
    Fail "KiwiMS.exe was not rewritten. See messages above."
}

Write-Host ""
Write-Host "[OK] KiwiMS.exe has been created (version $fileVersion)." -ForegroundColor Green
Write-Host "     It is a normal console application (black window will appear)." -ForegroundColor Green
Write-Host ""
if (-not $NoPause) { Read-Host "Press Enter to close" }
exit 0
