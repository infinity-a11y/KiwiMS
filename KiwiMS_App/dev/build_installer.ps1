<#
    Builds KiwiMS-Windows-x86_64.exe end to end.

        1. dev\build-launcher.ps1   compile KiwiMS.exe (ps2exe)
        2. ISCC setup_script.iss

    Prerequisites, none of which this script creates:
        - KiwiMS_App\env_kiwims\   conda-pack output of the kiwims environment
        - KiwiMS_App\renv\library\ restored R library
        - KiwiMS_App\R-Portable\   portable R 4.5.2
        - Inno Setup 6 (ISCC.exe) and the ps2exe module

    Usage:
        .\dev\build_installer.ps1
#>

[CmdletBinding()]
param(
    # Explicit path to ISCC.exe if it is not in PATH or the default location.
    [string] $Iscc
)

$ErrorActionPreference = 'Stop'

$appRoot = Split-Path -Parent $PSScriptRoot   # ...\KiwiMS_App
$repoRoot = Split-Path -Parent $appRoot        # ...\KiwiMS
$issFile = Join-Path $repoRoot 'setup_script.iss'

Write-Host ""
Write-Host "=============================================" -ForegroundColor Cyan
Write-Host "       Building KiwiMS installer" -ForegroundColor Cyan
Write-Host "=============================================" -ForegroundColor Cyan
Write-Host "  Repo root : $repoRoot"
Write-Host ""

# --- Prerequisites --------------------------------------------------------------
foreach ($required in @(
        (Join-Path $appRoot 'env_kiwims'),
        (Join-Path $appRoot 'renv\library'),
        (Join-Path $appRoot 'R-Portable'),
        $issFile
    )) {
    if (-not (Test-Path $required)) {
        Write-Host "[ERROR] Missing prerequisite: $required" -ForegroundColor Red
        exit 1
    }
}

if (-not $Iscc) {
    $candidates = @(
        'ISCC.exe',
        "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe",
        "$env:ProgramFiles\Inno Setup 6\ISCC.exe"
    )
    foreach ($candidate in $candidates) {
        $found = Get-Command $candidate -ErrorAction SilentlyContinue
        if ($found) { $Iscc = $found.Source; break }
    }
}
if (-not $Iscc) {
    Write-Host "[ERROR] ISCC.exe not found. Install Inno Setup 6 or pass -Iscc <path>." -ForegroundColor Red
    exit 1
}

# --- Sanity check: the environment must still be intact --------------------------
# conda-unpack rewrites the build prefix in every file conda-pack recorded, and a
# missing entry aborts the install rather than being skipped. Catch that here, at build
# time, instead of on a user's machine.
$unpackScript = Join-Path $appRoot 'env_kiwims\Scripts\conda-unpack-script.py'
if (Test-Path $unpackScript) {
    Write-Host "[1/3] Checking conda-unpack manifest" -ForegroundColor Cyan
    $entryPattern = [regex]"^\('([^']+)',\s*'"
    $envRoot = Join-Path $appRoot 'env_kiwims'
    $total = 0
    $missing = @()
    foreach ($line in [System.IO.File]::ReadAllLines($unpackScript)) {
        $m = $entryPattern.Match($line)
        if (-not $m.Success) { continue }
        $total++
        $rel = ($m.Groups[1].Value -replace '\\\\', '/') -replace '\\', '/'
        $full = Join-Path $envRoot ($rel -replace '/', '\')
        if (-not (Test-Path -LiteralPath $full)) { $missing += $full }
    }
    if ($missing.Count -gt 0) {
        Write-Host ("[ERROR] {0:N0} of {1:N0} manifest entries are missing from env_kiwims." -f $missing.Count, $total) -ForegroundColor Red
        Write-Host "        conda-unpack would fail and the install would abort. Rebuild the" -ForegroundColor Red
        Write-Host "        environment from resources\environment.yml and re-run conda-pack." -ForegroundColor Red
        Write-Host "        First few missing:" -ForegroundColor Red
        $missing | Select-Object -First 5 | ForEach-Object { Write-Host ("          " + $_) -ForegroundColor DarkGray }
        exit 1
    }
    Write-Host ("      {0:N0} manifest entries, all present." -f $total) -ForegroundColor Green
}
else {
    Write-Host "[WARN] conda-unpack-script.py not found - skipping manifest check." -ForegroundColor Yellow
}

# --- Version ---------------------------------------------------------------------
# KiwiMS.exe and the .iss both read resources\version.txt themselves, so they cannot
# drift. README.md and CITATION carry literal copies and can, so warn about it here
# rather than discovering it after the release is tagged. Not fatal: the mismatch
# never reaches the installer.
& (Join-Path $PSScriptRoot 'set-version.ps1') -Check
if ($LASTEXITCODE -ne 0) {
    Write-Host "[WARN] Continuing with the build - fix the documents before tagging." -ForegroundColor Yellow
}
Write-Host ""

# --- Launcher --------------------------------------------------------------------
Write-Host "[2/3] Compiling KiwiMS.exe" -ForegroundColor Cyan
$global:LASTEXITCODE = 0
& (Join-Path $appRoot 'dev\build-launcher.ps1') -NoPause
if ($LASTEXITCODE -ne 0) {
    Write-Host "[ERROR] Launcher compilation failed." -ForegroundColor Red
    exit 1
}

# --- Installer -------------------------------------------------------------------
Write-Host ""
Write-Host "[3/3] Compiling the installer (this takes a while)" -ForegroundColor Cyan
& $Iscc $issFile
if ($LASTEXITCODE -ne 0) {
    Write-Host "[ERROR] ISCC failed." -ForegroundColor Red
    exit 1
}

$output = Join-Path $repoRoot 'KiwiMS-Windows-x86_64.exe'
Write-Host ""
Write-Host "=============================================" -ForegroundColor Green
Write-Host "  Installer built" -ForegroundColor Green
if (Test-Path $output) {
    Write-Host ("  {0} ({1:N0} MB)" -f $output, ((Get-Item $output).Length / 1MB)) -ForegroundColor Green
}
Write-Host "=============================================" -ForegroundColor Green
