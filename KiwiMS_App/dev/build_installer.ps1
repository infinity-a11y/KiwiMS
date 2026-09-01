<#
    Builds KiwiMS-Windows-x86_64.exe end to end.

        1. dev\build-launcher.ps1   compile KiwiMS.exe (ps2exe)
        2. compileall               refresh env_kiwims bytecode
        3. ISCC setup_script.iss

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
    Write-Host "[1/4] Checking conda-unpack manifest" -ForegroundColor Cyan
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
Write-Host "[2/4] Compiling KiwiMS.exe" -ForegroundColor Cyan
$global:LASTEXITCODE = 0
& (Join-Path $appRoot 'dev\build-launcher.ps1') -NoPause
if ($LASTEXITCODE -ne 0) {
    Write-Host "[ERROR] Launcher compilation failed." -ForegroundColor Red
    exit 1
}

# --- Python bytecode -------------------------------------------------------------
# Ship bytecode that does not depend on file timestamps. Python validates a .pyc
# against the mtime of its .py, and nothing in this pipeline preserves those
# exactly: roughly 3,700 of the ~9,800 .pyc files in env_kiwims are already stale
# here, because conda stamped the .py files with the env-creation date while the
# .pyc came from the upstream package build, and the installer's coarser
# timestamps invalidate a further ~4,500 on the way in. The environment then
# recompiles about 90% of itself in every process that imports it - 6.2 s against
# 1.8 s for the UniDec import chain, paid by every worker on every run.
#
# --invalidation-mode unchecked-hash stamps each .pyc with the hash of its source
# rather than its mtime and never revalidates it, so the cache survives any copy.
# setup_script.iss runs this again after conda-unpack, which is the authoritative
# pass (conda-unpack rewrites prefixes inside some sources, and hash-based
# bytecode is never rechecked). Doing it here as well means the shipped payload is
# already correct should that step ever be skipped or fail.
Write-Host ""
Write-Host "[3/4] Pre-compiling the Python environment" -ForegroundColor Cyan
$envPython = Join-Path $appRoot 'env_kiwims\python.exe'
$envLib = Join-Path $appRoot 'env_kiwims\Lib'
if (Test-Path $envPython) {
    # -W ignore silences some sixty upstream SyntaxWarnings for invalid escape
    # sequences ("C:\Data\..." paths, unescaped regexes) in UniDec, multiplierz and
    # pyteomics. They are noise from third-party source, and the modules they come
    # from compile correctly. That they appear at this step at all is the point of
    # it: they used to be emitted at run time, in every worker, because those
    # modules were being recompiled on every single import.
    #
    # -x skips the three files no Python 3 can compile: contrib\netpubsub.py holds
    # pseudo-code and contrib\wx_monitor.py uses the Python 2 "raise Error, msg"
    # form (both are relics shipped inside pubsub), and
    # multiplierz\mzTools\chargeTransform.py declares a global after using it. None
    # of them is reachable from `import unidec` and none could ever be imported, so
    # excluding them costs nothing and makes a non-zero exit code meaningful again.
    $previousEap = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    & $envPython -W ignore -m compileall -q -f -j 0 `
        --invalidation-mode unchecked-hash `
        -x '(netpubsub|wx_monitor|chargeTransform)\.py$' $envLib
    $compileExit = $LASTEXITCODE
    $ErrorActionPreference = $previousEap
    $global:LASTEXITCODE = 0
    if ($compileExit -eq 0) {
        Write-Host "      Bytecode regenerated for the whole environment." -ForegroundColor Green
    }
    else {
        Write-Host "[WARN] compileall exited $compileExit - a module failed to compile." -ForegroundColor Yellow
        Write-Host "       Not fatal, but worth reading: the environment starts slower" -ForegroundColor Yellow
        Write-Host "       for whatever no longer has cached bytecode." -ForegroundColor Yellow
    }
}
else {
    Write-Host "[WARN] $envPython not found - skipping pre-compilation." -ForegroundColor Yellow
}

# --- Installer -------------------------------------------------------------------
Write-Host ""
Write-Host "[4/4] Compiling the installer (this takes a while)" -ForegroundColor Cyan
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
