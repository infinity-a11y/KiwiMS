#-----------------------------#
# Script Initialization
#-----------------------------#
param(
    [string]$basePath,         # Directory containing diagnosis.ps1 and renv.lock
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue" 

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

# Source helper functions
if (Test-Path "$basePath\functions.ps1") {
    . "$basePath\functions.ps1"
}

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "   KiwiMS Post-Installation Diagnosis     " -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Project Root: $basePath"

#-----------------------------#
# 1. System Settings Retrieval
#-----------------------------#
Write-Host "`n[1/5] System Environment" -ForegroundColor Yellow
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
Write-Host "Run as Admin:  $isElevated"
Write-Host "OS Version:    $((Get-CimInstance Win32_OperatingSystem).Caption)"

Write-Host "`n--- Environment PATH ---"
$env:Path -split ";" | ForEach-Object { 
    if ($_) {
        $color = if (Test-Path $_) { "Gray" } else { "Red" }
        Write-Host " - $_" -ForegroundColor $color
    }
}

#-----------------------------#
# 2. Conda Environment Diagnosis
#-----------------------------#
Write-Host "`n[2/5] Conda Environment Health" -ForegroundColor Yellow
$condaCmd = Find-CondaExecutable

if ($condaCmd) {
    $envs = & $condaCmd env list
    if ($envs -like "*$envName*") {
        Write-Host "Environment '$envName' found." -ForegroundColor Green
        Write-Host "--- Installed Conda Packages ---"
        & $condaCmd list -n $envName
    }
    else {
        Write-Host "ERROR: Conda environment '$envName' NOT found." -ForegroundColor Red
    }
}
else {
    Write-Host "ERROR: Conda executable not found." -ForegroundColor Red
}

#-----------------------------#
# 3. renv Deep Diagnosis (Root Level)
#-----------------------------#
Write-Host "`n[3/5] renv Environment Diagnosis" -ForegroundColor Yellow

if ($condaCmd -and (Test-Path (Join-Path $basePath "renv.lock"))) {
    # Move to the base directory where renv.lock exists
    Push-Location $basePath
    try {
        # Format path for R (forward slashes)
        $rRootPath = $basePath -replace '\\', '/'

        Write-Host "--- renv::status() ---" -ForegroundColor Gray
        # We explicitly target the current directory as the project
        & $condaCmd run -n $envName Rscript -e "setwd('$rRootPath'); if (!requireNamespace('renv', quietly=TRUE)) { stop('renv not found') }; renv::status(project = '.')" 2>&1 | Out-String | Write-Host

        Write-Host "`n--- renv::diagnostics() ---" -ForegroundColor Gray
        & $condaCmd run -n $envName Rscript -e "setwd('$rRootPath'); renv::diagnostics(project = '.')" 2>&1 | Out-String | Write-Host
    } 
    catch {
        Write-Host "Failed to execute renv diagnosis: $($_.Exception.Message)" -ForegroundColor Red
    }
    finally {
        Pop-Location
    }
}
else {
    Write-Host "ERROR: renv.lock not found in $basePath or Conda is missing." -ForegroundColor Red
}

#-----------------------------#
# 4. Toolchain Check
#-----------------------------#
Write-Host "`n[4/5] Toolchain Verification" -ForegroundColor Yellow

$rtools = Find-Rtools45Executable
if ($rtools) {
    Write-Host "[OK] Rtools 4.5: Found at $rtools" -ForegroundColor Green
}
else {
    Write-Host "[FAIL] Rtools 4.5: Missing." -ForegroundColor Red
}

$quarto = Find-QuartoInstallation
if ($quarto.Found) {
    Write-Host "[OK] Quarto: Version $($quarto.Version) found." -ForegroundColor Green
}
else {
    Write-Host "[FAIL] Quarto: Not detected." -ForegroundColor Red
}

#-----------------------------#
# 5. Summary
#-----------------------------#
Write-Host "`n==========================================" -ForegroundColor Cyan
Write-Host "Diagnosis Finished." -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan