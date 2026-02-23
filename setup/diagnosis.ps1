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

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

# Source helper functions
if (Test-Path "$basePath\functions.ps1") {
    . "$basePath\functions.ps1"
}

Write-Output "=========================================="
Write-Output "   KiwiMS Post-Installation Diagnosis     "
Write-Output "=========================================="
Write-Output "Project Root: $basePath"

#-----------------------------#
# 1. System Settings Retrieval
#-----------------------------#
Write-Output "`n[1/5] System Environment"
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
Write-Output "Run as Admin:  $isElevated"
Write-Output "OS Version:    $((Get-CimInstance Win32_OperatingSystem).Caption)"

Write-Output "`n--- Environment PATH ---"
$env:Path -split ";" | ForEach-Object {
    if ($_) {
        $color = if (Test-Path $_) { "Gray" } else { "Red" }
        Write-Output " - $_"
    }
}

#-----------------------------#
# 2. Conda Environment Diagnosis
#-----------------------------#
Write-Output "`n[2/5] Conda Environment Health"
$condaCmd = Find-CondaExecutable

if ($condaCmd) {
    $envs = & $condaCmd env list
    if ($envs -like "*$envName*") {
        Write-Output "Environment '$envName' found."
        Write-Output "--- Installed Conda Packages ---"
        & $condaCmd list -n $envName
    }
    else {
        Write-Output "ERROR: Conda environment '$envName' NOT found."
    }
}
else {
    Write-Output "ERROR: Conda executable not found."
}

#-----------------------------#
# 3. renv Deep Diagnosis (Root Level)
#-----------------------------#
Write-Output "`n[3/5] renv Environment Diagnosis"

if ($condaCmd -and (Test-Path (Join-Path $basePath "renv.lock"))) {
    # Move to the base directory where renv.lock exists
    Push-Location $basePath
    try {
        # Format path for R (forward slashes)
        $rRootPath = $basePath -replace '\\', '/'

        Write-Output "--- renv::status() ---"
        # We explicitly target the current directory as the project
        & $condaCmd run -n $envName Rscript -e "setwd('$rRootPath'); if (!requireNamespace('renv', quietly=TRUE)) { stop('renv not found') }; renv::status(project = '.')" 2>&1 | Out-String | Write-Output

        Write-Output "`n--- renv::diagnostics() ---"
        & $condaCmd run -n $envName Rscript -e "setwd('$rRootPath'); renv::diagnostics(project = '.')" 2>&1 | Out-String | Write-Output
    }
    catch {
        Write-Output "Failed to execute renv diagnosis: $($_.Exception.Message)"
    }
    finally {
        Pop-Location
    }
}
else {
    Write-Output "ERROR: renv.lock not found in $basePath or Conda is missing."
}

#-----------------------------#
# 4. Toolchain Check
#-----------------------------#
Write-Output "`n[4/5] Toolchain Verification"

$rtools = Find-Rtools45Executable
if ($rtools) {
    Write-Output "[OK] Rtools 4.5: Found at $rtools"
}
else {
    Write-Output "[FAIL] Rtools 4.5: Missing."
}

$quarto = Find-QuartoInstallation
if ($quarto.Found) {
    Write-Output "[OK] Quarto: Version $($quarto.Version) found."
}
else {
    Write-Output "[FAIL] Quarto: Not detected."
}

#-----------------------------#
# 5. Summary
#-----------------------------#
Write-Output "`n=========================================="
Write-Output "Diagnosis Finished."
Write-Output "=========================================="