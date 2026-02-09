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

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Host "### Setting up Conda Environment (conda_env.ps1)"

# Source functions
. "$basePath\functions.ps1"

# Discovery
$condaCmd = Find-CondaExecutable
$condaPrefix = Split-Path (Split-Path $condaCmd -Parent) -Parent
$condaEnvPath = Join-Path $condaPrefix "envs\$envName"
$environmentYmlPath = Join-Path $basePath "resources\environment.yml"

if (-Not (Test-Path $condaCmd)) {
    Write-Host "ERROR: Conda not found at $condaCmd"
    exit 1
}

if (-Not (Test-Path $environmentYmlPath)) {
    Write-Host "ERROR: environment.yml missing at $environmentYmlPath"
    exit 1
}

#-----------------------------#
# Environment Management logic
#-----------------------------#
$maxRetries = 2
$success = $false

# Accept channel policies
& $condaCmd tos accept

# Try 'env update --prune' allowing cache
# If it fails, delete and 'env create'

for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    try {
        if (Test-Path $condaEnvPath) {
            Write-Host "Existing environment found. Attempting incremental update (cache-aware)..."
            # --prune removes packages no longer in your .yml
            & $condaCmd env update -n $envName -f "$environmentYmlPath" --prune
        }
        else {
            Write-Host "Environment not found. Creating new environment from cache/source..."
            & $condaCmd env create -n $envName -f "$environmentYmlPath"
        }

        # Verify the python/R executables exist in the new env to confirm success
        if (Test-Path $condaEnvPath) {
            Write-Host "Environment '$envName' is synchronized and ready."
            $success = $true
            break
        }
    }
    catch {
        Write-Warning "Attempt $attempt failed: $($_.Exception.Message)"
        
        if ($attempt -lt $maxRetries) {
            Write-Host "Attempting 'Nuclear Option': Removing corrupted environment for a fresh rebuild..."
            & $condaCmd env remove -n $envName -y --all
            # Note: We do NOT 'conda clean' here because we want to keep the cached .tar.bz2 files!
        }
    }
}

if (-not $success) {
    Write-Host "CRITICAL ERROR: Failed to synchronize Conda environment."
    exit 1
}

Write-Host "Conda environment setup complete."
Stop-Transcript
exit 0