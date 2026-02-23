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

Write-Output "### Setting up Conda Environment (conda_env.ps1)"

# Source functions
. "$basePath\functions.ps1"

# Discovery
$condaCmd = Find-CondaExecutable

if (-Not (Test-Path $condaCmd)) {
    Write-Output "ERROR: Conda not found at $condaCmd"
    Stop-Transcript
    exit 1
}

Write-Output $condaCmd

$condaPrefix = Split-Path (Split-Path $condaCmd -Parent) -Parent
$condaEnvPath = Join-Path $condaPrefix "envs\$envName"
$environmentYmlPath = Join-Path $basePath "resources\environment.yml"

if (-Not (Test-Path $environmentYmlPath)) {
    Write-Output "ERROR: environment.yml missing at $environmentYmlPath"
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Environment Management logic
#-----------------------------#
$maxRetries = 2
$success = $false

# Accept channel policies
& $condaCmd tos accept

# Set the libmamba solver
& $condaCmd config --set solver libmamba

for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    try {
        if (Test-Path $condaEnvPath) {
            Write-Output "Existing environment found. Attempting incremental update (cache-aware)..."
            & $condaCmd env update -n $envName -f "$environmentYmlPath" --prune --verbose
        }
        else {
            Write-Output "Environment not found. Creating new environment from cache/source..."
            & $condaCmd env create -n $envName -f "$environmentYmlPath" --verbose
        }

        # Verify the python/R executables exist in the new env to confirm success
        if (Test-Path $condaEnvPath) {
            Write-Output "Environment '$envName' is synchronized and ready."
            $success = $true
            break
        }
    }
    catch {
        Write-Warning "Attempt $attempt failed: $($_.Exception.Message)"
        if ($attempt -lt $maxRetries) {
            Write-Output "Attempting to remove corrupted environment for a fresh rebuild..."
            & $condaCmd env remove -n $envName -y --all
        }
    }
}

if (-not $success) {
    Write-Output "CRITICAL ERROR: Failed to synchronize Conda environment."
    exit 1
}

Write-Output "Conda environment setup complete."
Stop-Transcript
exit 0