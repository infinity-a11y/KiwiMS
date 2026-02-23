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
Write-Output "Using conda at $condaCmd"

# Get environment yml path
$environmentYmlPath = Join-Path $basePath "resources\environment.yml"
if (-Not (Test-Path $environmentYmlPath)) {
    Write-Output "ERROR: environment.yml missing at $environmentYmlPath"
    Stop-Transcript
    exit 1
}

# Get conda info
& $condaCmd info

#-----------------------------#
# Environment Management logic
#-----------------------------#
$maxRetries = 2
$success = $false

# Accept channel policies and set solver
& $condaCmd tos accept
& $condaCmd config --set solver libmamba

for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    try {
        # 1. Check if environment exists by name
        $envList = & $condaCmd env list | Out-String
        $envExists = $envList -match "\b$envName\b"

        if ($envExists) {
            Write-Output "Existing environment '$envName' found. Attempting update..."
            & $condaCmd env update -n $envName -f "$environmentYmlPath" --prune --verbose
        }
        else {
            Write-Output "Environment '$envName' not found. Creating new..."
            & $condaCmd env create -n $envName -f "$environmentYmlPath" --verbose
        }

        # 2. Verify it now exists and get the path for the log
        $infoJson = & $condaCmd info --json | ConvertFrom-Json
        $condaEnvPath = $infoJson.envs | Where-Object { (Split-Path $_ -Leaf) -eq $envName }

        if ($null -ne $condaEnvPath -and (Test-Path $condaEnvPath)) {
            Write-Output "Environment '$envName' is ready at: $condaEnvPath"
            $success = $true
            break
        }
        else {
            throw "Environment path could not be verified after creation/update."
        }
    }
    catch {
        Write-Warning "Attempt $attempt failed: $($_.Exception.Message)"
        if ($attempt -lt $maxRetries) {
            Write-Output "Attempting to remove potentially corrupted environment..."
            # Fixed removal command: no --all for 'env remove'
            & $condaCmd env remove -n $envName -y
        }
    }
}

if (-not $success) {
    Write-Output "CRITICAL ERROR: Failed to synchronize Conda environment."
    Stop-Transcript
    exit 1
}

Write-Output "Conda environment setup complete."
Stop-Transcript
exit 0