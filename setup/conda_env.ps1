#-----------------------------#
# Script Initialization
#-----------------------------#

param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append

Write-Host "basePath: $basePath"
Write-Host "userDataPath: $userDataPath"
Write-Host "envName: $envName"
Write-Host "logFile: $logFile"

# Source functions
. "$basePath\functions.ps1"

# Path declaration
$condaCmd = Find-CondaExecutable
$condaPrefix = [System.IO.Path]::GetDirectoryName([System.IO.Path]::GetDirectoryName($condaCmd))
$condaEnvPath = Join-Path $condaPrefix "envs\$envName"

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Miniconda not found after installation. Exiting."
    exit 1
}

# Accept channel policies
& $condaCmd tos accept

# Create or Update Conda Env
Write-Host "Creating or updating conda environment..."

# Check if environment.yml exists
$environmentYmlPath = Join-Path $basePath "resources\environment.yml" 
if (-Not (Test-Path $environmentYmlPath)) {
    Write-Host "ERROR: environment.yml not found at '$environmentYmlPath'. Cannot create Conda environment. Exiting."
    exit 1
}

# Creating conda environment
$maxRetries = 3
for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    Write-Host "Attempt $attempt of $maxRetries to manage conda environment."
    
    try {
        # Clear conda cache to prevent persistent partial download issues
        Write-Host  "Clearing conda package cache..."
        & $condaCmd clean --all -y | Out-String | ForEach-Object { Write-Host "Conda clean: $_" }
        
        if (Test-Path $condaEnvPath) {
            Write-Host "Existing environment '$envName' detected at '$condaEnvPath'. Removing for fresh creation."
            & $condaCmd env remove -n $envName -y
            Write-Host "kiwiflow conda environment removed."
        }

        # Attempt to create the environment
        Write-Host "Running: '$condaCmd env create -f "$environmentYmlPath" -n $envName -y'"
        & $condaCmd env create -f "$environmentYmlPath" -n $envName

        # Check for success within the output or by path
        if (Test-Path $condaEnvPath) {
            Write-Host "Conda environment '$envName' created or updated successfully."
            break # Exit the retry loop on success
        }
        else {
            throw "Conda environment '$envName' not found after creation command completed."
        }
    }
    catch {
        Write-Host  "ERROR: Failed to manage conda environment on attempt $attempt. Error: $($_.Exception.Message)"
        # Log specific error details for debugging
        if ($_.Exception.InnerException) {
            Write-Host  "Inner Exception: $($_.Exception.InnerException.Message)"
        }
        Write-Host "Error record: $($_.Exception | Format-List -Force)"
        
        if ($attempt -eq $maxRetries) {
            Write-Host "All retry attempts failed. Exiting script."
            exit 1 # Exit if max retries reached
        }
        Write-Host  "Retrying in 10 seconds..."
        Start-Sleep -Seconds 10
    }
}
