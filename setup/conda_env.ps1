#-----------------------------#
# Script Initialization
#-----------------------------#

param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope = "currentuser"
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Setting up Conda Environment (conda_env.ps1)"
Write-Host "basePath:         $basePath"
Write-Host "userDataPath:     $userDataPath"
Write-Host "envName:          $envName"
Write-Host "logFile:          $logFile"
Write-Host "installScope:     $installScope"

# Determine if running elevated
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# Source functions
. "$basePath\functions.ps1"

# Decide paths based on scope
if ($installScope -eq "allusers") {
    Write-Host "System-wide (all users) mode selected"

    if (-not $isElevated) {
        Write-Host "ERROR: System-wide installation requires administrator rights."
        Write-Host "Please run the installer as administrator."
        exit 1
    }

    # System-wide Miniconda location
    $condaPrefix = "$env:ProgramData\miniconda3"
} else {
    Write-Host "Current-user mode selected (no elevation required)"
    # User-specific Miniconda location (should already be set by miniconda_installer.ps1)
    $condaPrefix = "$env:LOCALAPPDATA\miniconda3"
}

# Path declaration
# $condaCmd = Join-Path $condaPrefix "Scripts\conda.exe"
$condaCmd = Find-CondaExecutable
$condaPrefix = [System.IO.Path]::GetDirectoryName([System.IO.Path]::GetDirectoryName($condaCmd))
$condaEnvPath = Join-Path $condaPrefix "envs\$envName"

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "ERROR: Miniconda not found at expected location: $condaCmd"
    Write-Host "Make sure the Miniconda installation step completed successfully."
    exit 1
}

Write-Host "Using Conda at: $condaCmd"
Write-Host "Target env path: $condaEnvPath"

# Accept channel policies (non-interactive)
& $condaCmd config --set channel_priority strict
& $condaCmd config --add channels conda-forge
& $condaCmd config --add channels defaults
& $condaCmd config --set report_errors false

# Create or Update Conda Env
Write-Host "Creating or updating conda environment..."

# Check if environment.yml exists
$environmentYmlPath = Join-Path $basePath "resources\environment.yml"
if (-Not (Test-Path $environmentYmlPath)) {
    Write-Host "ERROR: environment.yml not found at '$environmentYmlPath'. Cannot create Conda environment."
    exit 1
}

# Creating conda environment with retries
$maxRetries = 3
$success = $false

for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    Write-Host "Attempt $attempt of $maxRetries to manage conda environment."
    
    try {
        # Clear conda cache
        Write-Host "Clearing conda package cache..."
        & $condaCmd clean --all -y --json | Out-Null

        # Remove existing env if present
        if (Test-Path $condaEnvPath) {
            Write-Host "Existing environment '$envName' detected. Removing for fresh creation."
            & $condaCmd env remove -n $envName -y --json | Out-Null
            Write-Host "Environment removed."
        }

        # Create new environment
        Write-Host "Creating environment from $environmentYmlPath..."
        & $condaCmd env create -f "$environmentYmlPath" -n $envName -y --json

        # Verify success
        if (Test-Path $condaEnvPath) {
            Write-Host "Conda environment '$envName' created successfully."
            $success = $true
            break
        } else {
            throw "Environment path $condaEnvPath not found after creation."
        }
    }
    catch {
        Write-Host "ERROR on attempt"
        
        if ($attempt -eq $maxRetries) {
            Write-Host "All retry attempts failed. Exiting."
            exit 1
        }
        
        Write-Host "Retrying in 10 seconds..."
        Start-Sleep -Seconds 10
    }
}

if (-not $success) {
    Write-Host "Final failure: Conda environment was not created."
    exit 1
}

Write-Host "Conda environment setup complete."
exit 0