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

Write-Host "### Quarto setup (quarto_install.ps1)"
Write-Host "basePath:         $basePath"
Write-Host "userDataPath:     $userDataPath"
Write-Host "envName:          $envName"
Write-Host "logFile:          $logFile"
Write-Host "installScope:     $installScope"

# Determine elevation status
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# Choose installation path based on scope
if ($installScope -eq "allusers") {
    Write-Host "System-wide (all users) mode selected"

    if (-not $isElevated) {
        Write-Host "ERROR: System-wide Quarto installation requires administrator rights."
        Write-Host "Please run the installer as administrator."
        exit 1
    }

    $QUARTO_INSTALL_DIR = Join-Path $env:ProgramFiles "Quarto"
    $pathTarget = "Machine"   # system PATH
} else {
    Write-Host "Current-user mode selected (no elevation required)"
    $QUARTO_INSTALL_DIR = Join-Path $env:LOCALAPPDATA "Programs\Quarto"
    $pathTarget = "User"      # user PATH
}

# Define version and download URL
$QUARTO_VERSION = "1.7.32"
$QUARTO_DOWNLOAD_URL = "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-win.zip"
$QUARTO_TEMP_ZIP = Join-Path $env:TEMP "quarto.zip"

Write-Host "Target installation directory: $QUARTO_INSTALL_DIR"

# Source functions
. "$basePath\functions.ps1"

# Check for existing Quarto installation
Write-Host "Checking for existing Quarto CLI installation..."
$quartoInfo = Find-QuartoInstallation

if ($quartoInfo.Found) {
    Write-Host "Quarto CLI v$($quartoInfo.Version) found at $($quartoInfo.Path)"

    # For simplicity, accept any existing version (you can add version comparison later)
    Write-Host "Using existing Quarto installation."

    # Ensure bin directory is in PATH (system or user depending on scope)
    $quartoBin = Join-Path $quartoInfo.Path "bin"
    if (-not (Test-PathInEnvironment -Directory $quartoBin)) {
        Write-Host "Adding Quarto bin directory to $pathTarget PATH..."
        [Environment]::SetEnvironmentVariable("PATH", 
            "$([Environment]::GetEnvironmentVariable('PATH', $pathTarget));$quartoBin", 
            $pathTarget)
    }

    Write-Host "Quarto CLI is configured and ready."
    exit 0
}

# Quarto not found → install it
Write-Host "Quarto CLI not found. Installing to $QUARTO_INSTALL_DIR..."

# In currentuser mode → no admin required
# In allusers mode → we already checked elevation above

Install-Quarto -InstallDir $QUARTO_INSTALL_DIR

if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Quarto installation failed (exit code $LASTEXITCODE)"
    exit 1
}

Write-Host "Quarto CLI installed successfully to $QUARTO_INSTALL_DIR"

# Add to PATH (system or user)
$quartoBin = Join-Path $QUARTO_INSTALL_DIR "bin"
if (-not (Test-PathInEnvironment -Directory $quartoBin)) {
    Write-Host "Adding Quarto bin directory to $pathTarget PATH..."
    [Environment]::SetEnvironmentVariable("PATH", 
        "$([Environment]::GetEnvironmentVariable('PATH', $pathTarget));$quartoBin", 
        $pathTarget)
}

Write-Host "Quarto installation and configuration complete."
exit 0