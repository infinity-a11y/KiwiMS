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
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Quarto setup (quarto_install.ps1)"

Write-Host "basePath: $basePath"
Write-Host "userDataPath: $userDataPath"
Write-Host "envName: $envName"
Write-Host "logFile: $logFile"

#-----------------------------#
# Install Quarto if Missing
#-----------------------------#

# Source functions
. "$basePath\functions.ps1"

# Define the target Quarto version and default installation path
$QUARTO_VERSION = "1.7.32"
$DEFAULT_QUARTO_INSTALL_DIR = Join-Path $env:ProgramFiles "Quarto"
$QUARTO_DOWNLOAD_URL = "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-win.zip"
$QUARTO_TEMP_ZIP = Join-Path $env:TEMP "quarto.zip"

# Main execution
Write-Host "Checking for Quarto CLI installation..."
    
# Check for existing Quarto installation
$quartoInfo = Find-QuartoInstallation
    
if ($quartoInfo.Found) {
    # Compare Quarto versions
    $versionComparison = Compare-Version -InstalledVersion $quartoInfo.Version -TargetVersion $TargetVersion
    
    if ($versionComparison -eq 0) {
        # Same version
        Write-Host "Quarto CLI v$($quartoInfo.Version) is already installed at $($quartoInfo.Path)"
        if (-not (Test-PathInEnvironment -Directory $quartoInfo.Path)) {
            Write-Host "Adding Quarto bin directory to PATH..."
            Add-ToSystemPath -Directory $quartoInfo.Path
        }
        Write-Host "Quarto CLI is properly installed and configured. No further action needed."
        exit 0
    } elseif ($versionComparison -gt 0) { # Newer version installed
        Write-Host "A newer version of Quarto CLI (v$($quartoInfo.Version)) is installed at $($quartoInfo.Path)"
        if (-not (Test-PathInEnvironment -Directory $quartoInfo.Path)) {
            Write-Host "Adding Quarto bin directory to PATH..."
            Add-ToSystemPath -Directory $quartoInfo.Path
        }
        Write-Host "No installation needed. Using existing newer version."
        exit 0
    } else { # Older version installed
        Write-Host "An older version of Quarto CLI (v$($quartoInfo.Version)) is installed at $($quartoInfo.Path)"
        if (-not (Test-PathInEnvironment -Directory $quartoInfo.Path)) {
            Write-Host "Adding Quarto bin directory to PATH..."
            Add-ToSystemPath -Directory $quartoInfo.Path
        }
        Write-Host "Using existing older version. Check logs for potential incompatibility."
        exit 0
    }
}
    
# Quarto not found, proceed with installation
Write-Host "Quarto CLI is not installed. Proceeding with installation to $DEFAULT_QUARTO_INSTALL_DIR..."
    
# Check for administrative privileges
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Host "This script requires administrative privileges to install Quarto and modify PATH."
    Write-Host "Please run PowerShell as Administrator and try again."
    exit 1
}
    
Install-Quarto -InstallDir $DEFAULT_QUARTO_INSTALL_DIR