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

# Start logging transcript to the specified log file
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Rtools setup (rtools_setup.ps1)"

Write-Host "basePath: $basePath"
Write-Host "userDataPath: $userDataPath"
Write-Host "envName: $envName"
Write-Host "logFile: $logFile"

# Source functions
. "$basePath\functions.ps1"

#-----------------------------#
# Ensure Rtools
#-----------------------------#

$tempPath = Join-Path $env:TEMP "kiwiflow_setup"
# Ensure temporary directory exists for downloads
if (-Not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
    Write-Host "Created temporary directory: $tempPath"
}

$rtoolsPath = "C:\rtools45"

try {
    Write-Host "Checking for existing Rtools installation at $rtoolsPath..."
    if (-Not (Test-Path $rtoolsPath)) {

        # Download
        Write-Host "Rtools not found."
        $tempPath = "$env:TEMP\kiwiflow_setup"
        $rtoolsInstaller = "$tempPath\rtools.exe"
        Write-Host "Downloading Rtools..."
        Download-File "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6691-6492.exe" $rtoolsInstaller

        # Installation
        Write-Host "Installing Rtools to $rtoolsPath..."
        $process = Start-Process -Wait -FilePath $rtoolsInstaller -ArgumentList "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART" -PassThru -WindowStyle Hidden
        if ($process.ExitCode -ne 0) {
            Write-Host "ERROR: Rtools installation failed with exit code $($process.ExitCode)."
            exit 1
        }
        else {
            Write-Host "Rtools installation completed."
        }

        # Path environment check
        Find-RtoolsExecutable $rtoolsPath
    }
    else {
        Write-Host "Rtools already installed at $rtoolsPath. Skipping installation."

        # Path environment check
        Find-RtoolsExecutable $rtoolsPath
    }
}
catch {
    Write-Host "Failed to ensure Rtools installation. Error: $($_.Exception.Message)"
    exit 1
}

if ($LASTEXITCODE -ne 0) { exit 1 }