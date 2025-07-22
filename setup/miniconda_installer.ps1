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

$condaPrefix = "$env:ProgramData\miniconda3"

#-----------------------------#
# FUNCTION Download with Retry
#-----------------------------#
function Download-File($url, $destination) {
    if (Test-Path $destination) {
        Remove-Item $destination -Force
    }

    $success = $false
    for ($i = 0; $i -lt 3; $i++) {
        try {
            Invoke-WebRequest -Uri $url -OutFile $destination -UseBasicParsing
            $success = $true
            break
        }
        catch {
            Start-Sleep -Seconds 3
        }
    }

    if (-Not $success) {
        Write-Host "Failed to download: $url"
        exit 1
    }
}

#-----------------------------#
# Install Miniconda if Missing
#-----------------------------#
try {
    if (-Not (Test-Path "$condaPrefix\Scripts\conda.exe")) {
        Write-Host "Installing Miniconda..."

        # Download installer
        $tempPath = "$env:TEMP\kiwiflow_setup"
        $minicondaInstaller = "$tempPath\miniconda.exe"
        Download-File "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe" $minicondaInstaller
        
        # Install miniconda
        Start-Process -Wait -FilePath $minicondaInstaller -ArgumentList "/S", "/D=$condaPrefix"
        $env:Path += ";$env:ProgramData\Miniconda3;$env:ProgramData\Miniconda3\Scripts;$env:ProgramData\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installation completed."   
    } else {
        Write-Host "Miniconda already present."
    }
}
catch {
    Write-Host "Miniconda installation failed. Exiting."
    exit 1
}

#-----------------------------#
# Conda Presence Check
#-----------------------------#
$condaCmd = "$condaPrefix\Scripts\conda.exe"
Write-Host "Checking for Conda executable at $condaCmd..."
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Miniconda not found after installation. Exiting."
    exit 1
}

Write-Host "Conda executable found."