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
# Ensure Rtools
#-----------------------------#
$rtoolsPath = "C:\rtools44"
$rtoolsBinPath = Join-Path $rtoolsPath "usr\bin"

try {
    Write-Host "Checking for existing Rtools installation at $rtoolsPath..."
    if (-Not (Test-Path $rtoolsPath)) {

        # Download
        Write-Host "Rtools not found."
        $tempPath = "$env:TEMP\kiwiflow_setup"
        $rtoolsInstaller = "$tempPath\rtools.exe"
        Write-Host "Downloading Rtools..."
        Download-File "https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe" $rtoolsInstaller
        
        # Installation
        Write-Host "Installing Rtools to $rtoolsPath..."
        $process = Start-Process -Wait -FilePath $rtoolsInstaller -ArgumentList "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART" -PassThru -WindowStyle Hidden
        if ($process.ExitCode -ne 0) {
            Write-Host "ERROR: Rtools installation failed with exit code $($process.ExitCode)."
            exit 1
        }
        
        # Path environment check
        #Start-Process -Wait -FilePath $rtoolsInstaller -ArgumentList "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART"
        Write-Host "Checking if $($rtoolsBinPath) is in PATH..."
        if (-not ($env:PATH -like "*$($rtoolsPath )\usr\bin*")) {
            $env:PATH = "$rtoolsPath \usr\bin;" + $env:PATH
            Write-Host "Added $($rtoolsPath )\usr\bin to PATH."
        } else {
            Write-Host "$($rtoolsBinPath) already in PATH."
        }

        Write-Host "Rtools installation completed."
    }
    else {
        Write-Host "Rtools already installed at $rtoolsPath. Skipping installation."
    }
}
catch {
    Write-Host "Failed to ensure Rtools installation. Error: $($_.Exception.Message)"
    Exit 1
}