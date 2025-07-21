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
# Conda Presence Check
#-----------------------------#
$condaCmd = "$condaPrefix\Scripts\conda.exe"
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found after installation. Exiting."
    exit 1
}

#-----------------------------#
# R: install reticulate
#-----------------------------#
Write-Host "Setting up reticulate"
try {
    & $condaCmd run -n kiwiflow R.exe -e "renv::install('reticulate')"
    Write-Host "Setting up reticulate completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "renv::install('reticulate')"
        Write-Host "Setting up reticulate completed."
    }
    catch {
        Write-Host "Failed to setup reticulate. Error: $($_.Exception.Message)"
        exit 1
    }
}