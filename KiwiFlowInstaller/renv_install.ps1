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
# R: install renv
#-----------------------------#
Write-Host "Restoring R packages with renv..."
try {
    & $condaCmd run -n kiwiflow R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
    Write-Host "Installation of 'renv' completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
        Write-Error "Installation of 'renv' completed."
    }
    catch {
        Write-Error "Failed to run R command for 'renv' installation. Error: $($_.Exception.Message)"
        exit 1        
    }
}