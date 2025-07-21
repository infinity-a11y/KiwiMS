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
# R: renv::restore
#-----------------------------#
Write-Host "Setting up R packages"
try {
    & $condaCmd run -n kiwiflow R.exe -e "renv::restore()"
    Write-Host "Setting up R packages completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "renv::restore()"
        Write-Host "Setting up R packages completed."
    }
    catch {
        Write-Host "Failed to run R commands for 'renv'. Error: $($_.Exception.Message)"
        pause
        Stop-Transcript
        exit 1
    }
}