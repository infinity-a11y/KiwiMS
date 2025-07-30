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
Start-Transcript -Path $logFile

# Show Path environment with newly installed programs
Write-Host "***Path Environment***"
($env:Path -split ';') | ForEach-Object { Write-Host $_ }
Write-Host "======================"