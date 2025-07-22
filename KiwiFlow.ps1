# KiwiFlow.ps1

# Run to launch KiwiFlow App

#-----------------------------#
# Script Initialization
#-----------------------------#

$condaPrefix = "$env:ProgramData\miniconda3"

# Conda presence check
$condaCmd = "$condaPrefix\Scripts\conda.exe"
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found after installation. Exiting."
    exit 1
}