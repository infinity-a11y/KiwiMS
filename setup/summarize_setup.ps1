#-----------------------------#
# Script Initialization
#-----------------------------#

# param(
#     [string]$basePath,
#     [string]$userDataPath,
#     [string]$envName,
#     [string]$logFile
# )

# $ErrorActionPreference = "Stop"
# $ProgressPreference = "SilentlyContinue"

# # Start logging
# Start-Transcript -Path $logFile -Append | Out-Null

# Write-Host "### Environment summary (summarize_setup.ps1)"

# # Show Path environment with newly installed programs
# Write-Host "***Path Environment***"
# ($env:Path -split ';') | ForEach-Object { Write-Host $_ }
# Write-Host "======================"

# # Show versions of conda and quarto dependencies
# try {
#     Write-Host "Conda version: $(conda --version)"
#     Write-Host "Quarto version: $(quarto --version)"
# }
# catch {
#     Write-Host "Fetching dependency version failed. Error details: $($_.Exception.Message)"
# }