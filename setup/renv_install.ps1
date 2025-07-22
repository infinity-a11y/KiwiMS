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

# Source functions
. "$basePath\functions.ps1"
$condaCmd = Find-CondaExecutable

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found. Exiting."
    exit 1
}

#-----------------------------#
# R: install.packages("renv")
#-----------------------------#
$rScriptPath = Join-Path $basePath "install_renv.R"
$routFile = Join-Path (Split-Path $rScriptPath -Parent) ((Get-Item $rScriptPath).BaseName + ".Rout")

try {
    # Clean up any previous .Rout file before running
    if (Test-Path $routFile) {
        Write-Host "Removing existing R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }

    # Execute the R script
    Write-Host "Running: & '$condaCmd' run -n '$envName' R.exe CMD BATCH --no-save --no-restore --slave '$rScriptPath'"
    $condaRunWrapperOutput = & $condaCmd run -n $envName R.exe CMD BATCH "--no-save" "--no-restore" "--slave" "$rScriptPath" 2>&1 | Out-String
    
    # Log output from conda run wrapper
    if ($condaRunWrapperOutput.Trim() -ne "") {
        Write-Host "--- Conda Run Wrapper Output ---"
        Write-Host "$condaRunWrapperOutput"
        Write-Host "-----------------------------------------"
    }

    # Check exit code
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: R script '$rScriptPath' failed with exit code $LASTEXITCODE."
        if (Test-Path $routFile) {
            $rScriptDetailedOutput = Get-Content -Path $routFile | Out-String
            Write-Host "--- R Script's Detailed Output (.Rout file content) ---"
            Write-Host "$rScriptDetailedOutput"
            Write-Host "-----------------------------------------------------"
        }
        else {
            Write-Host "WARNING: R output file (.Rout) was not generated at '$routFile' despite error exit code."
        }
        throw "R package installation failed. Check log for details and the .Rout file content."
    }
    
    # Read and log content of .Rout file
    if (Test-Path $routFile) {
        $rScriptDetailedOutput = Get-Content -Path $routFile | Out-String
        Write-Host "--- R Script's Detailed Output (.Rout file content) ---"
        Write-Host "$rScriptDetailedOutput"
        Write-Host "-----------------------------------------------------"
    }
    else {
        Write-Host "WARNING: R output file (.Rout) was not generated at '$routFile', but R script reported success."
    }

    Write-Host "renv package installation completed successfully."
}
catch {
    Write-Host "ERROR: Failed to execute R installation script."
    Write-Host "Error details: $($_.Exception.Message)"
    if ($_.Exception.InnerException) {
        Write-Host "Inner Exception: $($_.Exception.InnerException.Message)"
    }
    Write-Host "Full error record: $($_.Exception | Format-List -Force)"

    # Read and log content of .Rout file
    if (Test-Path $routFile) {
        $rScriptDetailedOutput = Get-Content -Path $routFile | Out-String
        Write-Host "--- R Script's Detailed Output (.Rout file content) ---"
        Write-Host "$rScriptDetailedOutput"
        Write-Host "-----------------------------------------------------"
    }
    else {
        Write-Host "WARNING: R output file (.Rout) was not generated at '$routFile'."
    }
    exit 1
}
finally {
    # Clean up .Rout file
    if (Test-Path $routFile) {
        Write-Host "Cleaning up R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }
}