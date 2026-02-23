#-----------------------------#
# 1. Script Initialization
#-----------------------------#
param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue"
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Output "=========================================="
Write-Output "   Deconvolution Functional Test          "
Write-Output "=========================================="

# Source functions
. "$basePath\functions.ps1"

# Find Conda executable
$condaCmd = Find-CondaExecutable

#-----------------------------#
# 2. Setup Test Environment
#-----------------------------#
# Create a unique directory inside the system temp folder
$guid = [guid]::NewGuid().ToString().Substring(0, 8)
$testTempDir = Join-Path ([System.IO.Path]::GetTempPath()) "KiwiMS_Test_$guid"

Write-Output "Creating temporary test directory: $testTempDir"
New-Item -ItemType Directory -Path $testTempDir -Force | Out-Null

try {
    # Generate config
    Write-Output "Writing configuration file ..."
    & $condaCmd run -n $envName Rscript.exe "$basePath\make_config.R" $basePath

    # Copy configuration to the specific test directory
    if (Test-Path "$basePath\resources\config.rds") {
        Copy-Item -Path "$basePath\resources\config.rds" -Destination (Join-Path $testTempDir "config.rds")
    }
    else {
        Write-Output "Warning: config.rds not found in current directory."
    }

    #-----------------------------#
    # 3. Execution
    #-----------------------------#
    Write-Output "Executing R deconvolution logic..."
    
    # We pass the specific $testTempDir to the R script
    & $condaCmd run -n $envName --no-capture-output Rscript.exe "$basePath\app\logic\deconvolution_execute.R" $testTempDir $testTempDir $basePath $testTempDir "testing"

    if ($LASTEXITCODE -ne 0) {
        Write-Output "R script failed with exit code $LASTEXITCODE"
    }
}
catch {
    Write-Output "An error occurred during the test: $($_.Exception.Message)"
}
finally {
    #-----------------------------#
    # 4. Cleanup
    #-----------------------------#
    if (Test-Path $testTempDir) {
        Write-Output "Cleaning up test directory..."
        # Sleep briefly to ensure R has released file handles
        Start-Sleep -Seconds 1 
        Remove-Item -Path $testTempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
    
    Write-Output "Deconvolution test complete."
    Stop-Transcript
}