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

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "   Deconvolution Functional Test          " -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan

# Source functions
. "$basePath\functions.ps1"

# Find Conda executable
$condaCmd = Find-CondaExecutable

#-----------------------------#
# 2. Setup Test Environment
#-----------------------------#
# Create a unique directory inside the system temp folder
$guid = [guid]::NewGuid().ToString().Substring(0,8)
$testTempDir = Join-Path ([System.IO.Path]::GetTempPath()) "KiwiMS_Test_$guid"

Write-Host "Creating temporary test directory: $testTempDir" -ForegroundColor Gray
New-Item -ItemType Directory -Path $testTempDir -Force | Out-Null

try {
    # Generate config 
    Write-Host "Writing configuration file ..."
    & $condaCmd run -n $envName Rscript.exe "make_config.R" $basePath

    # Copy configuration to the specific test directory
    if (Test-Path "resources\config.rds") {
        Copy-Item -Path "resources\config.rds" -Destination (Join-Path $testTempDir "config.rds")
    } else {
        Write-Host "Warning: config.rds not found in current directory." -ForegroundColor Yellow
    }

    #-----------------------------#
    # 3. Execution
    #-----------------------------#
    Write-Host "Executing R deconvolution logic..." -ForegroundColor Yellow
    
    # We pass the specific $testTempDir to the R script
    & $condaCmd run -n $envName --no-capture-output Rscript.exe "app\logic\deconvolution_execute.R" $testTempDir $testTempDir $pwd.Path $testTempDir "testing"

    if ($LASTEXITCODE -ne 0) {
        Write-Host "R script failed with exit code $LASTEXITCODE" -ForegroundColor Red
    }
}
catch {
    Write-Host "An error occurred during the test: $($_.Exception.Message)" -ForegroundColor Red
}
finally {
    #-----------------------------#
    # 4. Cleanup
    #-----------------------------#
    if (Test-Path $testTempDir) {
        Write-Host "Cleaning up test directory..." -ForegroundColor Gray
        # Sleep briefly to ensure R has released file handles
        Start-Sleep -Seconds 1 
        Remove-Item -Path $testTempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
    
    Write-Host "Deconvolution test complete."
    Stop-Transcript
}