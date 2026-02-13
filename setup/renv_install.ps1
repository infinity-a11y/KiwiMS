#-----------------------------#
# Script Initialization
#-----------------------------#

param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope = "currentuser"
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Renv setup (renv_install.ps1)"
Write-Host "basePath:         $basePath"
Write-Host "userDataPath:     $userDataPath"
Write-Host "envName:          $envName"
Write-Host "logFile:          $logFile"
Write-Host "installScope:     $installScope"

# Determine if running elevated (only relevant for allusers mode)
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if ($installScope -eq "allusers") {
    Write-Host "System-wide mode selected"
    if (-not $isElevated) {
        Write-Host "ERROR: System-wide installation requires administrator rights."
        Write-Host "Please run the installer as administrator."
        Stop-Transcript
        exit 1
    }
} else {
    Write-Host "Current-user mode selected (no elevation required)"
}

# Source functions
. "$basePath\functions.ps1"

# Find Conda executable (path depends on scope)
$condaCmd = Find-CondaExecutable

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "ERROR: Conda not found at expected location."
    Write-Host "Make sure the Miniconda installation step completed successfully."
    Stop-Transcript
    exit 1
}

Write-Host "Using Conda at: $condaCmd"

#-----------------------------#
# Check if renv is present
#-----------------------------#
Write-Host "Checking if renv is already installed in environment '$envName'..."

$oldPreference = $ErrorActionPreference
$ErrorActionPreference = "Continue" 

try {
    $checkRenv = & $condaCmd run -n $envName Rscript -e "if(!requireNamespace('renv', quietly=TRUE)) quit(status=1)" 2>&1
    $exitCode = $LASTEXITCODE
}
finally {
    # Restore the global 'Stop' preference
    $ErrorActionPreference = $oldPreference
}

if ($exitCode -eq 0) {
    Write-Host "renv is already installed. Skipping installation step."
    Stop-Transcript
    exit 0
}

# Reset LASTEXITCODE manually so it doesn't haunt the end of the script
$global:LASTEXITCODE = 0
Write-Host "renv not found. Proceeding with installation..."

#-----------------------------#
# R: install.packages("renv")
#-----------------------------#
$rScriptPath = Join-Path $basePath "install_renv.R"
$routFile = Join-Path (Split-Path $rScriptPath -Parent) ((Get-Item $rScriptPath).BaseName + ".Rout")

try {
    # Clean up any previous .Rout file
    if (Test-Path $routFile) {
        Write-Host "Removing existing R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }

    # Execute the R script via Conda environment
    Write-Host "Running R script to install renv: $rScriptPath"
    Write-Host "Command: & '$condaCmd' run -n '$envName' R.exe CMD BATCH --no-save --no-restore --slave '$rScriptPath'"

    $condaRunOutput = & $condaCmd run -n $envName R.exe CMD BATCH "--no-save" "--no-restore" "--slave" "$rScriptPath" 2>&1 | Out-String

    # Log wrapper output
    if ($condaRunOutput.Trim() -ne "") {
        Write-Host "--- Conda Run Output ---"
        Write-Host $condaRunOutput
        Write-Host "-------------------------"
    }

    # Check exit code
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: R script failed with exit code $LASTEXITCODE"
        
        if (Test-Path $routFile) {
            $routContent = Get-Content -Path $routFile -Raw
            Write-Host "--- R Script Output (.Rout) ---"
            Write-Host $routContent
            Write-Host "---------------------------------"
        } else {
            Write-Host "No .Rout file generated at $routFile"
        }
        
        throw "renv package installation failed"
    }

    # Success logging
    if (Test-Path $routFile) {
        $routContent = Get-Content -Path $routFile -Raw
        Write-Host "--- R Script Output (.Rout) ---"
        Write-Host $routContent
        Write-Host "---------------------------------"
    }

    Write-Host "renv package installation completed successfully."
}
catch {
    Write-Host "ERROR: Failed to install renv package."
    Write-Host "Exception: $($_.Exception.Message)"
    if ($_.Exception.InnerException) {
        Write-Host "Inner Exception: $($_.Exception.InnerException.Message)"
    }

    if (Test-Path $routFile) {
        $routContent = Get-Content -Path $routFile -Raw
        Write-Host "--- R Script Output (.Rout) ---"
        Write-Host $routContent
        Write-Host "---------------------------------"
    }

    Stop-Transcript
    exit 1
}
finally {
    # Cleanup .Rout file
    if (Test-Path $routFile) {
        Write-Host "Cleaning up R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }
}

Write-Host "renv installation finished."
exit 0