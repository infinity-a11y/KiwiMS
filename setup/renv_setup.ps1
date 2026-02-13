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

Write-Host "### Renv environment restore (renv_setup.ps1)"
Write-Host "basePath:         $basePath"
Write-Host "userDataPath:     $userDataPath"
Write-Host "envName:          $envName"
Write-Host "logFile:          $logFile"
Write-Host "installScope:     $installScope"

# Check elevation only in allusers mode
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

# Find Conda executable
$condaCmd = Find-CondaExecutable

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Host "ERROR: Conda executable not found."
    Write-Host "Make sure the Miniconda installation completed successfully."
    Stop-Transcript
    exit 1
}

Write-Host "Using Conda at: $condaCmd"

#-----------------------------#
# R: renv::restore
#-----------------------------#
$rScriptPath = Join-Path $basePath "setup_renv.R"
$routFile = Join-Path (Split-Path $rScriptPath -Parent) ((Get-Item $rScriptPath).BaseName + ".Rout")

try {
    # Clean up previous .Rout file
    if (Test-Path $routFile) {
        Write-Host "Removing existing R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }

    # Execute renv::restore via Conda environment
    Write-Host "Running renv::restore via: $rScriptPath"
    Write-Host "Command: & '$condaCmd' run -n '$envName' R.exe CMD BATCH --no-save --no-restore --slave '$rScriptPath'"

    # Get the folder where setup_renv.R is located
    $workDir = Split-Path -Path $rScriptPath -Parent

        # Use Set-Location to ensure 'conda run' inherits the correct path
    Push-Location $workDir
    try {
        $condaRunOutput = & $condaCmd run -n $envName R.exe CMD BATCH "--no-save" "--no-restore" "--slave" "$rScriptPath" 2>&1 | Out-String
    }
    finally {
        Pop-Location
    }

    # Log wrapper output if any
    if ($condaRunOutput.Trim() -ne "") {
        Write-Host "--- Conda Run Wrapper Output ---"
        Write-Host $condaRunOutput
        Write-Host "---------------------------------"
    }

    # Check exit code
    if ($LASTEXITCODE -ne 0) {
        Write-Host "ERROR: renv::restore failed with exit code $LASTEXITCODE"

        if (Test-Path $routFile) {
            $routContent = Get-Content -Path $routFile -Raw
            Write-Host "--- R Script Output (.Rout) ---"
            Write-Host $routContent
            Write-Host "---------------------------------"
        } else {
            Write-Host "No .Rout file generated at $routFile"
        }

        throw "renv environment restoration failed"
    }

    # Success logging
    if (Test-Path $routFile) {
        $routContent = Get-Content -Path $routFile -Raw
        Write-Host "--- R Script Output (.Rout) ---"
        Write-Host $routContent
        Write-Host "---------------------------------"
    } else {
        Write-Host "Note: No .Rout file generated, but command reported success."
    }

    Write-Host "renv::restore completed successfully."
}
catch {
    Write-Host "FATAL ERROR during renv::restore."
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
    # Clean up .Rout file
    if (Test-Path $routFile) {
        Write-Host "Cleaning up R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }
}

Write-Host "renv environment setup finished."
exit 0