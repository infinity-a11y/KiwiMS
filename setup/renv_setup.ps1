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

Write-Output "### Renv environment restore (renv_setup.ps1)"
Write-Output "basePath:         $basePath"
Write-Output "userDataPath:     $userDataPath"
Write-Output "envName:          $envName"
Write-Output "logFile:          $logFile"
Write-Output "installScope:     $installScope"

# Check elevation only in allusers mode
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

if ($installScope -eq "allusers") {
    Write-Output "System-wide mode selected"
    if (-not $isElevated) {
        Write-Output "ERROR: System-wide installation requires administrator rights."
        Write-Output "Please run the installer as administrator."
        Stop-Transcript
        exit 1
    }
} else {
    Write-Output "Current-user mode selected (no elevation required)"
}

# Source functions
. "$basePath\functions.ps1"

# Find Conda executable
$condaCmd = Find-CondaExecutable

# Conda Presence Check
if (-Not (Test-Path $condaCmd)) {
    Write-Output "ERROR: Conda executable not found."
    Write-Output "Make sure the Miniconda installation completed successfully."
    Stop-Transcript
    exit 1
}

Write-Output "Using Conda at: $condaCmd"

#-----------------------------#
# R: renv::restore
#-----------------------------#
$rScriptPath = Join-Path $basePath "setup_renv.R"
$routFile = Join-Path (Split-Path $rScriptPath -Parent) ((Get-Item $rScriptPath).BaseName + ".Rout")

try {
    # Clean up previous .Rout file
    if (Test-Path $routFile) {
        Write-Output "Removing existing R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }

    # Execute renv::restore via Conda environment
    Write-Output "Running renv::restore via: $rScriptPath"
    Write-Output "Command: & '$condaCmd' run -n '$envName' R.exe CMD BATCH --no-save --no-restore --slave '$rScriptPath'"

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
        Write-Output "--- Conda Run Wrapper Output ---"
        Write-Output $condaRunOutput
        Write-Output "---------------------------------"
    }

    # Check exit code
    if ($LASTEXITCODE -ne 0) {
        Write-Output "ERROR: renv::restore failed with exit code $LASTEXITCODE"

        if (Test-Path $routFile) {
            $routContent = Get-Content -Path $routFile -Raw
            Write-Output "--- R Script Output (.Rout) ---"
            Write-Output $routContent
            Write-Output "---------------------------------"
        } else {
            Write-Output "No .Rout file generated at $routFile"
        }

        throw "renv environment restoration failed"
    }

    # Success logging
    if (Test-Path $routFile) {
        $routContent = Get-Content -Path $routFile -Raw
        Write-Output "--- R Script Output (.Rout) ---"
        Write-Output $routContent
        Write-Output "---------------------------------"
    } else {
        Write-Output "Note: No .Rout file generated, but command reported success."
    }

    Write-Output "renv::restore completed successfully."
}
catch {
    Write-Output "FATAL ERROR during renv::restore."
    Write-Output "Exception: $($_.Exception.Message)"
    if ($_.Exception.InnerException) {
        Write-Output "Inner Exception: $($_.Exception.InnerException.Message)"
    }

    if (Test-Path $routFile) {
        $routContent = Get-Content -Path $routFile -Raw
        Write-Output "--- R Script Output (.Rout) ---"
        Write-Output $routContent
        Write-Output "---------------------------------"
    }

    Stop-Transcript
    exit 1
}
finally {
    # Clean up .Rout file
    if (Test-Path $routFile) {
        Write-Output "Cleaning up R output file: $routFile"
        Remove-Item $routFile -Force -ErrorAction SilentlyContinue | Out-Null
    }
}

Write-Output "renv environment setup finished."
exit 0