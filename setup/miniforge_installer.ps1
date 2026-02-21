#-----------------------------#
# Script Initialization
#-----------------------------#

param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

Write-Output "### Miniconda setup (miniforge_installer.ps1)"
Write-Output "basePath:         $basePath"
Write-Output "userDataPath:     $userDataPath"
Write-Output "envName:          $envName"
Write-Output "logFile:          $logFile"
Write-Output "installScope:     $installScope"

# Source helper functions
. "$basePath\functions.ps1"

#-----------------------------#
# Logic: Check for existing Conda
#-----------------------------#

$condaCmd = Find-CondaExecutable
$foundScope = Get-CondaScope -CondaPath $condaCmd

$needsInstall = $false

if (-not $condaCmd) {
    Write-Output "No existing Conda found. Proceeding with installation."
    $needsInstall = $true
}
elseif ($installScope -eq "allusers" -and $foundScope -eq "currentuser") {
    Write-Output "Conflict: System-wide install requested, but found only User-specific Conda."
    $needsInstall = $true
}
else {
    Write-Output "Compatible Conda found: $condaCmd (Scope: $foundScope)"
    $needsInstall = $false
}

#-----------------------------#
# Installation Block
#-----------------------------#

try {
    if ($needsInstall) {
        # Define installation prefix based on scope
        if ($installScope -eq "allusers") {
            Write-Output "Targeting System-wide directory (All Users)."
            $condaPrefix = "$env:ProgramData\miniconda3"
            $installType = "AllUsers"
        }
        else {
            Write-Output "Targeting Local AppData directory (Current User)."
            $condaPrefix = "$env:LOCALAPPDATA\miniconda3"
            $installType = "JustMe"
        }

        # Prepare Temp Directory
        $tempDir = Join-Path $env:TEMP "kiwims_setup"
        if (-not (Test-Path $tempDir)) { New-Item -ItemType Directory -Path $tempDir | Out-Null }
        
        $minicondaInstaller = Join-Path $tempDir "miniconda.exe"

        # Download Miniconda
        Write-Output "Downloading Miniconda installer..."
        Download-File "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe" $minicondaInstaller

        if (-not (Test-Path $minicondaInstaller)) {
            throw "Miniconda installer download failed."
        }

        # Execute Installation
        # We use /InstallationType to be explicit and avoid UI hangs
        Write-Output "Starting silent installation to: $condaPrefix"
        $args = @("/S", "/InstallationType=$installType", "/RegisterPython=0", "/D=$condaPrefix")
        
        $process = Start-Process -FilePath $minicondaInstaller -ArgumentList $args -Wait -PassThru

        # IMMEDIATE CHECK of the installer result
        if ($process.ExitCode -ne 0) {
            Write-Output "ERROR: Miniconda installer exited with error code: $($process.ExitCode)"
            Stop-Transcript
            exit 1
        }
        
        # Reset LASTEXITCODE so it doesn't pollute the final script status
        $global:LASTEXITCODE = 0
        Write-Output "Miniconda installation executable finished successfully."

        # Cleanup installer to save space
        Remove-Item $minicondaInstaller -Force -ErrorAction SilentlyContinue

        # Verify Installation Path
        # Miniconda structure: Conda is usually in Scripts\conda.exe
        $condaCmd = Join-Path $condaPrefix "Scripts\conda.exe"
        
        if (-not (Test-Path $condaCmd)) {
            # Fallback check for different versions
            $condaCmd = Join-Path $condaPrefix "condabin\conda.bat"
        }

        if (-not (Test-Path $condaCmd)) {
            throw "Miniconda installed but executable not found at $condaCmd"
        }

        # Update Path for the current process (so next scripts can find it)
        $newPaths = "$condaPrefix;$condaPrefix\Scripts;$condaPrefix\Library\bin"
        $env:Path = $newPaths + ";" + $env:Path
        
        # Persist Path for the user/system
        $target = if ($installScope -eq "allusers") { [System.EnvironmentVariableTarget]::Machine } else { [System.EnvironmentVariableTarget]::User }
        try {
            # Note: We append to avoid overwriting existing system paths
            $currentPath = [Environment]::GetEnvironmentVariable("Path", $target)
            if ($currentPath -notlike "*$condaPrefix*") {
                [Environment]::SetEnvironmentVariable("Path", $currentPath + ";" + $newPaths, $target)
            }
        }
        catch {
            Write-Output "Warning: Could not update persistent PATH variable. Continuing..."
        }

        Write-Output "Miniconda setup complete."
    }
    else {
        Write-Output "Skipping Miniconda installation as a compatible version is already present."
    }
}
catch {
    Write-Output "CRITICAL ERROR during Miniconda setup: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

# Final Exit
Write-Output "miniforge_installer.ps1 finished."
Stop-Transcript
exit 0