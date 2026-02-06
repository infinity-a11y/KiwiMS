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

Write-Host "### Miniconda setup (miniconda_installer.ps1)"
Write-Host "basePath:         $basePath"
Write-Host "userDataPath:     $userDataPath"
Write-Host "envName:          $envName"
Write-Host "logFile:          $logFile"
Write-Host "installScope:     $installScope"

#-----------------------------#
# Install Miniconda if Missing
#-----------------------------#

# Source functions
. "$basePath\functions.ps1"

$condaCmd = Find-CondaExecutable
$foundScope = Get-CondaScope -CondaPath $condaCmd

if (-not $condaCmd) {
    $needsInstall = $true
} elseif ($installScope -eq "allusers" -and $foundScope -eq "currentuser") {
    Write-Host "Found Conda at: $condaCmd"
    Write-Host "Conflict: System-wide install requested, but existing Conda is User-specific."
    Write-Host "A new system-wide Miniconda will be installed."
    $needsInstall = $true
} else {
    Write-Host "Compatible Conda found: $condaCmd (Scope: $foundScope)"
    $needsInstall = $false
}

try {
    if ($needsInstall) {
        # Decide paths based on scope      
        if ($installScope -eq "allusers") {
            Write-Host "System-wide (all users) mode selected"
            $condaPrefix = "$env:ProgramData\miniconda3"
        } else {
            Write-Host "Current-user mode selected (no elevation required)"
            $condaPrefix = "$env:LOCALAPPDATA\miniconda3"
        }

        Write-Host "Conda executable not found. Installing Miniconda in $condaPrefix ..."

        # Download installer
        $tempPath = "$env:TEMP\kiwiflow_setup"
        $minicondaInstaller = "$tempPath\miniconda.exe"
        Download-File "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe" $minicondaInstaller
                
        # Install miniconda
        Start-Process -Wait -FilePath $minicondaInstaller -ArgumentList "/S", "/D=$condaPrefix"
        $env:Path += ";$env:ProgramData\Miniconda3;$env:ProgramData\Miniconda3\Scripts;$env:ProgramData\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installation completed."   

        # Conda Presence Check
        Write-Host "Checking for Conda executable at $condaCmd..."
            if (-Not (Test-Path $condaCmd)) {
                Write-Host "Miniconda not found after installation. Exiting."
                exit 1
            }

        Write-Host "Conda executable found."
    }
}
catch {
    Write-Host "Miniconda installation failed. Exiting."
    exit 1
}

if ($LASTEXITCODE -ne 0) { exit 1 }