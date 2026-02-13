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

# Source functions
. "$basePath\functions.ps1"

# Start logging
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Host "### Quarto setup (quarto_install.ps1)"
Write-Host "Target Scope: $installScope"

#-----------------------------#
# Constants & Requirements
#-----------------------------#
$TARGET_VERSION = "1.7.32"
$DOWNLOAD_URL = "https://github.com/quarto-dev/quarto-cli/releases/download/v${TARGET_VERSION}/quarto-${TARGET_VERSION}-win.zip"

# Determine target directory and Registry target
if ($installScope -eq "allusers") {
    $QUARTO_INSTALL_DIR = Join-Path $env:ProgramFiles "Quarto"
    $regTarget = [System.EnvironmentVariableTarget]::Machine
} else {
    $QUARTO_INSTALL_DIR = Join-Path $env:LOCALAPPDATA "Programs\Quarto"
    $regTarget = [System.EnvironmentVariableTarget]::User
}

#-----------------------------#
# Discovery & Decision
#-----------------------------#
try {
    $quarto = Find-QuartoInstallation
    $foundScope = Get-PathScope -FilePath $quarto.Path
    $needsInstall = $false

    if (-not $quarto.Found) {
        Write-Host "Quarto not detected. Installation required."
        $needsInstall = $true
    } else {
        # Version Check
        $vComp = Compare-Version -InstalledVersion $quarto.Version -TargetVersion $TARGET_VERSION
        if ($vComp -lt 0) {
            Write-Host "Found older Quarto ($($quarto.Version)). Upgrade to $TARGET_VERSION required."
            $needsInstall = $true
        }
        # Scope Check
        elseif ($installScope -eq "allusers" -and $foundScope -eq "currentuser") {
            Write-Host "Found Quarto v$($quarto.Version) in User Scope, but System-wide was requested."
            $needsInstall = $true
        }
        else {
            Write-Host "Compatible Quarto v$($quarto.Version) already exists ($foundScope)."
            $needsInstall = $false
        }
    }
}
catch {
    Write-Host "Error during Quarto detection: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Installation Block
#-----------------------------#
if ($needsInstall) {
    try {
        Write-Host "Installing Quarto v$TARGET_VERSION to $QUARTO_INSTALL_DIR..."
        
        # Ensure directory exists
        if (-not (Test-Path $QUARTO_INSTALL_DIR)) {
            New-Item -ItemType Directory -Path $QUARTO_INSTALL_DIR -Force | Out-Null
        }

        $tempZip = Join-Path $env:TEMP "quarto.zip"
        Download-File $DOWNLOAD_URL $tempZip
        
        Write-Host "Extracting archive..."
        Expand-Archive -Path $tempZip -DestinationPath $QUARTO_INSTALL_DIR -Force
        
        # Clean up
        Remove-Item $tempZip -Force
        
        $quartoBin = Join-Path $QUARTO_INSTALL_DIR "bin"
    }
    catch {
        Write-Host "Installation failed: $($_.Exception.Message)"
        Stop-Transcript
        exit 1
    }
} else {
    $quartoBin = $quarto.BinDir
}

#-----------------------------#
# PATH Configuration
#-----------------------------#
try {
    $currentPath = [Environment]::GetEnvironmentVariable("Path", $regTarget)
    if ($currentPath -notlike "*$quartoBin*") {
        $newPath = "$currentPath;$quartoBin"
        [Environment]::SetEnvironmentVariable("Path", $newPath, $regTarget)
        Write-Host "Added Quarto bin to $regTarget PATH."
    }
    
    # Update current session path for immediate use
    $env:Path = "$quartoBin;$env:Path"
}
catch {
    Write-Host "Failed to update PATH: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

Write-Host "Quarto setup complete."
Stop-Transcript
exit 0