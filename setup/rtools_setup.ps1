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
. "$basePath\functions.ps1"

Start-Transcript -Path $logFile -Append -Force | Out-Null
Write-Output "### Rtools 4.5 Version-Specific Setup"

#-----------------------------#
# Discovery & Version Check
#-----------------------------#
try {
    # Specifically look for 4.5
    $foundRtoolsBin = Find-Rtools45Executable
    $foundScope = Get-PathScope -FilePath $foundRtoolsBin
    $needsInstall = $false

    if (-not $foundRtoolsBin) {
        Write-Output "Rtools 4.5 not detected (older versions or no version found)."
        $needsInstall = $true
    }
    elseif ($installScope -eq "allusers" -and $foundScope -eq "currentuser") {
        Write-Output "Found Rtools 4.5 at $foundRtoolsBin (User Scope)."
        Write-Output "System-wide install requested. Proceeding with new installation."
        $needsInstall = $true
    }
    else {
        Write-Output "Valid Rtools 4.5 found: $foundRtoolsBin (Scope: $foundScope)"
        $needsInstall = $false
    }
}
catch {
    Write-Output "Detection failed: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Installation Block
#-----------------------------#
if ($needsInstall) {
    try {
        if ($installScope -eq "allusers") {
            $targetDir = "C:\rtools45"
            $registryTarget = [System.EnvironmentVariableTarget]::Machine
        }
        else {
            $targetDir = Join-Path $env:LOCALAPPDATA "rtools45"
            $registryTarget = [System.EnvironmentVariableTarget]::User
        }
        
        $tempPath = Join-Path $env:TEMP "kiwims_setup"
        if (-not (Test-Path $tempPath)) { New-Item $tempPath -ItemType Directory -Force }
        $installer = Join-Path $tempPath "rtools45.exe"

        # Download Rtools 4.5 specifically
        Invoke-FileDownload "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe" $installer

        if (Test-Path $installer) {
            Write-Output "Download complete. Installing to $targetDir..."
        }
        else {
            Write-Output "Download failed: $($_.Exception.Message)"
            Stop-Transcript
            exit 1
        }

        # Run Installer with /DIR to ensure it goes to our scope-specific path
        $proc = Start-Process -FilePath $installer -ArgumentList "/VERYSILENT", "/DIR=$targetDir", "/NORESTART" -Wait -PassThru  
        if ($proc.ExitCode -ne 0) { throw "Rtools 4.5 installer failed with code $($proc.ExitCode)" }
        
        $foundRtoolsBin = Join-Path $targetDir "usr\bin\make.exe"
    }
    catch {
        Write-Output "Installation failed: $($_.Exception.Message)"
        Stop-Transcript
        exit 1
    }
}

#-----------------------------#
# PATH Update
#-----------------------------#
try {
    if (Test-Path $foundRtoolsBin) {
        $binDir = Split-Path $foundRtoolsBin -Parent
        $registryTarget = if ($installScope -eq "allusers") { [System.EnvironmentVariableTarget]::Machine } else { [System.EnvironmentVariableTarget]::User }

        $currentPath = [Environment]::GetEnvironmentVariable("Path", $registryTarget)
        if ($currentPath -notlike "*$binDir*") {
            [Environment]::SetEnvironmentVariable("Path", "$currentPath;$binDir", $registryTarget)
            Write-Output "Added Rtools 4.5 to $registryTarget PATH."
        }
        
        # Ensure current session sees the NEW path immediately
        $env:Path = "$binDir;$env:Path"
    }
}
catch {
    Write-Output "Path update failed: $($_.Exception.Message)"
    Stop-Transcript
    exit 1
}

Write-Output "Rtools 4.5 setup complete."
exit 0