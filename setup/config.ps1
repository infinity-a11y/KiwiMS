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

#-----------------------------#
# Start Logging
#-----------------------------#
try {
    if (Test-Path $logFile) { 
        Remove-Item $logFile -Force -ErrorAction SilentlyContinue
    }
    
    Start-Transcript -Path $logFile -Force | Out-Null
    Write-Output "### Starting fresh log at $(Get-Date)"
}
catch {
    Write-Error "Failed to initialize logging: "
    Stop-Transcript
    exit 1
}

try {
    Write-Output "### Configuring setup (config.ps1)"
    Write-Output "basePath:     $basePath"
    Write-Output "userDataPath: $userDataPath"
    Write-Output "envName:      $envName"
    Write-Output "logFile:      $logFile"
    Write-Output "installScope: $installScope"
}
catch {
    Write-Error "Failed to initialize logging: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Administrator Rights Check
#-----------------------------#
try {
    $isElevated = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

    if ($installScope -eq "allusers") {
        if (-not $isElevated) {
            Write-Output "ERROR: System-wide installation requires administrator rights."
            Stop-Transcript
            exit 1
        }
        Write-Output "Running elevated → system-wide mode OK"
    }
    else {
        Write-Output "Running in current-user mode (elevation not required)"
    }
}
catch {
    Write-Output "Privilege check failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Create User Data Directory
#-----------------------------#
try {
    if (-not (Test-Path $userDataPath)) {
        New-Item -ItemType Directory -Path $userDataPath -Force | Out-Null
        Write-Output "Created KiwiMS directory: $userDataPath"
    }
}
catch {
    Write-Output "Creating User Data directory failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Finalize Configuration
#-----------------------------#
try {
    Write-Output "Config complete"
    exit 0
}
catch {
    exit 0
}