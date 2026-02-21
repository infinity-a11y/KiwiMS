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
# Create Temporary Directory
#-----------------------------#
try {
    $tempPath = Join-Path $env:TEMP "kiwims_setup"
    if (-not (Test-Path $tempPath)) {
        New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
        Write-Output "Created temporary directory: $tempPath"
    }
}
catch {
    Write-Output "Creating temporary directory failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Determine Report Path
#-----------------------------#
try {
    if ($installScope -eq "allusers") {
        $reportBase = [Environment]::GetFolderPath("CommonDocuments")
    }
    else {
        $reportBase = [Environment]::GetFolderPath("MyDocuments")
    }
    $reportPath = Join-Path $reportBase "KiwiMS\report"
    
    if (-not (Test-Path $reportPath)) {
        New-Item -Path $reportPath -ItemType Directory -Force | Out-Null
        Write-Output "Created KiwiMS report directory: $reportPath"
    }
}
catch {
    Write-Output "Defining/Creating report directory failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Move Report Files
#-----------------------------#
try {
    $sourcePath = Join-Path $basePath "app\report\*"
    if (Test-Path $sourcePath) {
        Move-Item -Path $sourcePath -Destination $reportPath -Force -ErrorAction Stop
        Write-Output "Moved report files to: $reportPath"
    }
    else {
        Write-Output "No report files found at $sourcePath. Skipping ..."
    }
}
catch {
    Write-Output "Moving report files failed: "
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