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
    Write-Host "### Starting fresh log at $(Get-Date)"
}
catch {
    Write-Error "Failed to initialize logging: "
    Stop-Transcript
    exit 1
}

try {
    Write-Host "### Configuring setup (config.ps1)"
    Write-Host "basePath:     $basePath"
    Write-Host "userDataPath: $userDataPath"
    Write-Host "envName:      $envName"
    Write-Host "logFile:      $logFile"
    Write-Host "installScope: $installScope"
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
            Write-Host "ERROR: System-wide installation requires administrator rights."
            Stop-Transcript
            exit 1
        }
        Write-Host "Running elevated → system-wide mode OK"
    }
    else {
        Write-Host "Running in current-user mode (elevation not required)"
    }
}
catch {
    Write-Host "Privilege check failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Create User Data Directory
#-----------------------------#
try {
    if (-not (Test-Path $userDataPath)) {
        New-Item -ItemType Directory -Path $userDataPath -Force | Out-Null
        Write-Host "Created KiwiMS directory: $userDataPath"
    }
}
catch {
    Write-Host "Creating User Data directory failed: "
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
        Write-Host "Created temporary directory: $tempPath"
    }
}
catch {
    Write-Host "Creating temporary directory failed: "
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
        Write-Host "Created KiwiMS report directory: $reportPath"
    }
}
catch {
    Write-Host "Defining/Creating report directory failed: "
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
        Write-Host "Moved report files to: $reportPath"
    }
    else {
        Write-Host "No report files found at $sourcePath. Skipping ..."
    }
}
catch {
    Write-Host "Moving report files failed: "
    Stop-Transcript
    exit 1
}

#-----------------------------#
# Finalize Configuration
#-----------------------------#
try {
    Write-Host "Config complete"
    exit 0
}
catch {
    exit 0
}