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

$basePath = "C:\Users\marian\AppData\Local\KiwiFlow"
$userDataPath = "C:\Users\marian\AppData\Local\KiwiFlow"
$envName = "kiwiflow"
$logFile = "$userDataPath\setup1.log"
$installScope = "currentuser"



# $ErrorActionPreference = "Stop"
# $ProgressPreference = "SilentlyContinue"

# Fallback console output always
Write-Host "=== config.ps1 started ==="
Write-Host "Parameters received:"
Write-Host "  basePath:      $basePath"
Write-Host "  userDataPath:  $userDataPath"
Write-Host "  envName:       $envName"
Write-Host "  logFile:       '$logFile'"
Write-Host "  installScope:  '$installScope'"

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

Write-Host "### Configuring setup (config.ps1)"
Write-Host "basePath:        $basePath"
Write-Host "userDataPath:    $userDataPath"
Write-Host "envName:         $envName"
Write-Host "logFile:         $logFile"
Write-Host "installScope:    $installScope"

# Determine if running elevated
$isElevated = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)

# Only enforce admin check in allusers mode
if ($installScope -eq "allusers") {
    if (-not $isElevated) {
        Write-Host "ERROR: System-wide installation requires administrator rights."
        Write-Host "Please restart the installer as administrator."
        exit 1
    }
    Write-Host "Running elevated → system-wide mode OK"
} else {
    Write-Host "Running in current-user mode (elevation not required)"
}

# Make userDataPath (always user-local, no elevation needed)
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force | Out-Null
    Write-Host "Created KiwiFlow directory: $userDataPath"
}

# Temp path (always user TEMP)
$tempPath = Join-Path $env:TEMP "kiwiflow_setup"
if (-not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
    Write-Host "Created temporary directory: $tempPath"
}

# Report path – use CommonDocuments for allusers, MyDocuments for currentuser
try {
    if ($installScope -eq "allusers") {
        $reportBase = [Environment]::GetFolderPath("CommonDocuments")
    } else {
        $reportBase = [Environment]::GetFolderPath("MyDocuments")
    }
    $reportPath = Join-Path $reportBase "KiwiFlow\report"
    
    if (-not (Test-Path $reportPath)) {
        New-Item -Path $reportPath -ItemType Directory -Force | Out-Null
        Write-Host "Created KiwiFlow report directory: $reportPath"
    }
}
catch {
    Write-Host "Creating report directory failed: $($_.Exception.Message)"
    exit 1
}

# Move report files
try {
    $sourcePath = Join-Path $basePath "app\report\*"
    if (Test-Path $sourcePath) {
        Move-Item -Path $sourcePath -Destination $reportPath -Force -ErrorAction Stop
        Write-Host "Moved report files to: $reportPath"
    } else {
        Write-Host "No report files found at $sourcePath – skipping"
    }
}
catch {
    Write-Host "Moving report files failed: $($_.Exception.Message)"
    exit 1
}

Write-Host "Config complete (scope: $installScope)"
exit 0