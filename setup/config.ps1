#-----------------------------#
# Script Initialization
#-----------------------------#

param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile
)

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Start logging
Start-Transcript -Path $logFile

Write-Host "basePath: $basePath"
Write-Host "userDataPath: $userDataPath"
Write-Host "envName: $envName"
Write-Host "logFile: $logFile"

# Admin check
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "This script must be run as Administrator."
    exit 1
}

# Make userDataPath
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force
    Write-Host "Created Kiwiflow directory: $userDataPath"
}

# Make temp path
$tempPath = Join-Path $env:TEMP "kiwiflow_setup"
if (-not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
    Write-Host "Created temporary directory: $tempPath"
}

# Make report path
try {
    $documentsPath = [System.Environment]::GetFolderPath("MyDocuments")
    $reportPath = Join-Path  $documentsPath "KiwiFlow\report\"
    New-Item -Path $reportPath -ItemType Directory -Force | Out-Null
    Write-Host "Created KiwiwFlow directory in Documents: $reportPath."
}
catch {
    Write-Host "Creating KiwiwFlow directory in Documents failed."
    exit 1
}

# Move report files
try {
    mv "$basePath\app\report\*" $reportPath
    Write-Host "Set up reports directory in Documents."
}
catch {
    Write-Host "Setting up reports directory in Documents failed."
    exit 1
}

Write-Host "Config complete"