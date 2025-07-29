# update.ps1
# Fetch and run KiwiFlow update 

# Log update script
$logFile = "$env:LOCALAPPDATA\KiwiFlow\update.log"
Start-Transcript -Path $logFile

# Declare download function
function Download-File($url, $destination) {
    if (Test-Path $destination) {
        Remove-Item $destination -Force
    }

    $success = $false
    for ($i = 0; $i -lt 3; $i++) {
        try {
            Invoke-WebRequest -Uri $url -OutFile $destination -UseBasicParsing
            $success = $true
            break
        }
        catch {
            Start-Sleep -Seconds 3
        }
    }

    if (-Not $success) {
        Write-Host "Failed to download: $url"
        exit 1
    }
}

# Make temp path
$tempPath = Join-Path $env:TEMP "kiwiflow_setup"
if (-not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
    Write-Host "Created temporary directory: $tempPath"
}

# Declare url and target path
$updateURL = "https://github.com/infinity-a11y/KiwiFlow/raw/master/Output/KiwiFlow_2025-07-22_Setup.exe"
$updateInstaller = "$tempPath\update_kiwiflow.exe"

# Download update executable
try {
    Write-Host "Downloading update ..."
    Download-File $updateURL $updateInstaller
    Write-Host "Update download successful."
}
catch {
    Write-Host "Failed to download update. Error: $($_.Exception.Message)"
    exit 1
}

# Starting update wizard
try {
    Write-Host "Starting update wizard ..."
    Start-Process -Wait -FilePath $updateInstaller
    Write-Host "Update done."
}
catch {
    Write-Host "Failed to launch update wizard. Error: $($_.Exception.Message)"
    exit 1
}
