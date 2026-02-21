# update.ps1
# Fetch and run KiwiMS update 

# Log update script
$logFile = "$env:LOCALAPPDATA\KiwiMS\update.log"
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
        Write-Output "Failed to download: $url"
        exit 1
    }
}

# Make temp path
$tempPath = Join-Path $env:TEMP "kiwims_setup"
if (-not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory -Force | Out-Null
    Write-Output "Created temporary directory: $tempPath"
}

# Declare url and target path
$updateURL = "https://github.com/infinity-a11y/KiwiMS/raw/master/Output/KiwiMS_2025-07-22_Setup.exe"
$updateInstaller = "$tempPath\update_kiwims.exe"

# Download update executable
try {
    Write-Output "Downloading update ..."
    Download-File $updateURL $updateInstaller
    Write-Output "Update download successful."
}
catch {
    Write-Output "Failed to download update. Error: $($_.Exception.Message)"
    exit 1
}

# Starting update wizard
try {
    Write-Output "Starting update wizard ..."
    Start-Process -Wait -FilePath $updateInstaller
    Write-Output "Update done."
}
catch {
    Write-Output "Failed to launch update wizard. Error: $($_.Exception.Message)"
    exit 1
}
