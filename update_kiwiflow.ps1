# update_kiwiflow.ps1

param (
    [string]$WorkingDir = $PSScriptRoot
)

# Set base path to the working directory
$basePath = $WorkingDir
Start-Transcript -Path "$basePath\kiwiflow_update.log" -Append
Write-Host "Updating KiwiFlow in $basePath..."

# Check version
Write-Host "Checking for updates..."
try {
    $remoteVersionUrl = "https://raw.githubusercontent.com/infinity-a11y/KiwiFlow/master/version.txt"
    $remoteVersionContent = Invoke-WebRequest -Uri $remoteVersionUrl -ErrorAction Stop | Select-Object -ExpandProperty Content
    $remoteVersionInfo = @{}
    $remoteVersionContent -split "`n" | ForEach-Object {
        if ($_ -match "^([^=]+)=(.+)$") {
            $remoteVersionInfo[$matches[1]] = $matches[2]
        }
    }
    $remoteVersion = $remoteVersionInfo["version"]
    $zipUrl = $remoteVersionInfo["zip_url"]

    $localVersionFile = "$basePath\version.txt"
    $localVersion = if (Test-Path $localVersionFile) { (Get-Content $localVersionFile | Where-Object { $_ -match "^version=(.+)$" } | ForEach-Object { $matches[1] }) } else { "0.0.0" }

    Write-Host "Local version: $localVersion, Remote version: $remoteVersion"
} catch {
    Write-Host "Error: Failed to check version. $_"
    pause
    exit 1
}

# Download and extract the latest version
Write-Host "Downloading latest version..."
try {
    $zipFileName = [System.IO.Path]::GetFileName($zipUrl)
    $zipPath = "$env:USERPROFILE\Downloads\$zipFileName"
    $tempDir = "$env:USERPROFILE\Downloads\kiwiflow_temp"
    Invoke-WebRequest -Uri $zipUrl -OutFile $zipPath -ErrorAction Stop
    Write-Host "Zip file downloaded successfully."

    # Extract to temporary directory
    Expand-Archive -Path $zipPath -DestinationPath $tempDir -Force
    # Find the root folder in the zip
    $extractedFolder = Get-ChildItem -Path $tempDir -Directory | Select-Object -First 1
    # Copy updated files to basePath
    Copy-Item -Path "$($extractedFolder.FullName)\*" -Destination $basePath -Recurse -Force
    Remove-Item $tempDir -Recurse -Force
    Remove-Item $zipPath -ErrorAction SilentlyContinue
    Write-Host "App files updated successfully."
} catch {
    Write-Host "Error: Failed to download or extract app files. $_"
    pause
    exit 1
}

# Ensure Conda is initialized
Write-Host "Initializing Conda for PowerShell..."
try {
    $condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
    if (-not $condaPath) {
        Write-Host "Error: Conda not found. Please run setup_kiwiflow.ps1 first."
        pause
        exit 1
    }
    & conda init powershell
    $condaHook = "$env:USERPROFILE\Miniconda3\shell\condabin\conda-hook.ps1"
    if (-not (Test-Path $condaHook)) {
        $condaHook = "C:\Miniconda3\shell\condabin\conda-hook.ps1"
    }
    if (Test-Path $condaHook) {
        . $condaHook
    } else {
        Write-Host "Warning: conda-hook.ps1 not found. May need manual shell restart."
        pause
        exit 1
    }
} catch {
    Write-Host "Error: Failed to initialize Conda. $_"
    pause
    exit 1
}

# Activate Conda base environment
Write-Host "Activating Conda base environment..."
try {
    & conda activate base
    if ($LASTEXITCODE -ne 0) { throw "Base environment activation failed with exit code $LASTEXITCODE." }
} catch {
    Write-Host "Error: Failed to activate base environment. $_"
    pause
    exit 1
}

# Update kiwiflow environment
Write-Host "Updating kiwiflow environment..."
try {
    conda update -n base -c defaults conda 
    $envYmlPath = "$basePath\environment.yml"
    $envExists = & conda env list | Select-String "kiwiflow"
    if ($envExists) {
        & conda env update -n kiwiflow -f $envYmlPath --prune
        if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
        Write-Host "Environment updated successfully."
    } else {
        Write-Host "Error: kiwiflow environment not found. Please run setup_kiwiflow.ps1 first."
        pause
        exit 1
    }
} catch {
    Write-Host "Error: Failed to update environment. $_"
    Write-Host "Run 'conda env update -n kiwiflow -f $envYmlPath --prune' manually to debug."
    pause
    exit 1
}

# Re-create Desktop Shortcut
Write-Host "Re-creating desktop shortcut for KiwiFlow..."
try {
    $shortcutPath = "$env:USERPROFILE\Desktop\KiwiFlow.lnk"
    $iconPath = "$basePath\app\static\favicon.ico"
    $appPath = "$basePath\app.R" -replace '\\', '\\'
    $wshShell = New-Object -ComObject WScript.Shell
    $shortcut = $wshShell.CreateShortcut($shortcutPath)
    $shortcut.TargetPath = "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"
    $shortcut.Arguments = "-NoExit -Command `"conda activate kiwiflow; Rscript -e \`"shiny::runApp('$appPath', port=3838, launch.browser = T)\`"`""
    $shortcut.WorkingDirectory = $basePath
    $shortcut.Description = "Launch KiwiFlow Shiny App"
    if (Test-Path $iconPath) {
        $shortcut.IconLocation = $iconPath
        Write-Host "Custom icon applied from $iconPath."
    } else {
        Write-Host "Warning: Custom icon not found at $iconPath. Using default icon."
        $shortcut.IconLocation = "C:\Windows\System32\shell32.dll,23"
    }
    $shortcut.Save()
    Write-Host "Desktop shortcut re-created at $shortcutPath."
} catch {
    Write-Host "Error: Failed to re-create desktop shortcut. $_"
    pause
    exit 1
}

Write-Host "Update complete. Check kiwiflow_update.log in $basePath for details."
Write-Host "Press ENTER to finish, then restart the KiwiFlow app with the new version."
pause
exit