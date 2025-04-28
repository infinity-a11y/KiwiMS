# update_kiwiflow.ps1

# Set base path to the directory of the script or executable
$basePath = [System.AppContext]::BaseDirectory
if (-not $basePath) {
    $basePath = Split-Path -Path $PSCommandPath -Parent -ErrorAction SilentlyContinue
}
if (-not $basePath) {
    Write-Host "Error: Could not determine the script/executable directory."
    Write-Host "Please ensure setup_kiwiflow.exe is run from the KiwiFlow directory."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Normalize $basePath to remove trailing backslash
$basePath = $basePath.TrimEnd('\')

# Validate $basePath
if (-not (Test-Path $basePath)) {
    Write-Host "Error: Directory $basePath does not exist."
    Write-Host "Please ensure setup_kiwiflow.exe is run from the KiwiFlow directory."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Ensure the working directory is set to $basePath
try {
    Write-Host "Setting working directory to $basePath"
    Set-Location -Path $basePath -ErrorAction Stop
} catch {
    Write-Host "Error: Failed to set working directory to $basePath. $_"
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

Start-Transcript -Path "$basePath\kiwiflow_update.log" -Append

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

# Check if Conda is installed
Write-Host "Checking for Conda..."
try {
    $condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
    if (-not $condaPath) {
        Write-Host "Error: Conda not found. Please run setup_kiwiflow.exe first."
        $wShell.Popup("Error: Conda not found. Please run setup_kiwiflow.exe first.", 0, "KiwiFlow Update Error", 16)
        pause
        Stop-Transcript
        exit 1
    }
    Write-Host "Conda found at $condaPath"
} catch {
    Write-Host "Error: Failed to locate Conda. $_"
    $wShell.Popup("Error: Failed to locate Conda. $_", 0, "KiwiFlow Update Error", 16)
    pause
    Stop-Transcript
    exit 1
}

# Initialize Conda environment manually
Write-Host "Initializing Conda environment..."
try {
    # Set PATH to include Conda
    $env:Path = "$condaPath\..\..;$condaPath\..\Scripts;$condaPath\..\Library\bin;" + $env:Path
    # Verify conda.exe is accessible
    if (-not (Test-Path $condaPath)) {
        throw "Conda executable not found at $condaPath."
    }
    Write-Host "Conda environment initialized."
} catch {
    Write-Host "Error: Failed to initialize Conda. $_"
    $wShell.Popup("Error: Failed to initialize Conda. $_", 0, "KiwiFlow Update Error", 16)
    pause
    Stop-Transcript
    exit 1
}

# Update kiwiflow environment using conda run
Write-Host "Updating kiwiflow environment..."
try {
    $envYmlPath = "$basePath\environment.yml"
    # Use conda run to execute commands in the base environment
    Write-Host "Test1"
    & $condaPath run -n base conda update -n base -c defaults conda
    Write-Host "Test2"
    if ($LASTEXITCODE -ne 0) { throw "Conda base environment update failed with exit code $LASTEXITCODE." }
    Write-Host "Test3"
    $envExists = & $condaPath run -n base conda env list | Select-String "kiwiflow"
    if ($envExists) {
        Write-Host "Test4"
        & $condaPath run -n base conda env update -n kiwiflow -f $envYmlPath --prune
        Write-Host "Test5"
        if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
        Write-Host "Environment updated successfully."
    } else {
        Write-Host "Error: kiwiflow environment not found. Please run setup_kiwiflow.exe first."
        $wShell.Popup("Error: kiwiflow environment not found. Please run setup_kiwiflow.exe first.", 0, "KiwiFlow Update Error", 16)
        pause
        Stop-Transcript
        exit 1
    }
} catch {
    Write-Host "Error: Failed to update environment. $_"
    Write-Host "Run 'conda env update -n kiwiflow -f $envYmlPath --prune' manually to debug."
    pause
    Stop-Transcript
    exit 1
}

# Re-create Desktop Shortcut
Write-Host "Creating desktop shortcut for KiwiFlow..."
try {
    $shortcutPath = "$env:USERPROFILE\Desktop\KiwiFlow.lnk"
    $iconPath = "$basePath\app\static\favicon.ico"
    $appPath = "$basePath\app.R" -replace '\\', '\\'
    $vbsPath = "$basePath\run_app.vbs" -replace '\\', '\\'
    $wshShell = New-Object -ComObject WScript.Shell
    $shortcut = $wshShell.CreateShortcut($shortcutPath)
    $shortcut.TargetPath = "C:\Windows\System32\wscript.exe"
    $shortcut.Arguments = """$vbsPath"""
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
    Write-Host "Desktop shortcut created at $shortcutPath."
} catch {
    Write-Host "Error: Failed to create desktop shortcut. $_"
    pause
    Stop-Transcript
    exit 1
}

Write-Host "Update complete. Check kiwiflow_update.log in $basePath for details."
Write-Host "Press ENTER to finish, then restart the KiwiFlow app with the new version."
pause
exit