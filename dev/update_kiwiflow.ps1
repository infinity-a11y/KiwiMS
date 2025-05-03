# update_kiwiflow.ps1

$userDataPath = "$env:LOCALAPPDATA\KiwiFlow"
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force
}

Start-Transcript -Path "$userDataPath\kiwiflow_update.log"

# Set base path to the directory of the script or executable
$basePath = [System.AppContext]::BaseDirectory
# Remove trailing backslash
$basePath = $basePath.TrimEnd('\')

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

    # Skip update if versions are the same
    if ($localVersion -eq $remoteVersion) {
        Write-Host "No update needed. Local version matches remote version."
    } else {
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
            Write-Host "Zip file extracted to $tempDir."
        } catch {
            Write-Host "Error: Failed to download or extract zip file. $_"
            pause
            Stop-Transcript
            exit 1
        }

        # Create a batch file to handle file replacement
        $updaterBatPath = "$env:USERPROFILE\Downloads\update_secondary.bat"
        $updaterBatContent = @"
@echo off
set "basePath=$basePath"
set "userDataPath=$userDataPath"
set "tempDir=$tempDir"
set "zipPath=$zipPath"

echo Waiting for kiwiflow_update.exe to close...

:CHECK_PROCESS
tasklist | findstr /I "kiwiflow_update.exe" >nul
if %ERRORLEVEL% == 0 (
    echo Waiting for kiwiflow_update.exe to close...
    timeout /t 1 /nobreak >nul
    goto CHECK_PROCESS
)

echo kiwiflow_update.exe has closed.

:: Find the extracted folder
for /d %%D in ("%tempDir%\*") do set "extractedFolder=%%D"

:: Debug: Print source and destination paths
echo Source: %extractedFolder%
echo Destination: %basePath%

:: Verify extracted folder exists
if not exist "%extractedFolder%" (
    echo Error: Extracted folder not found.
    pause
    exit /b 1
)

:: Copy updated files to basePath using robocopy
echo Copying updated files to %basePath%...
robocopy "%extractedFolder%" "%basePath%" /MIR /XD "%basePath%" /R:3 /W:1 > "%userDataPath%\update_secondary.log" 2>&1
if %ERRORLEVEL% GEQ 8 (
    echo Error: Failed to copy updated files. Check update_secondary.log for details.
    pause
    exit /b 1
)

:: Clean up
echo Cleaning up...
rd /s /q "%tempDir%" 2>nul
del "%zipPath%" 2>nul
echo Cleanup completed.

:: Relaunch the application
start "" "%basePath%\kiwiflow_update.exe"
exit
"@

        try {
            Set-Content -Path $updaterBatPath -Value $updaterBatContent -ErrorAction Stop
            Write-Host "Secondary updater batch file created at $updaterBatPath."
        } catch {
            Write-Host "Error: Failed to create secondary updater batch file. $_"
            pause
            Stop-Transcript
            exit 1
        }

        # Launch the batch file and exit
        try {
            Start-Process -FilePath "cmd.exe" -ArgumentList "/c `"$updaterBatPath`"" -ErrorAction Stop
            Write-Host "Launched secondary updater batch file. This process will now exit."
            Stop-Transcript
            exit 0
        } catch {
            Write-Host "Error: Failed to launch secondary updater batch file. $_"
            pause
            Stop-Transcript
            exit 1
        }
    }
} catch {
    Write-Host "Error: Failed to check version. $_"
    pause
    Stop-Transcript
    exit 1
}

# Check if Conda is installed
Write-Host "Checking for Conda..."
try {
    $condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
    if (-not $condaPath) {
        Write-Host "Error: Conda not found. Please run setup_kiwiflow.exe first."
        $wShell = New-Object -ComObject WScript.Shell
        $wShell.Popup("Error: Conda not found. Please run setup_kiwiflow.exe first.", 0, "KiwiFlow Update Error", 16)
        pause
        Stop-Transcript
        exit 1
    }
    Write-Host "Conda found at $condaPath"
} catch {
    Write-Host "Error: Failed to locate Conda. $_"
    $wShell = New-Object -ComObject WScript.Shell
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
    $wShell = New-Object -ComObject WScript.Shell
    $wShell.Popup("Error: Failed to initialize Conda. $_", 0, "KiwiFlow Update Error", 16)
    pause
    Stop-Transcript
    exit 1
}

# Update kiwiflow environment using conda run
Write-Host "Updating kiwiflow environment..."
try {
    $envYmlPath = "$basePath\resources\environment.yml"
    # Use conda run to execute commands in the base environment
    & $condaPath run -n base conda update -n base -c defaults conda
    if ($LASTEXITCODE -ne 0) { throw "Conda base environment update failed with exit code $LASTEXITCODE." }
    $envExists = & $condaPath run -n base conda env list | Select-String "kiwiflow"
    if ($envExists) {
        & $condaPath run -n base conda env update -n kiwiflow -f $envYmlPath --prune
        if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
        Write-Host "Environment updated successfully."
    } else {
        Write-Host "Error: kiwiflow environment not found. Please run setup_kiwiflow.exe first."
        $wShell = New-Object -ComObject WScript.Shell
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

# Create run_app.vbs for launch
Write-Host "Creating run_app.vbs script..."
try {
    $vbsPath = "$basePath\run_app.vbs"
    $appPath = "$basePath\app.R" -replace '\\', '\\'
    $logPath = "$basePath\launch_log.txt" -replace '\\', '\\'
    $condaExe = $condaPath -replace '\\', '\\'
    $vbsContent = @"
Set WShell = CreateObject("WScript.Shell")
WShell.Popup "KiwiFlow will open shortly, please wait...", 3, "KiwiFlow", 0
WShell.Run "cmd.exe /c ""$condaExe run -n kiwiflow Rscript -e ""shiny::runApp('$appPath', port=3838, launch.browser=TRUE)"" > $logPath 2>&1""", 0
"@
    Set-Content -Path $vbsPath -Value $vbsContent -ErrorAction Stop
    Write-Host "run_app.vbs created at $vbsPath."
} catch {
    Write-Host "Error: Failed to create run_app.vbs. $_"
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

Write-Host "Update complete. Check kiwiflow_update.log in $userDataPath for details."
Write-Host "Press ENTER to finish, then restart the KiwiFlow app with the new version."
pause
Stop-Transcript
exit 0
