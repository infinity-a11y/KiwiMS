# update_kiwims.ps1

$userDataPath = "$env:LOCALAPPDATA\KiwiMS"
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force
}

Start-Transcript -Path "$userDataPath\kiwims_update.log"

# Set base path to the directory of the script or executable
$basePath = [System.AppContext]::BaseDirectory
# Remove trailing backslash
$basePath = $basePath.TrimEnd('\')

if (-not $basePath) {
    $basePath = Split-Path -Path $PSCommandPath -Parent -ErrorAction SilentlyContinue
}
if (-not $basePath) {
    Write-Host "Error: Could not determine the script/executable directory."
    Write-Host "Please ensure setup_kiwims.exe is run from the KiwiMS directory."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Validate $basePath
if (-not (Test-Path $basePath)) {
    Write-Host "Error: Directory $basePath does not exist."
    Write-Host "Please ensure setup_kiwims.exe is run from the KiwiMS directory."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Ensure the working directory is set to $basePath
try {
    Write-Host "Setting working directory to $basePath"
    Set-Location -Path $basePath -ErrorAction Stop
}
catch {
    Write-Host "Error: Failed to set working directory to $basePath. $_"
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Check version
Write-Host "Checking for updates..."
try {
    $remoteVersionUrl = "https://raw.githubusercontent.com/infinity-a11y/KiwiMS/master/resources/version.txt"
    $remoteVersionContent = Invoke-WebRequest -Uri $remoteVersionUrl -ErrorAction Stop | Select-Object -ExpandProperty Content
    $remoteVersionInfo = @{}
    $remoteVersionContent -split "`n" | ForEach-Object {
        if ($_ -match "^([^=]+)=(.+)$") {
            $remoteVersionInfo[$matches[1]] = $matches[2]
        }
    }
    $remoteVersion = $remoteVersionInfo["version"]
    $zipUrl = $remoteVersionInfo["zip_url"]

    $localVersionFile = "$basePath\resources\version.txt"
    $localVersion = if (Test-Path $localVersionFile) { (Get-Content $localVersionFile | Where-Object { $_ -match "^version=(.+)$" } | ForEach-Object { $matches[1] }) } else { "0.0.0" }

    Write-Host "Local version: $localVersion, Remote version: $remoteVersion"

    # Skip update if versions are the same
    if ($localVersion -eq $remoteVersion) {
        Write-Host "No update needed. Local version matches remote version."
    }
    else {
        # Download and extract the latest version
        Write-Host "Downloading latest version..."
        try {
            $zipFileName = [System.IO.Path]::GetFileName($zipUrl)
            $zipPath = "$env:USERPROFILE\Downloads\$zipFileName"
            $tempDir = "$env:USERPROFILE\Downloads\kiwims_temp"
            Invoke-WebRequest -Uri $zipUrl -OutFile $zipPath -ErrorAction Stop
            Write-Host "Zip file downloaded successfully."

            # Extract to temporary directory
            Expand-Archive -Path $zipPath -DestinationPath $tempDir -Force
            Write-Host "Zip file extracted to $tempDir."
        }
        catch {
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

echo Waiting for kiwims_update.exe to close...

:CHECK_PROCESS
tasklist | findstr /I "kiwims_update.exe" >nul
if %ERRORLEVEL% == 0 (
    echo Waiting for kiwims_update.exe to close...
    timeout /t 1 /nobreak >nul
    goto CHECK_PROCESS
)

echo kiwims_update.exe has closed.

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
start "" "%basePath%\kiwims_update.exe"
exit
"@

        try {
            Set-Content -Path $updaterBatPath -Value $updaterBatContent -ErrorAction Stop
            Write-Host "Secondary updater batch file created at $updaterBatPath."
        }
        catch {
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
        }
        catch {
            Write-Host "Error: Failed to launch secondary updater batch file. $_"
            pause
            Stop-Transcript
            exit 1
        }
    }
}
catch {
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
        Write-Host "Error: Conda not found. Please run setup_kiwims.exe first."
        $wShell = New-Object -ComObject WScript.Shell
        $wShell.Popup("Error: Conda not found. Please run setup_kiwims.exe first.", 0, "KiwiMS Update Error", 16)
        pause
        Stop-Transcript
        exit 1
    }
    Write-Host "Conda found at $condaPath"
}
catch {
    Write-Host "Error: Failed to locate Conda. $_"
    $wShell = New-Object -ComObject WScript.Shell
    $wShell.Popup("Error: Failed to locate Conda. $_", 0, "KiwiMS Update Error", 16)
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
}
catch {
    Write-Host "Error: Failed to initialize Conda. $_"
    $wShell = New-Object -ComObject WScript.Shell
    $wShell.Popup("Error: Failed to initialize Conda. $_", 0, "KiwiMS Update Error", 16)
    pause
    Stop-Transcript
    exit 1
}

# Update kiwims environment using conda run
Write-Host "Updating kiwims environment..."
try {
    $envYmlPath = "$basePath\resources\environment.yml"
    # Use conda run to execute commands in the base environment
    & $condaPath run -n base conda update -n base -c defaults conda
    if ($LASTEXITCODE -ne 0) { throw "Conda base environment update failed with exit code $LASTEXITCODE." }
    $envExists = & $condaPath run -n base conda env list | Select-String "kiwims"
    if ($envExists) {
        & $condaPath run -n base conda env update -n kiwims -f $envYmlPath --prune
        if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
        Write-Host "Environment updated successfully."
    }
    else {
        Write-Host "Error: kiwims environment not found. Please run setup_kiwims.exe first."
        $wShell = New-Object -ComObject WScript.Shell
        $wShell.Popup("Error: kiwims environment not found. Please run setup_kiwims.exe first.", 0, "KiwiMS Update Error", 16)
        pause
        Stop-Transcript
        exit 1
    }
}
catch {
    try {
        Write-Host "Conda environment update failed. Retrying."
        Start-Sleep -Seconds 5
        if ($envExists) {
            & $condaPath run -n base conda env update -n kiwims -f $envYmlPath --prune
            if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
            Write-Host "Environment updated successfully."
        }
        else {
            Write-Host "Error: kiwims environment not found. Please run setup_kiwims.exe first."
            $wShell = New-Object -ComObject WScript.Shell
            $wShell.Popup("Error: kiwims environment not found. Please run setup_kiwims.exe first.", 0, "KiwiMS Update Error", 16)
            pause
            Stop-Transcript
            exit 1
        }
    }
    catch {
        Write-Host "Error: Failed to update environment. $_"
        Write-Host "Run 'conda env update -n kiwims -f $envYmlPath --prune' manually to debug."
        pause
        Stop-Transcript
        exit 1
    }
}

# Updating R packages
Write-Host "Updating R packages"
try {
    & $condaPath run -n kiwims R.exe -e "renv::restore()"
}
catch {
    try {
        Write-Host "R package update failed. Retrying."
        Start-Sleep -Seconds 5
        & $condaPath run -n kiwims R.exe -e "renv::restore()"
    }
    catch {
        Write-Host "Error: Failed to update R packages. $_"
        pause
        Stop-Transcript
        exit 1
    }
}

# Create run_app.vbs for launch
Write-Host "Creating run_app.vbs script..."
try {
    $vbsPath = "$basePath\run_app.vbs"
    $appPath = "$basePath\app.R" -replace '\\', '\\'  # Single to double backslashes
    $logPath = "$userDataPath\launch.log" -replace '\\', '\\'  # Single to double backslashes
    $condaExe = $condaPath -replace '\\', '\\'  # Single to double backslashes
    $vbsContent = @"
Option Explicit
Dim WShell, WMI, Process, Processes, IsRunning, PortInUse, CmdLine, LogFile
Dim PopupMsg, PopupTitle, PopupTimeout, AppPath

' Initialize objects
Set WShell = CreateObject("WScript.Shell")
Set WMI = GetObject("winmgmts:{impersonationLevel=impersonate}!\\.\root\cimv2")

' Configuration
PopupTitle = "KiwiMS"
LogFile = "$logPath"
AppPath = "$appPath"
IsRunning = False
PortInUse = False

' Function to check if a process is running
Function CheckProcess(processName)
    Dim procQuery, proc
    procQuery = "SELECT * FROM Win32_Process WHERE Name = '" & processName & "'"
    Set Processes = WMI.ExecQuery(procQuery)
    For Each proc In Processes
        If InStr(1, proc.CommandLine, "shiny::runApp", 1) > 0 And InStr(1, proc.CommandLine, "port=3838", 1) > 0 Then
            IsRunning = True
            Exit For
        End If
    Next
End Function

' Function to check if port 3838 is in use
Function CheckPort(port)
    Dim netStat, line, lines
    ' Run netstat to check for port usage
    netStat = WShell.Exec("netstat -an -p TCP").StdOut.ReadAll
    lines = Split(netStat, vbCrLf)
    For Each line In lines
        If InStr(line, ":" & port) > 0 And InStr(line, "LISTENING") > 0 Then
            PortInUse = True
            Exit For
        End If
    Next
End Function

' Check if Rscript.exe is running the Shiny app
Call CheckProcess("Rscript.exe")
Call CheckPort(3838)

' Logic based on process and port status
If IsRunning And PortInUse Then
    PopupMsg = "KiwiMS is already running on port 3838. Please use the existing browser tab."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
    ' Do not open a new browser tab
Else
    PopupMsg = "KiwiMS will open shortly, please wait..."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
    ' Run the Shiny app with properly escaped path
    CmdLine = "cmd.exe /c ""$condaExe run -n kiwims Rscript -e ""shiny::runApp('" & AppPath & "', port=3838, launch.browser=TRUE)"" > " & LogFile & " 2>&1"""
    WShell.Run CmdLine, 0
End If

' Clean up
Set WShell = Nothing
Set WMI = Nothing
"@
    Set-Content -Path $vbsPath -Value $vbsContent -ErrorAction Stop
    Write-Host "run_app.vbs created at $vbsPath."
}
catch {
    Write-Host "Error: Failed to create run_app.vbs. $_"
    pause
    Stop-Transcript
    exit 1
}

# Re-create Desktop Shortcut
Write-Host "Creating desktop shortcut for KiwiMS..."
try {
    $shortcutPath = "$env:USERPROFILE\Desktop\KiwiMS.lnk"
    $iconPath = "$basePath\app\static\favicon.ico"
    $appPath = "$basePath\app.R" -replace '\\', '\\'
    $vbsPath = "$basePath\run_app.vbs" -replace '\\', '\\'
    $wshShell = New-Object -ComObject WScript.Shell
    $shortcut = $wshShell.CreateShortcut($shortcutPath)
    $shortcut.TargetPath = "C:\Windows\System32\wscript.exe"
    $shortcut.Arguments = """$vbsPath"""
    $shortcut.WorkingDirectory = $basePath
    $shortcut.Description = "Launch KiwiMS Shiny App"
    if (Test-Path $iconPath) {
        $shortcut.IconLocation = $iconPath
        Write-Host "Custom icon applied from $iconPath."
    }
    else {
        Write-Host "Warning: Custom icon not found at $iconPath. Using default icon."
        $shortcut.IconLocation = "C:\Windows\System32\shell32.dll,23"
    }
    $shortcut.Save()
    Write-Host "Desktop shortcut created at $shortcutPath."
}
catch {
    Write-Host "Error: Failed to create desktop shortcut. $_"
    pause
    Stop-Transcript
    exit 1
}

Write-Host "Update complete. Check kiwims_update.log in $userDataPath for details."
Write-Host "Press ENTER to finish, then restart the KiwiMS app with the new version."
pause
Stop-Transcript
exit 0
