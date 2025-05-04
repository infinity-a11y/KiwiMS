# setup_kiwiflow.ps1

# Initialize WScript.Shell
$wShell = New-Object -ComObject WScript.Shell

# Check if running as Administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Host "Error: This script requires administrative privileges."
    Write-Host "Please right-click 'update.exe' and select 'Run as administrator'."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Set base path
$basePath = [System.AppContext]::BaseDirectory
if (-not $basePath) {
    $basePath = Split-Path -Path $PSCommandPath -Parent -ErrorAction SilentlyContinue
}
if (-not $basePath) {
    Write-Host "Error: Could not determine the script/executable directory."
    Write-Host "Please ensure update.exe is run from the KiwiFlow directory."
    pause
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}

# Normalize $basePath
$basePath = $basePath.TrimEnd('\')

# Validate $basePath
if (-not (Test-Path $basePath)) {
    Write-Host "Error: Directory $basePath does not exist."
    Write-Host "Please ensure update.exe is run from the KiwiFlow directory."
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

$userDataPath = "$env:LOCALAPPDATA\KiwiFlow"
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force
}

# Start logging
Start-Transcript -Path "$userDataPath\kiwiflow_setup.log"
Write-Host "Setting up KiwiFlow environment in $basePath..."

# Verify critical files exist
$criticalFiles = @("resources\environment.yml", "app.R")
foreach ($file in $criticalFiles) {
    if (-not (Test-Path "$basePath\$file")) {
        Write-Host "Error: Required file '$file' not found in $basePath."
        Write-Host "Please ensure all required files are in the KiwiFlow directory and try again."
        pause
        Stop-Transcript
        exit 1
    }
}

# Check if Conda is installed
$condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
if (-not $condaPath) {
    Write-Host "Conda not found. Downloading and installing Miniconda to user directory..."
    $minicondaUrl = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe"
    $installerPath = "$env:USERPROFILE\Downloads\Miniconda3-latest.exe"

    try {
        Invoke-WebRequest -Uri $minicondaUrl -OutFile $installerPath -ErrorAction Stop
        Write-Host "Miniconda downloaded successfully."
    } catch {
        $errorMsg = "Error: Failed to download Miniconda. $_"
        Write-Host $errorMsg
        $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
        Stop-Transcript
        exit 1
    }

    Write-Host "Installing Miniconda to $env:USERPROFILE\Miniconda3..."
    try {
        Start-Process -FilePath $installerPath -ArgumentList "/S /AddToPath=0 /D=$env:USERPROFILE\Miniconda3" -Wait -NoNewWindow -ErrorAction Stop
        $env:Path += ";$env:USERPROFILE\Miniconda3;$env:USERPROFILE\Miniconda3\Scripts;$env:USERPROFILE\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installed to $env:USERPROFILE\Miniconda3."
    } catch {
        $errorMsg = "Error: Failed to install Miniconda. $_"
        Write-Host $errorMsg
        $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
        Stop-Transcript
        exit 1
    }
    Remove-Item $installerPath -ErrorAction SilentlyContinue

    # Refresh PATH
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "User")
    $condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
    if (-not $condaPath) {
        $errorMsg = "Error: Conda installation failed. Please install manually from https://docs.conda.io/en/latest/miniconda.html"
        Write-Host $errorMsg
        $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
        Stop-Transcript
        exit 1
    }
    Write-Host "Miniconda installed successfully."
} else {
    Write-Host "Conda found at $condaPath"
}

# Initialize Conda environment
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
    $errorMsg = "Error: Failed to initialize Conda. $_`nPlease ensure Conda is installed correctly."
    Write-Host $errorMsg
    $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
    Stop-Transcript
    exit 1
}

# Set working directory to the base path
Set-Location $basePath

# Check and manage kiwiflow environment
Write-Host "Checking kiwiflow environment in $basePath\resources\environment.yml"
try {
    $envYmlPath = "$basePath\resources\environment.yml"
    # Use conda run to execute commands in the base environment
    $envExists = & $condaPath run -n base conda env list | Select-String "kiwiflow"
    if ($envExists) {
        Write-Host "kiwiflow environment already exists. Updating the environment..."
        & $condaPath run -n base conda env update -n kiwiflow -f $envYmlPath --prune
        if ($LASTEXITCODE -ne 0) { throw "Conda environment update failed with exit code $LASTEXITCODE." }
        Write-Host "Environment updated successfully."
    } else {
        Write-Host "kiwiflow environment does not exist. Creating the environment..."
        & $condaPath run -n base conda env create -n kiwiflow -f $envYmlPath
        if ($LASTEXITCODE -ne 0) { throw "Conda environment creation failed with exit code $LASTEXITCODE." }
        Write-Host "Environment created successfully."
    }
} catch {
    $errorMsg = "Error: Failed to manage environment. $_`nRun 'conda env create -n kiwiflow -f $envYmlPath' or 'conda env update -n kiwiflow -f $envYmlPath --prune' manually to debug."
    Write-Host $errorMsg
    $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
    Stop-Transcript
    exit 1
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
PopupTitle = "KiwiFlow"
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
    PopupMsg = "KiwiFlow is already running on port 3838. Please use the existing browser tab."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
    ' Do not open a new browser tab
Else
    PopupMsg = "KiwiFlow will open shortly, please wait..."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
    ' Run the Shiny app with properly escaped path
    CmdLine = "cmd.exe /c ""$condaExe run -n kiwiflow Rscript -e ""shiny::runApp('" & AppPath & "', port=3838, launch.browser=TRUE)"" > " & LogFile & " 2>&1"""
    WShell.Run CmdLine, 0
End If

' Clean up
Set WShell = Nothing
Set WMI = Nothing
"@
    Set-Content -Path $vbsPath -Value $vbsContent -ErrorAction Stop
    Write-Host "run_app.vbs created at $vbsPath."
} catch {
    Write-Host "Error: Failed to create run_app.vbs. $_"
    pause
    Stop-Transcript
    exit 1
}

# Create Desktop Shortcut for KiwiFlow
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

Write-Host "Setup complete. Check kiwiflow_setup.log in $userDataPath for details."
pause
Stop-Transcript