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
}
catch {
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

# Instal rtools
# --- Configuration ---
# $RtoolsInstallerUrl = "https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe" # *** IMPORTANT: Verify this URL on the CRAN Rtools page for the latest version ***
# $DownloadPath = "$env:TEMP\rtools44.exe"
# $RtoolsInstallDir = "C:\rtools44"

# # --- Download Rtools44 installer ---
# Write-Host "Downloading Rtools44 installer from $RtoolsInstallerUrl to $DownloadPath..."
# try {
#     Invoke-WebRequest -Uri $RtoolsInstallerUrl -OutFile $DownloadPath -ErrorAction Stop
#     Write-Host "Download complete."
# }
# catch {
#     Write-Host "Download failed. Trying again."
#     try {
#         Invoke-WebRequest -Uri $RtoolsInstallerUrl -OutFile $DownloadPath -ErrorAction Stop
#         Write-Host "Download complete."
#     }
#     catch {
#         Write-Error "Failed to download Rtools44 installer. Please check the URL and your internet connection. Error: $($_.Exception.Message)"
#         pause
#         Stop-Transcript
#         exit 1
#     }
# }

# # --- Create installation directory if it doesn't exist ---
# if (-not (Test-Path $RtoolsInstallDir)) {
#     Write-Host "Creating installation directory: $RtoolsInstallDir"
#     New-Item -ItemType Directory -Path $RtoolsInstallDir | Out-Null
# }

# --- Run Rtools44 installer silently ---
# Write-Host "Installing Rtools44 ..."
# try {
#     Write-Host "Rtools44 installation initiated. Please follow instructions in the Rtools installer."
#     Start-Process -FilePath $DownloadPath -ArgumentList "/DIR=$RtoolsInstallDir" -Wait -Passthru | Out-Null
# }
# catch {
#     Write-Error "Failed to start Rtools44 installation. Error: $($_.Exception.Message)"
#     pause
#     Stop-Transcript
#     exit 1
# }

# Remove the installer file after successful installation
# if (Test-Path $DownloadPath) {
#     Remove-Item $DownloadPath -Force
#     Write-Host "Cleaned up temporary installer file."
# }

# Write-Host "`n--- Installation Complete ---"

# Check if Conda is installed
$condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
if (-not $condaPath) {
    Write-Host "Conda not found. Downloading and installing Miniconda to user directory..."
    $minicondaUrl = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe"
    $installerPath = "$env:USERPROFILE\Downloads\Miniconda3-latest.exe"

    try {
        Invoke-WebRequest -Uri $minicondaUrl -OutFile $installerPath -ErrorAction Stop
        Write-Host "Miniconda downloaded successfully."
    }
    catch {
        $errorMsg = "Error: Failed to download Miniconda. $_"
        Write-Host $errorMsg
        $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
        pause
        Stop-Transcript
        exit 1
    }

    Write-Host "Installing Miniconda to $env:USERPROFILE\Miniconda3..."
    try {
        Start-Process -FilePath $installerPath -ArgumentList "/S /AddToPath=0 /D=$env:USERPROFILE\Miniconda3" -Wait -NoNewWindow -ErrorAction Stop
        $env:Path += ";$env:USERPROFILE\Miniconda3;$env:USERPROFILE\Miniconda3\Scripts;$env:USERPROFILE\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installed to $env:USERPROFILE\Miniconda3."
    }
    catch {
        $errorMsg = "Error: Failed to install Miniconda. $_"
        Write-Host $errorMsg
        $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
        pause
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
        pause
        Stop-Transcript
        exit 1
    }
    Write-Host "Miniconda installed successfully."
}
else {
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
}
catch {
    $errorMsg = "Error: Failed to initialize Conda. $_`nPlease ensure Conda is installed correctly."
    Write-Host $errorMsg
    $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
    pause
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
    }
    else {
        Write-Host "kiwiflow environment does not exist. Creating the environment..."
        & $condaPath run -n base conda env create -n kiwiflow -f $envYmlPath
        if ($LASTEXITCODE -ne 0) { throw "Conda environment creation failed with exit code $LASTEXITCODE." }
        Write-Host "Environment created successfully."
    }
}
catch {
    $errorMsg = "Error: Failed to manage environment. $_`nRun 'conda env create -n kiwiflow -f $envYmlPath' or 'conda env update -n kiwiflow -f $envYmlPath --prune' manually to debug."
    Write-Host $errorMsg
    $wShell.Popup($errorMsg, 0, "KiwiFlow Setup Error", 16)
    pause
    Stop-Transcript
    exit 1
}

# --- Configuration ---
$RtoolsInstallerUrl = "https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe"
$DownloadPath = "$env:TEMP\rtools44.exe"
$RtoolsDefaultInstallDir = "C:\rtools44" # The directory where Rtools44 is *expected* to be installed by default

# Function to check if Rtools is found via R's Sys.which("make")
function Test-RtoolsFoundByR {
    param(
        [string]$condaPath,
        [string]$condaEnvName = "kiwiflow"
    )

    Write-Host "Attempting to check Rtools presence via R command 'Sys.which(\"make\")'..."
    try {
        # Temporarily add System32 to PATH for conda run
        $system32Path = "$env:SystemRoot\System32"
        $rOutput = & $condaPath run -n $condaEnvName -e "PATH=$system32Path;$env:PATH" R.exe -e "cat(Sys.which('make'))" 2>&1

        # Check if the output contains a valid path to make.exe
        # A successful output usually looks like "C:/Rtools44/usr/bin/make.exe" or similar.
        # We look for a path that contains "make.exe" and is not empty.
        if ($rOutput -match "make\.exe" -and $rOutput.Trim() -ne "") {
            Write-Host "Rtools 'make.exe' found by R at: $($rOutput.Trim())"
            return $true
        }
        else {
            Write-Host "Rtools 'make.exe' not found by R. Output: $($rOutput.Trim())"
            return $false
        }
    }
    catch {
        Write-Warning "Could not execute R command to check for Rtools. Error: $($_.Exception.Message)"
        return $false
    }
}

# --- Check if Rtools44 is already installed ---
Write-Host "Checking for existing Rtools44 installation..."
$rtoolsFound = $false

# 1. Check for the RTOOLS44_HOME environment variable
$rtoolsEnvVarPath = $env:RTOOLS44_HOME
if ($rtoolsEnvVarPath -and (Test-Path "$rtoolsEnvVarPath\usr\bin\make.exe" -PathType Leaf)) {
    Write-Host "Rtools44 found via RTOOLS44_HOME environment variable at: $rtoolsEnvVarPath"
    $RtoolsInstallDir = $rtoolsEnvVarPath # Update the variable to the found path
    $rtoolsFound = $true
}
# 2. Check the default installation directory if not found by env var
elseif (Test-Path "$RtoolsDefaultInstallDir\usr\bin\make.exe" -PathType Leaf) {
    Write-Host "Rtools44 found in default installation directory: $RtoolsDefaultInstallDir"
    $RtoolsInstallDir = $RtoolsDefaultInstallDir # Confirm default is used
    $rtoolsFound = $true
}
# 3. Check if R can find 'make.exe' (most reliable for R's purposes)
# Only run this if Conda/R.exe is expected to be available for testing
if (-not $rtoolsFound -and $condaPath -and (Test-Path $condaPath -PathType Leaf)) {
    if (Test-RtoolsFoundByR -condaPath $condaPath -condaEnvName "kiwiflow") {
        Write-Host "Rtools detected as accessible by R, though exact path not determined via standard locations."
        # If R can find it, we don't necessarily need to know *where* it is for this script's purpose
        # unless we need to refer to its path later for *our* operations.
        # For now, we'll assume it's good to go.
        $rtoolsFound = $true
    }
}


if ($rtoolsFound) {
    Write-Host "Rtools44 appears to be already installed and configured correctly."
}
else {
    Write-Host "Rtools44 not found or not fully installed. Proceeding with installation to default location: $RtoolsDefaultInstallDir..."

    # Ensure $RtoolsInstallDir is set to the default for installation if not found
    $RtoolsInstallDir = $RtoolsDefaultInstallDir

    # --- Download Rtools44 installer ---
    Write-Host "Downloading Rtools44 installer from $RtoolsInstallerUrl to $DownloadPath..."
    try {
        Invoke-WebRequest -Uri $RtoolsInstallerUrl -OutFile $DownloadPath -ErrorAction Stop
        Write-Host "Download complete."
    }
    catch {
        Write-Host "Download failed. Trying again."
        try {
            Invoke-WebRequest -Uri $RtoolsInstallerUrl -OutFile $DownloadPath -ErrorAction Stop
            Write-Host "Download complete."
        }
        catch {
            Write-Error "Failed to download Rtools44 installer. Please check the URL and your internet connection. Error: $($_.Exception.Message)"
            pause
            Stop-Transcript
            exit 1
        }
    }

    # --- Create installation directory if it doesn't exist ---
    if (-not (Test-Path $RtoolsInstallDir)) {
        Write-Host "Creating installation directory: $RtoolsInstallDir"
        New-Item -ItemType Directory -Path $RtoolsInstallDir | Out-Null
    }

    # --- Run Rtools44 installer silently ---
    Write-Host "Installing Rtools44 ..."
    try {
        Write-Host "Rtools44 installation initiated. Please wait for it to complete silently."
        Start-Process -FilePath $DownloadPath -ArgumentList "/VERYSILENT /SUPPRESSMSGBOXES /DIR=$RtoolsInstallDir" -Wait -Passthru | Out-Null
        Write-Host "Rtools44 silent installation command sent. Waiting for completion."

        # Add a brief pause to allow the installer process to start and clean up
        Start-Sleep -Seconds 5

        # Verify installation success by checking a key file again
        if (Test-Path "$RtoolsInstallDir\usr\bin\make.exe" -PathType Leaf) {
            Write-Host "Rtools44 installation appears to have completed successfully."
        }
        else {
            Write-Error "Rtools44 installation might have failed or not completed as expected. Key file 'make.exe' not found in $RtoolsInstallDir."
            Write-Error "Please check $RtoolsInstallDir for installation artifacts and run the installer manually if needed: $DownloadPath"
            pause
            Stop-Transcript
            exit 1
        }

    }
    catch {
        Write-Error "Failed to start Rtools44 installation. Error: $($_.Exception.Message)"
        pause
        Stop-Transcript
        exit 1
    }

    # Remove the installer file after successful installation
    if (Test-Path $DownloadPath) {
        Remove-Item $DownloadPath -Force
        Write-Host "Cleaned up temporary installer file."
    }
}

Write-Host "`n--- Rtools44 Setup Complete ---"

# IMPORTANT: Ensure the PATH for Rtools is set for the current session.
# This might be redundant if it was already installed and in PATH,
# but it ensures it's available for subsequent R commands in *this script's session*.
# If Rtools was just installed, this line is crucial.
Write-Host "Adding Rtools to current session's PATH if not already present..."
if (-not ($env:PATH -like "*$($RtoolsInstallDir)\usr\bin*")) {
    $env:PATH = "$RtoolsInstallDir\usr\bin;" + $env:PATH
    Write-Host "Added $($RtoolsInstallDir)\usr\bin to PATH."
}
else {
    Write-Host "Rtools path already found in session PATH."
}

# Write-Host "--- Updating Session PATH for R and Rtools ---"
# try {
#     $env:PATH = "C:\rtools44\usr\bin;$PATH"
#     $env:PATH = "$env:USERPROFILE\Miniconda3\envs\kiwiflow\Lib\R\bin;" + $env:PATH
#     Write-Host "Environment PATH set."
# }
# catch {
#     Write-Host "Environment PATH was not set correctly."
#     pause
#     Stop-Transcript
#     exit 1
# }


# Make R environment
# --- Configuration (ensure these are defined earlier in your script) ---
# $condaPath (e.g., C:\Users\Marian\Miniconda3\Scripts\conda.exe)
# $RtoolsInstallDir (e.g., C:\rtools44)
# $condaEnvName = "kiwiflow"

# ... (rest of the script) ...

# ... (rest of the script) ...

# --- Running R command: install.packages('renv', repos = 'https://cloud.r-project.org') ---
Write-Host "Installing renv"
try {
    # & $condaPath init
    # & $condaPath activate kiwiflow
    # & R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
    & $condaPath run -n kiwiflow R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
    # # CRUCIAL: Capture and temporarily modify the PATH for this command execution.
    # $originalPath = $env:PATH

    # $system32Path = "$env:SystemRoot\System32"
    # $rToolsPath = "$RtoolsInstallDir\usr\bin"
    # $condaScriptsPath = (Split-Path -Path $condaPath -Parent)
    # $condaLibraryBinPath = "$condaScriptsPath\..\Library\bin"

    # $tempPath = "$system32Path;$rToolsPath;$condaScriptsPath;$condaLibraryBinPath;$originalPath"
    # [Environment]::SetEnvironmentVariable("Path", $tempPath, [System.EnvironmentVariableTarget]::Process)
    # Write-Host "Temporarily adjusted PATH for R command execution."

    # # THIS IS THE KEY CHANGE: Wrap the R.exe command and its -e argument in double quotes
    # # so that 'conda run' treats it as a single command string to execute.
    # # We use single quotes for the R string itself to avoid conflicts with PowerShell's double quotes.
    # $rCommand = "R.exe -e 'install.packages(\"renv\", repos = \"https://cloud.r-project.org\")'"
    # & $condaPath run -n $condaEnvName $rCommand

    Write-Host "Installation of 'renv' completed."
}
catch {
    Write-Error "Failed to run R command for 'renv' installation. Error: $($_.Exception.Message)"
    pause
    Stop-Transcript
    exit 1
}

# --- Running R command: renv::restore() and renv::install('reticulate') ---
Write-Host "Setting up R packages"
try {
    & $condaPath run -n kiwiflow R.exe -e "renv::restore()"
    & $condaPath run -n kiwiflow R.exe -e "renv::install('reticulate')"

    # $originalPath = $env:PATH
    # $system32Path = "$env:SystemRoot\System32"
    # $rToolsPath = "$RtoolsInstallDir\usr\bin"
    # $condaScriptsPath = (Split-Path -Path $condaPath -Parent)
    # $condaLibraryBinPath = "$condaScriptsPath\..\Library\bin"
    # $tempPath = "$system32Path;$rToolsPath;$condaScriptsPath;$condaLibraryBinPath;$originalPath"
    # [Environment]::SetEnvironmentVariable("Path", $tempPath, [System.EnvironmentVariableTarget]::Process)
    # Write-Host "Temporarily adjusted PATH for R command execution."

    # # KEY CHANGE for renv::install('reticulate')
    # $rCommandReticulate = "R.exe -e 'renv::install(\"reticulate\")'"
    # & $condaPath run -n $condaEnvName $rCommandReticulate

    # # KEY CHANGE for renv::restore()
    # $rCommandRestore = "R.exe -e 'renv::restore()'"
    # & $condaPath run -n $condaEnvName $rCommandRestore

    Write-Host "Setting up R packages completed."
}
catch {
    Write-Error "Failed to run R commands for 'renv'. Error: $($_.Exception.Message)"
    pause
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
}
catch {
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

Write-Host "Setup complete. Check kiwiflow_setup.log in $userDataPath for details."
pause
Stop-Transcript