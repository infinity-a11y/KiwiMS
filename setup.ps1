# Requires admin
# Run as: powershell -ExecutionPolicy Bypass -File setup_kiwiflow.ps1

#-----------------------------#
# Script Initialization
#-----------------------------#
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Admin check
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "This script must be run as Administrator."
    exit 1
}

#-----------------------------#
# FUNCTION Logging
#-----------------------------#
# function Write-Host($message) {
#     $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
#     "$timestamp $message" | Out-File -FilePath $logFile -Append
#     Write-Host "$timestamp $message"
# }

#-----------------------------#
# FUNCTION Download with Retry
#-----------------------------#
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

#-----------------------------#
# Paths and Setup
#-----------------------------#
$userDataPath = "$env:LOCALAPPDATA\KiwiFlow"
$basePath = Split-Path -Parent $MyInvocation.MyCommand.Definition
$appPath = Join-Path $basePath "app"
$tempPath = "$env:TEMP\kiwiflow_setup"
$condaPrefix = "$env:USERPROFILE\miniconda3"
$envName = "kiwiflow"
$condaEnvPath = "$condaPrefix\envs\$envName"
$logFile = "$userDataPath\kiwiflow_setup.log"

Start-Transcript -Path $logFile

if (-Not (Test-Path $tempPath)) {
    New-Item -Path $tempPath -ItemType Directory | Out-Null
}
if (-not (Test-Path $userDataPath)) {
    New-Item -ItemType Directory -Path $userDataPath -Force
}

Write-Host "Base path: $basePath"
Write-Host "App path: $appPath"
Write-Host "Temp path: $tempPath"
Write-Host "Conda base: $condaPrefix"
Write-Host "Conda env path: $condaEnvPath"
Write-Host "Log file: $logFile"

#-----------------------------#
# Set working directory
#-----------------------------#
try {
    Write-Host "Setting working directory to $basePath"
    Set-Location -Path $basePath -ErrorAction Stop
}
catch {
    Write-Host "Error: Failed to set working directory to $basePath. $_"
    exit 1
}

#-----------------------------#
# Install Miniconda if Missing
#-----------------------------#
try {
    if (-Not (Test-Path "$condaPrefix\Scripts\conda.exe")) {
        Write-Host "Installing Miniconda..."
        $minicondaInstaller = "$tempPath\miniconda.exe"
        Download-File "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe" $minicondaInstaller
        Start-Process -Wait -FilePath $minicondaInstaller -ArgumentList "/S", "/D=$condaPrefix"
        $env:Path += ";$env:USERPROFILE\Miniconda3;$env:USERPROFILE\Miniconda3\Scripts;$env:USERPROFILE\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installation completed."   
    }
}
catch {
    Write-Host "Conda installation failed. Exiting."
    exit 1
}

#-----------------------------#
# Conda Presence Check
#-----------------------------#
$condaCmd = "$condaPrefix\Scripts\conda.exe"
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found after installation. Exiting."
    exit 1
}

#-----------------------------#
# Create or Update Conda Env
#-----------------------------#
Write-Host "Creating or updating conda environment..."
try {
    #& $condaCmd env create -f "$basePath\env\environment.yml" -n $envName -p $condaEnvPath -y
    & $condaCmd env create -f "$basePath\resources\environment.yml" -n $envName -y
}
catch {
    Write-Error "Failed to manage conda environment. Error: $($_.Exception.Message)"
    exit 1
}
Write-Host "Conda environment created or updated."

#-----------------------------#
# FUNCTION Ensure Rtools
#-----------------------------#
function Ensure-RTools {
    $rtoolsPath = "C:\rtools44"
    if (-Not (Test-Path $rtoolsPath)) {
        $rtoolsInstaller = "$tempPath\rtools.exe"
        Write-Host "Downloading Rtools..."
        Download-File "https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6459-6401.exe" $rtoolsInstaller
        Write-Host "Installing Rtools..."
        Start-Process -Wait -FilePath $rtoolsInstaller -ArgumentList "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART"
        if (-not ($env:PATH -like "*$($RtoolsInstallDir)\usr\bin*")) {
            $env:PATH = "$RtoolsInstallDir\usr\bin;" + $env:PATH
            Write-Host "Added $($RtoolsInstallDir)\usr\bin to PATH."
        }
        Write-Host "Rtools installation completed."
    }
    else {
        Write-Host "Rtools already installed."
    }
}
Ensure-RTools

#-----------------------------#
# R: install renv
#-----------------------------#
Write-Host "Restoring R packages with renv..."
try {
    & $condaCmd run -n kiwiflow R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
    Write-Host "Installation of 'renv' completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
        Write-Error "Installation of 'renv' completed."
    }
    catch {
        Write-Error "Failed to run R command for 'renv' installation. Error: $($_.Exception.Message)"
        exit 1        
    }
}

#-----------------------------#
# R: renv::restore
#-----------------------------#
Write-Host "Setting up R packages"
try {
    & $condaCmd run -n kiwiflow R.exe -e "renv::restore()"
    Write-Host "Setting up R packages completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "renv::restore()"
        Write-Host "Setting up R packages completed."
    }
    catch {
        Write-Host "Failed to run R commands for 'renv'. Error: $($_.Exception.Message)"
        pause
        Stop-Transcript
        exit 1
    }
}

#-----------------------------#
# R: install reticulate
#-----------------------------#
Write-Host "Setting up reticulate"
try {
    & $condaCmd run -n kiwiflow R.exe -e "renv::install('reticulate')"
    Write-Host "Setting up reticulate completed."
}
catch {
    try {
        Start-Sleep -Seconds 3
        & $condaCmd run -n kiwiflow R.exe -e "renv::install('reticulate')"
        Write-Host "Setting up reticulate completed."
    }
    catch {
        Write-Host "Failed to setup reticulate. Error: $($_.Exception.Message)"
        exit 1
    }
}
Write-Host "R packages restored."

#-----------------------------#
# Create run_app.vbs launcher
#-----------------------------#
Write-Host "Creating run_app.vbs launcher..."

try {
    $vbsPath = "$basePath\run_app.vbs"
    $escapedAppPath = "$basePath\app.R" -replace '\\', '\\'
    $escapedLogPath = "$userDataPath\launch.log" -replace '\\', '\\'
    $escapedCondaExe = $condaCmd -replace '\\', '\\'

    $vbsContent = @"
Option Explicit
Dim WShell, WMI, Process, Processes, IsRunning, PortInUse, CmdLine, LogFile
Dim PopupMsg, PopupTitle, PopupTimeout, AppPath

' Initialize objects
Set WShell = CreateObject("WScript.Shell")
Set WMI = GetObject("winmgmts:{impersonationLevel=impersonate}!\\.\root\cimv2")

' Configuration
PopupTitle = "KiwiFlow"
LogFile = "$escapedLogPath"
AppPath = "$escapedAppPath"
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

If IsRunning And PortInUse Then
    PopupMsg = "KiwiFlow is already running on port 3838. Please use the existing browser tab."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
Else
    PopupMsg = "KiwiFlow will open shortly, please wait..."
    PopupTimeout = 3
    WShell.Popup PopupMsg, PopupTimeout, PopupTitle, 0
    CmdLine = "cmd.exe /c ""$escapedCondaExe run -n kiwiflow Rscript -e ""shiny::runApp('" & AppPath & "', port=3838, launch.browser=TRUE)"" > " & LogFile & " 2>&1"""
    WShell.Run CmdLine, 0
End If

Set WShell = Nothing
Set WMI = Nothing
"@

    Set-Content -Path $vbsPath -Value $vbsContent -Encoding ASCII -ErrorAction Stop
    Write-Host "run_app.vbs created at: $vbsPath"
}
catch {
    Write-Host "Error: Failed to create run_app.vbs: $_"
    exit 1
}

#-----------------------------#
# Create Desktop Shortcut
#-----------------------------#
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
}

Write-Host "Installation complete."
