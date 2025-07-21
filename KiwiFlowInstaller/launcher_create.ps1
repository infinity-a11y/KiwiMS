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
Start-Transcript -Path $logFile -Append

Write-Host "basePath: $basePath"
Write-Host "userDataPath: $userDataPath"
Write-Host "envName: $envName"
Write-Host "logFile: $logFile"

$condaPrefix = "$env:ProgramData\miniconda3"

#-----------------------------#
# Conda Presence Check
#-----------------------------#
$condaCmd = "$condaPrefix\Scripts\conda.exe"
if (-Not (Test-Path $condaCmd)) {
    Write-Host "Conda not found after installation. Exiting."
    exit 1
}

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

Write-Host "Launchers created."