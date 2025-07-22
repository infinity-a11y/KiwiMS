[Setup]
AppName=KiwiFlow
AppVersion=0.1.0
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiFlow
DefaultGroupName=KiwiFlow
Compression=lzma2
SolidCompression=yes
OutputDir=.\Output
OutputBaseFilename=KiwiFlow_2025-07-22_Setup
SetupIconFile=setup\favicon.ico
PrivilegesRequired=admin

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Files]
; Setup scripts
Source: "setup\config.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\functions.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\miniconda_installer.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\conda_env.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_rtools.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\setup_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_reticulate.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\reticulate_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall

; App files
Source: "KiwiFlow_App\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs

; Other
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion

[Run]
#define KiwiFlowLogFile "{localappdata}\KiwiFlow\kiwiflow_setup.log"

; Run config
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\config.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Configuring setup..."; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(5);

; 1. Install Miniconda
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\miniconda_installer.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing Miniconda (Python Environment)..."; Flags:  runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(25);

; 2. Setup Conda Environment
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\conda_env.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Setting up Conda Environment..."; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(50);

; 3. Install Rtools
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\install_rtools.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing Rtools (R Build Tools)..."; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(65);

; 4. Install R Packages
; 4a. Install renv package
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing renv package (R environment setup phase 1/3)..."; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(70);

; 4b. Restore renv environment using dedicated script
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_setup.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Restoring R packages (renv environment setup phase 2/3)..."; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(90);

; 4c. Install reticulate
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\reticulate_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing reticulate (R environment setup phase 3/3)..."; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(100);

; After all steps, potentially launch the app or show info
Filename: "{app}\KiwiFlow.exe"; Description: "{cm:LaunchProgram,KiwiFlow}"; Flags: postinstall skipifsilent shellexec;

[Code]

procedure SetProgressMax(Ratio: Integer);
begin
  WizardForm.ProgressGauge.Max := WizardForm.ProgressGauge.Max * Ratio;
end;

procedure UpdateProgress(Position: Integer);
begin
  WizardForm.ProgressGauge.Position :=
    Position * WizardForm.ProgressGauge.Max div 100;
end;

[Icons]
; Creates a shortcut in the Start Menu Programs group
Name: "{group}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\setup\favicon.ico"; Comment: "Launch the KiwiFlow Application";

; Creates a desktop shortcut
Name: "{userdesktop}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\setup\favicon.ico"; Comment: "Launch the KiwiFlow Application";
