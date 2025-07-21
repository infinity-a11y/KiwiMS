[Setup]
AppName=KiwiFlow
AppVersion=0.1.0
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiFlow
DefaultGroupName=KiwiFlow
Compression=lzma2
SolidCompression=yes
OutputDir=.\Output
OutputBaseFilename=KiwiFlow_Setup
SetupIconFile=favicon.ico
PrivilegesRequired=admin

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; PowerShell setup scripts
Source: "config.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "miniconda_installer.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "conda_env.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "install_rtools.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "renv_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "renv_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "reticulate_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "launcher_create.ps1"; DestDir: "{app}"; Flags: deleteafterinstall

; R Shiny files
Source: "KiwiFlow_App_Source\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs

; Other files
Source: "favicon.ico"; DestDir: "{app}"; Flags: ignoreversion

[Run]
; Define the shared log file path here as a simple string constant.
; The PowerShell scripts will need to handle creating/appending to it.
#define KiwiFlowLogFile "{localappdata}\KiwiFlow\kiwiflow_setup.log"

; Run config
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\config.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Configuring setup..."; Flags:  shellexec waituntilterminated; Tasks: config_setup; AfterInstall: UpdateProgress(5);

; 1. Install Miniconda
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\miniconda_installer.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing Miniconda (Python Environment)..."; Flags:  shellexec waituntilterminated; Tasks: install_miniconda; AfterInstall: UpdateProgress(25);

; 2. Setup Conda Environment
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\conda_env.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Setting up Conda Environment..."; Flags:  shellexec waituntilterminated; Tasks: setup_conda_env; AfterInstall: UpdateProgress(50);

; 3. Install Rtools
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\install_rtools.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing Rtools (R Build Tools)..."; Flags:  shellexec waituntilterminated; Tasks: install_rtools; AfterInstall: UpdateProgress(65);

; 4. Install R Packages
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing renv..."; Flags:  shellexec waituntilterminated; Tasks: renv_install; AfterInstall: UpdateProgress(70);
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_setup.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing R Packages ..."; Flags:  shellexec waituntilterminated; Tasks: renv_setup; AfterInstall: UpdateProgress(90);
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\reticulate_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Installing reticulate ..."; Flags:  shellexec waituntilterminated; Tasks: reticulate_install; AfterInstall: UpdateProgress(95);

; 5. Create Launchers and Shortcut
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\launcher_create.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "Creating Application Launchers and Desktop Shortcut..."; Flags:  shellexec waituntilterminated; Tasks: create_launchers; AfterInstall: UpdateProgress(100);

; After all steps, potentially launch the app or show info
Filename: "{app}\run_app.vbs"; Description: "{cm:LaunchProgram,KiwiFlow}"; Flags: postinstall skipifsilent shellexec; Tasks: create_launchers

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

[Tasks]
Name: "config_setup"; Description: "Configuring setup"; GroupDescription: "KiwiFlow Components"
Name: "install_miniconda"; Description: "Install Miniconda"; GroupDescription: "KiwiFlow Components"
Name: "setup_conda_env"; Description: "Setup KiwiFlow Conda Environment"; GroupDescription: "KiwiFlow Components"
Name: "install_rtools"; Description: "Install Rtools (R Build Tools)"; GroupDescription: "KiwiFlow Components"
Name: "renv_install"; Description: "Install R Packages and Configure renv"; GroupDescription: "KiwiFlow Components"
Name: "renv_setup"; Description: "Install R Packages and Configure renv"; GroupDescription: "KiwiFlow Components"
Name: "reticulate_install"; Description: "Install R Packages and Configure renv"; GroupDescription: "KiwiFlow Components"
Name: "create_launchers"; Description: "Create Application Launchers and Desktop Shortcut"; GroupDescription: "KiwiFlow Components"

