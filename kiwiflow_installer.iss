[Setup]
AppName=KiwiFlow
AppId=KiwiFlow
AppVersion=0.3.0
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiFlow
DisableDirPage=yes
DefaultGroupName=KiwiFlow
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=KiwiFlow_2026-01-28_Setup
SetupIconFile=setup\favicon.ico
PrivilegesRequired=admin
FlatComponentsList=no
WizardImageFile=setup\kiwiflow_Banner.bmp
WizardSmallImageFile=setup\kiwiflow_small.bmp
WizardStyle=modern
AlwaysShowDirOnReadyPage=yes
CloseApplications=yes

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Files]
; Setup scripts
Source: "setup\config.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\functions.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\miniconda_installer.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\conda_env.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\rtools_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\setup_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\quarto_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\summarize_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall

; App files
Source: "KiwiFlow_App\KiwiFlow.exe"; DestDir: "{app}";
Source: "KiwiFlow_App\update.exe"; DestDir: "{app}";
Source: "KiwiFlow_App\app.R"; DestDir: "{app}";
Source: "KiwiFlow_App\config.yml"; DestDir: "{app}";
Source: "KiwiFlow_App\renv.lock"; DestDir: "{app}";
Source: "KiwiFlow_App\renv\activate.R"; DestDir: "{app}\renv";
Source: "KiwiFlow_App\rhino.yml"; DestDir: "{app}";
Source: "KiwiFlow_App\app\*"; DestDir: "{app}\app"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiFlow_App\dev\*"; DestDir: "{app}\dev"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiFlow_App\resources\*"; DestDir: "{app}\resources"; Flags: recursesubdirs createallsubdirs;

; Other
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion

[CustomMessages]
; English Messages (default)
StatusMsg_Configuring=Configuring setup...
StatusMsg_InstallMiniconda=Installing Miniconda (Python Environment)...
StatusMsg_SetupCondaEnv=Setting up Conda Environment...
StatusMsg_SetupRtools=Setting up rtools44...
StatusMsg_InstallRenv=Installing renv package (R environment setup phase 1/2)...
StatusMsg_RestoreRenv=Restoring R packages (renv environment setup phase 2/2)...
StatusMsg_InstallQuarto=Installing Quarto...
StatusMsg_SummarizeSetup=Summarizing setup...
Icons_Comment=Launch the KiwiFlow Application
Description_Launch=Launch KiwiFlow

; German Messages
de.StatusMsg_Configuring=Setup wird konfiguriert...
de.StatusMsg_InstallMiniconda=Miniconda wird installiert (Python Umgebung)...
de.StatusMsg_SetupCondaEnv=Conda Umgebung wird eingerichtet...
de.StatusMsg_SetupRtools=Installiere rtools44...
de.StatusMsg_InstallRenv=renv Paket wird installiert (R Umgebung Einrichtung Phase 1/2)...
de.StatusMsg_RestoreRenv=R-Pakete werden wiederhergestellt (renv Umgebung Einrichtung Phase 2/2)...
de.StatusMsg_InstallQuarto=Quarto wird installiert...
de.StatusMsg_SummarizeSetup=Setup wird zusammengefasst...
de.Icons_Comment=KiwiFlow Anwendung starten
de.Description_Launch=KiwiFlow starten

[Run]
#define KiwiFlowLogFile "{localappdata}\KiwiFlow\kiwiflow_setup.log"

; Run config
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\config.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_Configuring}"; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(5);

; 1. Install Miniconda
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\miniconda_installer.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_InstallMiniconda}"; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(20);

; 2. Setup Conda Environment
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\conda_env.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_SetupCondaEnv}"; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(40);

; 2. Setup Conda Environment
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\rtools_setup.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_SetupRtools}"; Flags: runhidden shellexec waituntilterminated; AfterInstall: UpdateProgress(55);

; 4. Install R Packages
; 4a. Install renv package
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_InstallRenv}"; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(60);

; 4b. Restore renv environment
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\renv_setup.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_RestoreRenv}"; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(85);

; 5. Install Quarto
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\quarto_install.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_InstallQuarto}"; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(95);

; 6. Summarize setup
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\summarize_setup.ps1"" -basePath ""{app}"" -userDataPath ""{localappdata}\KiwiFlow"" -envName ""kiwiflow"" -logFile ""{#KiwiFlowLogFile}"""; WorkingDir: "{app}"; StatusMsg: "{cm:StatusMsg_SummarizeSetup}"; Flags: shellexec waituntilterminated runhidden; AfterInstall: UpdateProgress(100);

; After all steps, potentially launch the app or show info
Filename: "{app}\KiwiFlow.exe"; Description: "{cm:Description_Launch}"; Flags: postinstall skipifsilent shellexec;

[Icons]
; Creates a shortcut in the Start Menu Programs group
Name: "{group}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"; Comment: "{cm:Icons_Comment}";

; Creates a desktop shortcut
Name: "{userdesktop}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"; Comment: "{cm:Icons_Comment}";

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

