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
PrivilegesRequiredOverridesAllowed=commandline
FlatComponentsList=no
WizardImageFile=setup\kiwiflow_Banner.bmp
WizardSmallImageFile=setup\kiwiflow_small.bmp
WizardStyle=modern
AlwaysShowDirOnReadyPage=yes
CloseApplications=yes
SetupLogging=yes

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
StatusMsg_SetupRtools=Setting up rtools45...
StatusMsg_InstallRenv=Installing renv package (R environment setup phase 1/2)...
StatusMsg_RestoreRenv=Restoring R packages (renv environment setup phase 2/2)...
StatusMsg_InstallQuarto=Installing Quarto...
Icons_Comment=Launch the KiwiFlow Application
Description_Launch=Launch KiwiFlow

; German Messages
de.StatusMsg_Configuring=Setup wird konfiguriert...
de.StatusMsg_InstallMiniconda=Miniconda wird installiert (Python Umgebung)...
de.StatusMsg_SetupCondaEnv=Conda Umgebung wird eingerichtet...
de.StatusMsg_SetupRtools=Installiere rtools45...
de.StatusMsg_InstallRenv=renv Paket wird installiert (R Umgebung Einrichtung Phase 1/2)...
de.StatusMsg_RestoreRenv=R-Pakete werden wiederhergestellt (renv Umgebung Einrichtung Phase 2/2)...
de.StatusMsg_InstallQuarto=Quarto wird installiert...
de.Icons_Comment=KiwiFlow Anwendung starten
de.Description_Launch=KiwiFlow starten

; Installation scope
ScopeTitle=Select Installation Type
ScopeSub=Who should this application be installed for?
ScopeDesc=Choose how you want to install KiwiFlow.
ScopeAllUsers=System-wide for all users (requires administrator rights)
ScopeCurrUser=Current user only
de.ScopeTitle=Installationstyp auswählen
de.ScopeSub=Für wen soll diese Anwendung installiert werden?
de.ScopeDesc=Wählen Sie aus, wie Sie KiwiFlow installieren möchten.
de.ScopeAllUsers=Systemweit für alle Benutzer (erfordert Administratorrechte)
de.ScopeCurrUser=Nur für den aktuellen Benutzer

[Run]
#define KiwiFlowLogFile "{localappdata}\KiwiFlow\kiwiflow_setup.log"

; Post Install
Filename: "{app}\KiwiFlow.exe"; Description: "{cm:Description_Launch}"; Flags: postinstall skipifsilent shellexec;

[Icons]
; Create shortcut in Start Menu Programs group
Name: "{group}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"; Comment: "{cm:Icons_Comment}";

; Create desktop shortcut
Name: "{userdesktop}\KiwiFlow"; Filename: "{app}\KiwiFlow.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"; Comment: "{cm:Icons_Comment}";

[Code]

// Progress bar
procedure SetProgressMax(Ratio: Integer);
begin
  WizardForm.ProgressGauge.Max := WizardForm.ProgressGauge.Max * Ratio;
end;

// Update progress bar
procedure UpdateProgress(Position: Integer);
begin
  WizardForm.ProgressGauge.Position :=
    Position * WizardForm.ProgressGauge.Max div 100;
end;

var
  InstallScopePage: TInputOptionWizardPage;

procedure InitializeWizard;
begin
  // Create the input option page using the IDs defined in [CustomMessages]
  InstallScopePage := CreateInputOptionPage(wpWelcome,
    CustomMessage('ScopeTitle'), 
    CustomMessage('ScopeSub'),
    CustomMessage('ScopeDesc'),
    True, False);

  // Add the two options (Index 0 and Index 1)
  InstallScopePage.Add(CustomMessage('ScopeAllUsers'));
  InstallScopePage.Add(CustomMessage('ScopeCurrUser'));

  // Default the selection to 'Current user only' (Index 1)
  InstallScopePage.Values[1] := True;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  if CurPageID = InstallScopePage.ID then
  begin
    if InstallScopePage.Values[0] then
    begin
      // All users → force admin mode
      WizardForm.DirEdit.Text := ExpandConstant('{pf}\KiwiFlow');
      // You can also set registry root later
    end else
    begin
      WizardForm.DirEdit.Text := ExpandConstant('{localappdata}\KiwiFlow');
    end;
  end;
  Result := True;
end;


// Get install scope (system or user wide install)
function GetInstallScope: string;
begin
  if InstallScopePage.Values[0] then
    Result := 'allusers'
  else
    Result := 'currentuser';
end;

// Check ps1 exit codes
procedure CheckPowerShellResult;
var
  ResultCode: Integer;
begin
  ResultCode := WizardForm.ProgressGauge.Tag; 
  if ResultCode <> 0 then
  begin
    MsgBox('Installation step failed with error code: ' + IntToStr(ResultCode) + #13#10 +
           'Please check the log file: ' + ExpandConstant('{#KiwiFlowLogFile}'),
           mbError, MB_OK);
    WizardForm.Close;
    // or Abort;
  end;
end;

// Run Config
procedure RunConfigStep;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope; 

  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\config.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';
            
  UpdateProgress(5);

  Log('Full command line: powershell.exe ' + Params);
  
  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_Configuring');
  
  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch config.ps1');
    MsgBox('Failed to start PowerShell.exe.', mbError, MB_OK);
    Abort;
  end;

  Log('config.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('config.ps1 failed with exit code ' + IntToStr(ResultCode), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(10);
end;

// Miniconda setup
procedure RunMinicondaInstall;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;
  
  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\miniconda_installer.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  Log('Full command for miniconda_installer.ps1: powershell.exe ' + Params);
  
  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_InstallMiniconda');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch miniconda_installer.ps1');
    MsgBox('Failed to start PowerShell for Miniconda installation step', mbError, MB_OK);
    Abort;
  end;

  Log('miniconda_installer.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('miniconda_installer.ps1 failed with exit code ' + IntToStr(ResultCode) + #13#10 +
           'Check the log file:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(20);
end;

// Make conda environment
procedure RunCondaEnvSetup;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;
  
  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\conda_env.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  Log('Full command for conda_env.ps1: powershell.exe ' + Params);
  Log('Install scope passed: ' + Scope);

  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_SetupCondaEnv');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,                    
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch conda_env.ps1');
    MsgBox('Failed to start PowerShell for Conda environment setup', mbError, MB_OK);
    Abort;
  end;

  Log('conda_env.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('Conda environment setup failed (exit code: ' + IntToStr(ResultCode) + ').' + #13#10#13#10 +
           'This step tried to create/update the Conda environment.' + #13#10 +
           'Common causes:' + #13#10 +
           ' • Miniconda installation issues' + #13#10 +
           ' • Network problems during package download' + #13#10 +
           ' • Disk space or permissions issues' + #13#10#13#10 +
           'Please check the log file for details:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(40);
end;

// RTools setup
procedure RunRtoolsSetup;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;
  
  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\rtools_setup.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_SetupRtools');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch rtools_setup.ps1');
    MsgBox('Failed to start PowerShell for Rtools setup step', mbError, MB_OK);
    Abort;
  end;

  Log('rtools_setup.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('Rtools setup failed (exit code: ' + IntToStr(ResultCode) + ').' + #13#10#13#10 +
           'The installer tried to download and install Rtools 45.' + #13#10 +
           'Possible reasons:' + #13#10 +
           ' • Download failed (network issue)' + #13#10 +
           ' • Installer returned non-zero exit code' + #13#10 +
           ' • Insufficient permissions' + #13#10 +
           ' • Antivirus/software blocked the installer' + #13#10#13#10 +
           'Please check the log file for more details:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(55);
end;

// Install renv
procedure RunRenvInstall;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;
  
  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\renv_install.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  Log('Full command for renv_install.ps1: powershell.exe ' + Params);
  Log('Install scope passed: ' + Scope);

  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_InstallRenv');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch renv_install.ps1');
    MsgBox('Failed to start PowerShell for renv installation step', mbError, MB_OK);
    Abort;
  end;

  Log('renv_install.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('renv installation failed (exit code: ' + IntToStr(ResultCode) + ').' + #13#10#13#10 +
           'This step installs the renv package in the Conda environment.' + #13#10 +
           'Please check the log file for details:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(60);
end;

// Renv setup
procedure RunRenvSetup;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;

  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope = \"currentuser\"); ' +
            '& \"' + ExpandConstant('{app}\renv_setup.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  Log('Full command for renv_setup.ps1: powershell.exe ' + Params);
  Log('Install scope passed: ' + Scope);

  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_RestoreRenv');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch renv_setup.ps1');
    MsgBox('Failed to start PowerShell for renv restore step', mbError, MB_OK);
    Abort;
  end;

  Log('renv_setup.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('renv restore failed (exit code: ' + IntToStr(ResultCode) + ').' + #13#10#13#10 +
           'This step restores the R packages using renv::restore.' + #13#10 +
           'Please check the log file for details:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(85);
end;


// Quarto setup
procedure RunQuartoInstall;
var
  Params: string;
  ResultCode: Integer;
  Scope: string;
begin
  Scope := GetInstallScope;
  
  Params := '-ExecutionPolicy Bypass -Command "& { ' +
            'param([string]$basePath, [string]$userDataPath, [string]$envName, [string]$logFile, [string]$installScope); ' +
            '& \"' + ExpandConstant('{app}\quarto_install.ps1') + '\" ' +
            '-basePath \"' + ExpandConstant('{app}') + '\" ' +
            '-userDataPath \"' + ExpandConstant('{localappdata}\KiwiFlow') + '\" ' +
            '-envName \"kiwiflow\" ' +
            '-logFile \"' + ExpandConstant('{#KiwiFlowLogFile}') + '\" ' +
            '-installScope \"' + Scope + '\" }"';

  Log('Full command for quarto_install.ps1: powershell.exe ' + Params);
  Log('Install scope passed: ' + Scope);

  WizardForm.StatusLabel.Caption := CustomMessage('StatusMsg_InstallQuarto');

  if not Exec('powershell.exe', Params,
              ExpandConstant('{app}'),
              SW_HIDE,                     
              ewWaitUntilTerminated,
              ResultCode) then
  begin
    Log('Exec failed to launch quarto_install.ps1');
    MsgBox('Failed to start PowerShell for Quarto installation', mbError, MB_OK);
    Abort;
  end;

  Log('quarto_install.ps1 exited with code: ' + IntToStr(ResultCode));

  if ResultCode <> 0 then
  begin
    MsgBox('Quarto installation failed (exit code: ' + IntToStr(ResultCode) + ').' + #13#10#13#10 +
           'This step installs Quarto CLI.' + #13#10 +
           'Please check the log file for details:' + #13#10 +
           ExpandConstant('{#KiwiFlowLogFile}'), mbError, MB_OK);
    Abort;
  end;

  UpdateProgress(100);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    // 10% - Initial configuration (folders, reports, etc.)
    RunConfigStep;

    // 20% - Install Miniconda (Python environment)
    RunMinicondaInstall;

    // 40% - Create/update Conda environment
    RunCondaEnvSetup;

    // 55% - Install / configure Rtools
    RunRtoolsSetup;

    // 60% - Install renv package
    RunRenvInstall;

    // 85% - Restore renv environment (install all R packages from renv.lock)
    RunRenvSetup;

    // 100% - Install Quarto CLI
    RunQuartoInstall;

    Sleep(800);
  end;
end;
