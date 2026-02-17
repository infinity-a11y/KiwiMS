#define KiwiMSLogFile "{localappdata}\KiwiMS\kiwims_setup.log"

[Setup]
AppName=KiwiMS
AppId=KiwiMS
AppVersion=0.3.1
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiMS
DefaultGroupName=KiwiMS
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=KiwiMS-Windows-x86_64
SetupIconFile=setup\favicon.ico
WizardImageFile=setup\kiwims_banner.bmp
WizardSmallImageFile=setup\kiwims_small.bmp
PrivilegesRequired=none
PrivilegesRequiredOverridesAllowed=commandline
WizardStyle=modern
SetupLogging=yes

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "de"; MessagesFile: "compiler:Languages\German.isl"

[Files]
Source: "setup\config.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\functions.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\miniforge_installer.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\conda_env.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\rtools_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\install_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\setup_renv.R"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\renv_setup.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\quarto_install.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "setup\diagnosis.ps1"; DestDir: "{app}"; Flags: deleteafterinstall
Source: "KiwiMS_App\KiwiMS.exe"; DestDir: "{app}";
Source: "KiwiMS_App\update.exe"; DestDir: "{app}";
Source: "KiwiMS_App\app.R"; DestDir: "{app}";
Source: "KiwiMS_App\config.yml"; DestDir: "{app}";
Source: "KiwiMS_App\renv.lock"; DestDir: "{app}";
Source: "KiwiMS_App\renv\activate.R"; DestDir: "{app}\renv";
Source: "KiwiMS_App\rhino.yml"; DestDir: "{app}";
Source: "KiwiMS_App\app\*"; DestDir: "{app}\app"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiMS_App\dev\*"; DestDir: "{app}\dev"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiMS_App\resources\*"; DestDir: "{app}\resources"; Flags: recursesubdirs createallsubdirs;
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion

[CustomMessages]
StatusMsg_Configuring=Configuring setup...
StatusMsg_InstallMiniconda=Installing Miniconda...
StatusMsg_SetupCondaEnv=Setting up Conda Environment...
StatusMsg_SetupRtools=Setting up rtools45...
StatusMsg_InstallRenv=Installing renv (1/2)...
StatusMsg_RestoreRenv=Restoring R packages (2/2)...
StatusMsg_InstallQuarto=Installing Quarto...
StatusMsg_Diagnosis=Concluding...
Description_Launch=Launch KiwiMS
ScopeTitle=Select Installation Type
ScopeSub=Who should this application be installed for?
ScopeDesc=Choose how you want to install KiwiMS.
ScopeAllUsers=System-wide for all users (requires admin)
ScopeCurrUser=Current user only
de.StatusMsg_Configuring=Setup wird konfiguriert...
de.StatusMsg_InstallMiniconda=Miniconda wird installiert...
de.StatusMsg_SetupCondaEnv=Conda Umgebung wird eingerichtet...
de.StatusMsg_SetupRtools=Installiere rtools45...
de.StatusMsg_InstallRenv=renv wird installiert (1/2)...
de.StatusMsg_RestoreRenv=R-Pakete werden wiederhergestellt (2/2)...
de.StatusMsg_InstallQuarto=Quarto wird installiert...
de.StatusMsg_Diagnosis=Fertigstellen...
de.Description_Launch=KiwiMS starten
de.ScopeTitle=Installationstyp auswählen
de.ScopeSub=Für wen soll diese Anwendung installiert werden?
de.ScopeDesc=Wählen Sie aus, wie Sie KiwiMS installieren möchten.
de.ScopeAllUsers=Systemweit für alle Benutzer (erfordert Admin)
de.ScopeCurrUser=Nur für den aktuellen Benutzer

[Run]
Filename: "{app}\KiwiMS.exe"; Description: "{cm:Description_Launch}"; Flags: postinstall skipifsilent shellexec;

[Icons]
Name: "{group}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"
Name: "{userdesktop}\KiwiMS"; Filename: "{app}\KiwiMS.exe"; WorkingDir: "{app}"; IconFilename: "{app}\favicon.ico"

[Code]
var
  InstallScopePage: TInputOptionWizardPage;
  SelectedScope: string; 
  InstallationFailed: Boolean;

procedure UpdateProgress(Position: Integer);
begin
  if (not WizardSilent) and (WizardForm <> nil) then
    WizardForm.ProgressGauge.Position := Position * WizardForm.ProgressGauge.Max div 100;
end;

procedure UpdateStatus(Msg: string);
begin
  if (not WizardSilent) and (WizardForm <> nil) then
    WizardForm.StatusLabel.Caption := Msg;
end;

function GetInstallScope(Param: string): string;
begin
  if WizardSilent then
  begin
    if IsAdminInstallMode then Result := 'allusers' else Result := 'currentuser';
  end
  else Result := SelectedScope;
end;

procedure InitializeWizard;
begin
  SelectedScope := 'currentuser';
  InstallationFailed := False;
  if not WizardSilent then
  begin
    InstallScopePage := CreateInputOptionPage(wpWelcome, CustomMessage('ScopeTitle'), CustomMessage('ScopeSub'), CustomMessage('ScopeDesc'), True, False);
    InstallScopePage.Add(CustomMessage('ScopeAllUsers'));
    InstallScopePage.Add(CustomMessage('ScopeCurrUser'));
    InstallScopePage.Values[1] := True;
  end;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if (not WizardSilent) and (InstallScopePage <> nil) and (CurPageID = InstallScopePage.ID) then
  begin
    if InstallScopePage.Values[0] then
    begin
      SelectedScope := 'allusers';
      WizardForm.DirEdit.Text := ExpandConstant('{pf}\KiwiMS');
    end else
    begin
      SelectedScope := 'currentuser';
      WizardForm.DirEdit.Text := ExpandConstant('{localappdata}\KiwiMS');
    end;
  end;
end;

procedure RunStep(CaptionMsg: string; ScriptName: string; ProgressPos: Integer);
var
  ResultCode: Integer;
  PSArgs: string;
begin
  if InstallationFailed then Exit;

  UpdateStatus(CaptionMsg);
  UpdateProgress(ProgressPos);
  
  PSArgs := Format('-ExecutionPolicy Bypass -File "%s" -basePath "%s" -userDataPath "%s" -envName "kiwims" -logFile "%s" -installScope "%s"', [ExpandConstant('{app}\') + ScriptName, ExpandConstant('{app}'), ExpandConstant('{localappdata}\KiwiMS'), ExpandConstant('{#KiwiMSLogFile}'), GetInstallScope('')]);

  if Exec('powershell.exe', PSArgs, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    if ResultCode <> 0 then
    begin
      Log('FATAL: ' + ScriptName + ' failed with code ' + IntToStr(ResultCode));
      InstallationFailed := True;
      if not WizardSilent then
        MsgBox(ScriptName + ' failed. See log: ' + ExpandConstant('{#KiwiMSLogFile}'), mbError, MB_OK);
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
  
    RunStep(CustomMessage('StatusMsg_Configuring'),       'config.ps1', 10);
    RunStep(CustomMessage('StatusMsg_InstallMiniconda'),  'miniforge_installer.ps1', 20);
    RunStep(CustomMessage('StatusMsg_SetupCondaEnv'),     'conda_env.ps1', 40);
    RunStep(CustomMessage('StatusMsg_SetupRtools'),       'rtools_setup.ps1', 55);
    RunStep(CustomMessage('StatusMsg_InstallRenv'),       'renv_install.ps1', 60);
    RunStep(CustomMessage('StatusMsg_RestoreRenv'),       'renv_setup.ps1', 80);
    RunStep(CustomMessage('StatusMsg_InstallQuarto'),     'quarto_install.ps1', 90);
    RunStep(CustomMessage('StatusMsg_Diagnosis'),         'diagnosis.ps1', 95);

    if InstallationFailed then Abort;
  end;
end;