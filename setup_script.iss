#define KiwiMSLogFile "{localappdata}\KiwiMS\kiwims_setup.log"

[Setup]
AppName=KiwiMS
AppId=KiwiMS
AppVersion=0.5.1
AppPublisher=Marian Freisleben
DefaultDirName={autopf}\KiwiMS
DefaultGroupName=KiwiMS
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=KiwiMS-Windows-x86_64
SetupIconFile=setup\favicon.ico
UninstallDisplayIcon={app}\favicon.ico
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
Source: "KiwiMS_App\KiwiMS.exe"; DestDir: "{app}";
Source: "KiwiMS_App\env_kiwims\*"; DestDir: "{app}\env_kiwims"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "KiwiMS_App\.Rprofile"; DestDir: "{app}";
Source: "KiwiMS_App\.renvignore"; DestDir: "{app}";
Source: "KiwiMS_App\app.R"; DestDir: "{app}";
Source: "KiwiMS_App\config.yml"; DestDir: "{app}";
Source: "KiwiMS_App\renv.lock"; DestDir: "{app}";
Source: "KiwiMS_App\renv\*"; DestDir: "{app}\renv"; Flags: recursesubdirs createallsubdirs
Source: "KiwiMS_App\rhino.yml"; DestDir: "{app}";
Source: "KiwiMS_App\R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: recursesubdirs createallsubdirs
Source: "KiwiMS_App\app\*"; DestDir: "{app}\app"; Flags: recursesubdirs createallsubdirs;
Source: "KiwiMS_App\resources\*"; DestDir: "{app}\resources"; Flags: recursesubdirs createallsubdirs;
Source: "setup\favicon.ico"; DestDir: "{app}"; Flags: ignoreversion
Source: "setup\VC_redist.x64.exe"; DestDir: "{tmp}"; Flags: deleteafterinstall
Source: "setup\config.ps1"; DestDir: "{tmp}"; Flags: deleteafterinstall
Source: "LICENSE"; DestDir: "{app}";

[CustomMessages]
Description_Launch=Launch KiwiMS
ScopeTitle=Select Installation Type
ScopeSub=Who should this application be installed for?
ScopeDesc=Choose how you want to install KiwiMS.
ScopeAllUsers=System-wide for all users (requires admin)
ScopeCurrUser=Current user only
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

procedure CurStepChanged(CurStep: TSetupStep);
var
  ResultCode: Integer;
  LogFile, PsArgs, UnpackCmd: string;
begin
  LogFile := ExpandConstant('{#KiwiMSLogFile}');

  if CurStep = ssPostInstall then
  begin
    // Step 1: Run config.ps1 — initialises the log file and user data directory
    UpdateStatus('Configuring KiwiMS...');
    UpdateProgress(80);
    PsArgs := '-NonInteractive -ExecutionPolicy Bypass -File "' + ExpandConstant('{tmp}\config.ps1') + '"'
            + ' -basePath "' + ExpandConstant('{app}') + '"'
            + ' -userDataPath "' + ExpandConstant('{localappdata}\KiwiMS') + '"'
            + ' -envName "kiwims"'
            + ' -logFile "' + LogFile + '"'
            + ' -installScope "' + SelectedScope + '"';
    Exec('powershell.exe', PsArgs, '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

    // Step 1.5: Install VC++ 2015-2022 Redistributable — required by Python and
    // conda-unpack.exe (VCRUNTIME140.dll). Exit code 1638 means already installed.
    UpdateStatus('Installing Visual C++ runtime...');
    UpdateProgress(82);
    Exec(ExpandConstant('{tmp}\VC_redist.x64.exe'),
         '/quiet /norestart', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
    SaveStringToFile(LogFile,
      '[INFO] VC++ redistributable exit code: ' + IntToStr(ResultCode) + #13#10, True);

    // Step 2: Run conda-unpack.exe via PowerShell so output is captured in the log
    UpdateStatus('Finalizing portable environment ...');
    UpdateProgress(90);
    UnpackCmd := '& ' + Chr(39) + ExpandConstant('{app}\env_kiwims\Scripts\conda-unpack.exe') + Chr(39)
               + ' *>&1 | Add-Content -Path ' + Chr(39) + LogFile + Chr(39)
               + '; exit $LASTEXITCODE';
    PsArgs := '-NonInteractive -ExecutionPolicy Bypass -Command ' + Chr(34) + UnpackCmd + Chr(34);

    if not Exec('powershell.exe', PsArgs, ExpandConstant('{app}'), SW_HIDE, ewWaitUntilTerminated, ResultCode)
       or (ResultCode <> 0) then
    begin
      SaveStringToFile(LogFile,
        '[ERROR] conda-unpack.exe failed with exit code: ' + IntToStr(ResultCode) + #13#10, True);
      MsgBox('Failed to finalize the environment. Please check write permissions.', mbError, MB_OK);
      InstallationFailed := True;
    end else
    begin
      SaveStringToFile(LogFile, '[OK] conda-unpack.exe completed successfully.' + #13#10, True);
    end;

    // Remove conda-meta/ so reticulate does not detect env_kiwims as a conda environment
    UpdateStatus('Cleaning up environment metadata ...');
    DelTree(ExpandConstant('{app}\env_kiwims\conda-meta'), True, True, True);
    SaveStringToFile(LogFile, '[OK] conda-meta removed.' + #13#10, True);

    UpdateProgress(100);
    if InstallationFailed then Abort;
  end;
end;