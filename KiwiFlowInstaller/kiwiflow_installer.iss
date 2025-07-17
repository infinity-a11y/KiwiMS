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
; Main PowerShell setup script
Source: "setup.ps1"; DestDir: "{app}";

; All the R Shiny app files from the 'KiwiFlow_App_Source' directory
; This line tells Inno Setup to copy everything from 'KiwiFlow_App_Source'
; into the user's chosen installation directory ({app}).
; The '*' acts as a wildcard, meaning "all files and folders".
Source: "KiwiFlow_App_Source\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs

; --- OPTIONAL: Refinements and Exclusions ---

; If there are specific files you want to exclude from the source directory,
; you can use the 'Excludes' flag. For example, to exclude .Rproj files
; and a '.git' directory if it somehow ended up in your source folder:
; Source: "KiwiFlow_App_Source\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs; Excludes: "*.Rproj, .git\*"

; If you have other specific files at the root of your installer project
; that should go into {app}, list them like this:
; Source: "path\to\License.txt"; DestDir: "{app}";
; Source: "path\to\ReadMe.md"; DestDir: "{app}";

[Icons]
; This will be handled by your PowerShell script, but you could create one here too
; Name: "{group}\KiwiFlow"; Filename: "{app}\run_app.vbs"; WorkingDir: "{app}"; IconFilename: "{app}\app\static\favicon.ico"

[Tasks]
; Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: checked

[Run]
; Execute the PowerShell script.
; This will run after all files are copied.
; You might want to display a custom page before this step.
Filename: "powershell.exe"; Parameters: "-ExecutionPolicy Bypass -File ""{app}\setup.ps1"""; StatusMsg: "Installing KiwiFlow. This may take some time..."; Flags: 

[Code]
var
  InstallationProgressPage: TOutputProgressWizardPage;

procedure InitializeWizard;
begin
  InstallationProgressPage := CreateOutputProgressPage('KiwiFlow Installation', 'Please wait while KiwiFlow is being installed. This process may take several minutes depending on your system and internet speed.');
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  // Corrected line: Use .ID instead of .PageID
  if CurPageID = InstallationProgressPage.ID then
  begin
    // When this page is displayed, you can update its built-in label and progress bar.
    // For now, we're relying on the [Run] section's StatusMsg for the main feedback.
    // If you were to run a process directly here and want to show progress:
    // InstallationProgressPage.SetProgress(0, 100); // Set initial progress
    // InstallationProgressPage.SetText('Starting installation process...', False); // Update the status message
  end;
end;

procedure CurPageBack(CurPageID: Integer);
begin
  // Handle back button if needed
end;

procedure DeinitializeSetup;
begin
  // Clean up temporary files created by the installer if not already handled by Flags: deleteafterinstall
end;