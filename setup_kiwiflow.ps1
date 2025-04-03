# setup_kiwiflow.ps1
Start-Transcript -Path "C:\Users\$env:USERNAME\Desktop\KiwiFlow\kiwiflow_setup.log" -Append
Write-Host "Setting up KiwiFlow environment..."

# Check if running as Administrator
function Test-Admin {
    $currentUser = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
    return $currentUser.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

if (-not (Test-Admin)) {
    Write-Host "This script requires Administrator privileges to install Miniconda to C:\. Attempting to elevate..."
    try {
        Start-Process powershell -Verb RunAs -ArgumentList "-File `"$PSCommandPath`"" -Wait -ErrorAction Stop
        Write-Host "Elevation successful. Script will continue in elevated window."
        exit
    } catch {
        Write-Host "Error: Failed to elevate privileges. $_"
        Write-Host "Falling back to user directory installation."
    }
}

# Check if Conda is installed
$condaPath = (Get-Command conda -ErrorAction SilentlyContinue).Source
if (-not $condaPath) {
    Write-Host "Conda not found. Downloading and installing Miniconda..."
    $minicondaUrl = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe"
    $installerPath = "$env:USERPROFILE\Downloads\Miniconda3-latest.exe"

    try {
        Invoke-WebRequest -Uri $minicondaUrl -OutFile $installerPath -ErrorAction Stop
        Write-Host "Miniconda downloaded successfully."
    } catch {
        Write-Host "Error: Failed to download Miniconda. $_"
        pause
        exit 1
    }

    # Try installing to C:\Miniconda3
    Write-Host "Installing Miniconda to C:\Miniconda3 (requires admin)..."
    try {
        Start-Process -FilePath $installerPath -ArgumentList "/S /AddToPath=1 /D=C:\Miniconda3" -Wait -NoNewWindow -ErrorAction Stop
        $env:Path += ";C:\Miniconda3;C:\Miniconda3\Scripts;C:\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installed to C:\Miniconda3."
    } catch {
        Write-Host "Failed to install to C:\Miniconda3: $_"
        Write-Host "Trying user directory instead..."
        Remove-Item $installerPath -ErrorAction SilentlyContinue
        Invoke-WebRequest -Uri $minicondaUrl -OutFile $installerPath -ErrorAction Stop
        Start-Process -FilePath $installerPath -ArgumentList "/S /AddToPath=1 /D=$env:USERPROFILE\Miniconda3" -Wait -NoNewWindow -ErrorAction Stop
        $env:Path += ";$env:USERPROFILE\Miniconda3;$env:USERPROFILE\Miniconda3\Scripts;$env:USERPROFILE\Miniconda3\Library\bin"
        [Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
        Write-Host "Miniconda installed to $env:USERPROFILE\Miniconda3."
    }
    Remove-Item $installerPath -ErrorAction SilentlyContinue

    # Refresh PATH
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "User")
    if (-not (Get-Command conda -ErrorAction SilentlyContinue)) {
        Write-Host "Error: Conda installation failed. Please install manually from https://docs.conda.io/en/latest/miniconda.html"
        pause
        exit 1
    }
    Write-Host "Miniconda installed successfully."
} else {
    Write-Host "Conda found at $condaPath"
}

# Ensure Conda is initialized and reload shell
Write-Host "Initializing Conda for PowerShell..."
try {
    & conda init powershell
    Write-Host "Conda initialized. Reloading shell environment..."
    $condaHook = "C:\Miniconda3\shell\condabin\conda-hook.ps1"
    if (Test-Path $condaHook) {
        . $condaHook
    } else {
        Write-Host "Warning: conda-hook.ps1 not found. May need manual shell restart after script."
    }
} catch {
    Write-Host "Error: Failed to initialize Conda. $_"
    pause
    exit 1
}

# Set working directory
Set-Location "C:\Users\$env:USERNAME\Desktop\KiwiFlow"

# Create or update kiwiflow environment
Write-Host "Creating/updating kiwiflow environment..."
try {
    # Remove existing environment if it exists
    & conda env remove -n kiwiflow -q
    # Create new environment
    & conda env create -f environment.yml --yes
    if ($LASTEXITCODE -ne 0) { throw "Conda environment creation failed with exit code $LASTEXITCODE." }
    Write-Host "Environment created successfully."
} catch {
    Write-Host "Error: Failed to create environment. $_"
    Write-Host "Run 'conda env create -f environment.yml --yes' manually to debug."
    pause
    exit 1
}

# Activate environment
Write-Host "Activating kiwiflow environment..."
try {
    & "C:\Miniconda3\condabin\conda" activate kiwiflow
    if ($LASTEXITCODE -ne 0) { throw "Activation failed with exit code $LASTEXITCODE." }
    Write-Host "Environment activated."
} catch {
    Write-Host "Error: Failed to activate environment. $_"
    Write-Host "Try closing and reopening PowerShell, then run 'conda activate kiwiflow'."
    pause
    exit 1
}

# Verify key packages
Write-Host "Verifying key packages..."
try {
    & Rscript -e "find.package('shiny')"
    & Rscript -e "find.package('rhino')"
} catch {
    Write-Host "Error: Failed to verify packages. $_"
}

# Launch app
Write-Host "Launching KiwiFlow app..."
try {
    Start-Process powershell -ArgumentList "-NoExit", "-Command", "& {conda activate kiwiflow; Rscript -e 'shiny::runApp(''C:/Users/$env:USERNAME/Desktop/KiwiFlow/app.R'', port=3838)'; Start-Sleep 2; Start-Process 'http://localhost:3838'}" -ErrorAction Stop
    Write-Host "KiwiFlow should open in your browser at http://localhost:3838. Close the new PowerShell window to stop."
} catch {
    Write-Host "Error: Failed to launch app. $_"
}

Write-Host "Setup complete. Check kiwiflow_setup.log for details."
pause
Stop-Transcript