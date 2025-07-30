
#-----------------------------#
# FUNCTION Find Conda
#-----------------------------#
function Find-CondaExecutable {
    # 1. Check common default system-wide installation path (ProgramData)
    $defaultProgramDataPath = "$env:ProgramData\miniconda3\Scripts\conda.exe"
    if (Test-Path $defaultProgramDataPath) {
        Write-Host "Found conda.exe at default ProgramData path: $defaultProgramDataPath"
        return $defaultProgramDataPath
    }

    # 2. Check common default user-specific installation path (UserProfile)
    $defaultUserProfilePath = "$env:UserProfile\miniconda3\Scripts\conda.exe"
    if (Test-Path $defaultUserProfilePath) {
        Write-Host "Found conda.exe at default UserProfile path: $defaultUserProfilePath"
        return $defaultUserProfilePath
    }

    # 3. Check if conda.exe is in the system's PATH using Get-Command
    # This is often the most reliable if user installed Conda and added it to PATH.
    try {
        $condaCmdInPath = (Get-Command conda.exe -ErrorAction SilentlyContinue).Path
        if ($condaCmdInPath) {
            Write-Host "Found conda.exe in system PATH: $condaCmdInPath"
            return $condaCmdInPath
        }
    }
    catch {
        Write-Warning "Failed to find conda.exe in system PATH using Get-Command. Error: $($_.Exception.Message)"
    }

    # 4. Fallback for common Anaconda installation paths (if a user has Anaconda instead of Miniconda)
    $defaultProgramFilesAnacondaPath = "$env:ProgramFiles\Anaconda3\Scripts\conda.exe"
    if (Test-Path $defaultProgramFilesAnacondaPath) {
        Write-Host "Found conda.exe at default ProgramFiles Anaconda path: $defaultProgramFilesAnacondaPath"
        return $defaultProgramFilesAnacondaPath
    }

    $defaultUserProfileAnacondaPath = "$env:UserProfile\Anaconda3\Scripts\conda.exe"
    if (Test-Path $defaultUserProfileAnacondaPath) {
        Write-Host "Found conda.exe at default UserProfile Anaconda path: $defaultUserProfileAnacondaPath"
        return $defaultUserProfileAnacondaPath
    }

    Write-Host "ERROR: conda.exe not found in common locations or system PATH." -ForegroundColor Red
    return $null # Return null if conda.exe is not found anywhere
}

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
# FUNCTION to install Quarto
#-----------------------------#
function Install-Quarto {
    param (
        [string]$InstallDir = $DEFAULT_QUARTO_INSTALL_DIR
    )
    try {
        $quartoBinDir = Join-Path $InstallDir "bin"
        
        Write-Host "Downloading Quarto v$QUARTO_VERSION..."
        Invoke-WebRequest -Uri $QUARTO_DOWNLOAD_URL -OutFile $QUARTO_TEMP_ZIP -ErrorAction Stop
        
        # Create installation directory if it doesn't exist
        if (-not (Test-Path $InstallDir)) {
            New-Item -ItemType Directory -Path $InstallDir -Force -ErrorAction Stop
        }
        
        Write-Host "Extracting Quarto to $InstallDir..."
        Expand-Archive -Path $QUARTO_TEMP_ZIP -DestinationPath $InstallDir -Force -ErrorAction Stop
        
        # Clean up
        Remove-Item -Path $QUARTO_TEMP_ZIP -Force -ErrorAction Stop
        
        Write-Host "Quarto v$QUARTO_VERSION installed successfully to $InstallDir"
        
        # Add bin directory to PATH if not already present
        if (-not (Test-PathInEnvironment -Directory $quartoBinDir)) {
            Add-ToSystemPath -Directory $quartoBinDir
        }
        
        # Verify installation
        $quartoInfo = Find-QuartoInstallation
        if ($quartoInfo.Found) {
            Write-Host "Quarto installation verified successfully at $($quartoInfo.Path)"
        } else {
            Write-Host "Quarto installation completed but verification failed"
            exit 1
        }
    } catch {
        Write-Host "Error installing Quarto: $_"
        exit 1
    }
}

#-----------------------------#
# FUNCTION to find Quarto installation
#-----------------------------#
function Find-QuartoInstallation {
    try {
        $quartoPath = Get-Command quarto -ErrorAction SilentlyContinue
        if ($quartoPath) {
            $quartoBinDir = Split-Path $quartoPath.Source -Parent
            $quartoVersion = & quarto --version
            Write-Host "Quarto CLI version $quartoVersion found at $quartoBinDir"
            return @{
                Found = $true
                Path = $quartoBinDir
                Version = $quartoVersion
            }
        } else {
            Write-Host "Quarto CLI is not installed or not found in PATH"
            return @{
                Found = $false
                Path = $null
                Version = $null
            }
        }
    } catch {
        Write-Host "Error checking Quarto installation: $_"
        return @{
            Found = $false
            Path = $null
            Version = $null
        }
    }
}

#-----------------------------#
# FUNCTION to check if a directory is in PATH
#-----------------------------#
function Test-PathInEnvironment {
    param (
        [string]$Directory
    )
    $currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    if ($currentPath -like "*$Directory*") {
        Write-Host "Directory $Directory is already in system PATH"
        return $true
    } else {
        Write-Host "Directory $Directory is not in system PATH"
        return $false
    }
}

#-----------------------------#
# FUNCTION to add a directory to system PATH
#-----------------------------#
function Add-ToSystemPath {
    param (
        [string]$Directory
    )
    try {
        $currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
        if (-not ($currentPath -like "*$Directory*")) {
            $newPath = "$currentPath;$Directory"
            [Environment]::SetEnvironmentVariable("Path", $newPath, "Machine")
            Write-Host "Added $Directory to system PATH"
            # Update current session PATH
            $env:Path = [Environment]::GetEnvironmentVariable("Path", "Machine")
        }
    } catch {
        Write-Host "Error adding $Directory to PATH: $_"
        exit 1
    }
}

#-----------------------------#
# FUNCTION to compare version numbers
#-----------------------------#
function Compare-Version {
    param (
        [string]$InstalledVersion,
        [string]$TargetVersion
    )
    try {
        $installed = [version]($InstalledVersion -replace '^v', '')
        $target = [version]($TargetVersion -replace '^v', '')
        if ($installed -gt $target) { return 1 }  # Installed is newer
        elseif ($installed -eq $target) { return 0 }  # Same version
        else { return -1 }  # Installed is older
    } catch {
        Write-Host "Error comparing versions: $_"
        return $null
    }
}