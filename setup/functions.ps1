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
    $defaultUserProfilePath = "$env:LOCALAPPDATA\miniconda3\Scripts\conda.exe"
    if (Test-Path $defaultUserProfilePath) {
        Write-Host "Found conda.exe at default UserProfile path: $defaultUserProfilePath"
        return $defaultUserProfilePath
    }

    # 3. Check if conda.exe is in the system's PATH using Get-Command
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
# FUNCTION Get-CondaScope
#-----------------------------#
function Get-CondaScope {
    param([string]$CondaPath)

    if (-not $CondaPath) { return $null }

    # Define common system-level roots
    $systemRoots = @(
        $env:ProgramData,
        $env:ProgramFiles,
        "${env:ProgramFiles(x86)}"
    )

    foreach ($root in $systemRoots) {
        if ($CondaPath.StartsWith($root, "OrdinalIgnoreCase")) {
            return "allusers"
        }
    }

    # If it's in the Users folder or LocalAppData, it's definitely currentuser
    if ($CondaPath -like "*\Users\*" -or $CondaPath.StartsWith($env:LOCALAPPDATA, "OrdinalIgnoreCase")) {
        return "currentuser"
    }

    # Fallback: if we can't be sure, it's safer to treat as currentuser 
    # or return 'unknown'
    return "currentuser"
}

#-----------------------------#
# FUNCTION Find Rtools
#-----------------------------#
#-----------------------------------------#
# FUNCTION: Find-Rtools45 (Version Aware)
#-----------------------------------------#
function Find-Rtools45Executable {
    # 1. Check Registry (The most reliable way to find the version)
    $regPaths = @(
        "HKLM:\SOFTWARE\R-core\Rtools\4.5",
        "HKCU:\SOFTWARE\R-core\Rtools\4.5"
    )
    
    foreach ($reg in $regPaths) {
        if (Test-Path $reg) {
            $installPath = Get-ItemProperty -Path $reg -Name "InstallPath" -ErrorAction SilentlyContinue
            if ($installPath) {
                $exe = Join-Path $installPath.InstallPath "usr\bin\make.exe"
                if (Test-Path $exe) { return $exe }
            }
        }
    }

    # 2. Hard-coded path checks (fallback)
    $paths = @(
        "C:\rtools45\usr\bin\make.exe",
        (Join-Path $env:LOCALAPPDATA "rtools45\usr\bin\make.exe")
    )

    foreach ($p in $paths) {
        if (Test-Path $p) { return $p }
    }

    # 3. Check PATH but ensure the directory name contains '45'
    $cmds = Get-Command make.exe -All -ErrorAction SilentlyContinue
    foreach ($cmd in $cmds) {
        if ($cmd.Path -like "*rtools45*") {
            return $cmd.Path
        }
    }

    return $null
}

#-----------------------------------------#
# FUNCTION: Get-PathScope
#-----------------------------------------#
function Get-PathScope {
    param([string]$FilePath)
    if (-not $FilePath) { return $null }

    $systemRoots = @($env:ProgramData, $env:ProgramFiles, ${env:ProgramFiles(x86)}, "C:\rtools45")
    
    foreach ($root in $systemRoots) {
        if ($FilePath.StartsWith($root, "OrdinalIgnoreCase")) { return "allusers" }
    }

    if ($FilePath -like "*\Users\*" -or $FilePath.StartsWith($env:LOCALAPPDATA, "OrdinalIgnoreCase")) {
        return "currentuser"
    }

    return "currentuser"
}

#-----------------------------#
# FUNCTION Download with Retry
#-----------------------------#
function Download-File($url, $destination) {
    # Force TLS 1.2
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

    # Use a temporary name for the download itself to avoid locking the main destination
    $tempDownloadPath = $destination + ".tmp"

    if (Test-Path $tempDownloadPath) {
        Remove-Item $tempDownloadPath -Force -ErrorAction SilentlyContinue
    }

    $success = $false
    for ($i = 0; $i -lt 3; $i++) {
        try {
            Write-Host "Downloading via BITS to: $tempDownloadPath (Attempt $($i+1))"
            
            # Start-BitsTransfer is synchronous by default
            # It handles the "Complete" call automatically when it finishes
            Start-BitsTransfer -Source $url -Destination $tempDownloadPath -Priority High -ErrorAction Stop
            
            # If successful, move the temp file to the final destination
            if (Test-Path $destination) { Remove-Item $destination -Force }
            Move-Item -Path $tempDownloadPath -Destination $destination -Force
            
            $success = $true
            break
        }
        catch {
            Write-Warning "Attempt $($i+1) failed: $($_.Exception.Message)"
            # Clean the temp file on failure to ensure a fresh start for the next retry
            if (Test-Path $tempDownloadPath) { Remove-Item $tempDownloadPath -Force -ErrorAction SilentlyContinue }
            Start-Sleep -Seconds 3
        }
    }

    if (-Not $success) {
        Write-Error "Failed to download $url after 3 attempts."
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
        }
        else {
            Write-Host "Quarto installation completed but verification failed"
            exit 1
        }
    }
    catch {
        Write-Host "Error installing Quarto: $_"
        exit 1
    }
}

#-----------------------------------------#
# FUNCTION: Find Quarto (Version Aware)
#-----------------------------------------#
function Find-QuartoInstallation {
    try {
        # Check if quarto is already in the current session PATH
        $quartoPath = Get-Command quarto -ErrorAction SilentlyContinue
        if ($quartoPath) {
            $binDir = Split-Path $quartoPath.Source -Parent
            # Quarto usually lives in 'bin', we want the parent for the 'Path' property
            $installRoot = Split-Path $binDir -Parent 
            $versionString = & quarto --version
            
            return @{
                Found   = $true
                Path    = $installRoot
                BinDir  = $binDir
                Version = $versionString.Trim()
            }
        }
    } catch {}

    return @{ Found = $false; Path = $null; Version = $null }
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
    }
    else {
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
    }
    catch {
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
    }
    catch {
        Write-Host "Error comparing versions: $_"
        return $null
    }
}
