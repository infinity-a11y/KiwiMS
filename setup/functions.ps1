#-----------------------------#
# FUNCTION Find Conda
#-----------------------------#
function Find-CondaExecutable {
    $searchPaths = @(
        # Miniconda - System Wide
        "$env:ProgramData\miniconda3\Scripts\conda.exe",
        "$env:ProgramData\miniconda3\Library\bin\conda.exe",
        "$env:ProgramData\miniconda3\condabin\conda.bat",

        # Miniconda - User Specific
        "$env:LOCALAPPDATA\miniconda3\Scripts\conda.exe",
        "$env:LOCALAPPDATA\miniconda3\Library\bin\conda.exe",
        "$env:LOCALAPPDATA\miniconda3\condabin\conda.bat"
    )

    # Search through hardcoded common paths
    foreach ($path in $searchPaths) {
        if (Test-Path $path) {
            return $path
        }
    }

    # Check if conda.exe is in the system PATH
    $condaInPath = Get-Command conda.exe, conda.bat -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($condaInPath) {
        return $condaInPath.Path
    }

    # Check Environment Variables
    if ($env:CONDA_EXE -and (Test-Path $env:CONDA_EXE)) {
        return $env:CONDA_EXE
    }

    return $null
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

    if ($CondaPath -like "*\Users\*" -or $CondaPath.StartsWith($env:LOCALAPPDATA, "OrdinalIgnoreCase")) {
        return "currentuser"
    }

    return "currentuser"
}

#-----------------------------#
# FUNCTION Find Rtools
#-----------------------------#
function Find-Rtools45Executable {
    # Check Registry
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

    # Hard-coded path checks (fallback)
    $paths = @(
        "C:\rtools45\usr\bin\make.exe",
        (Join-Path $env:LOCALAPPDATA "rtools45\usr\bin\make.exe")
    )

    foreach ($p in $paths) {
        if (Test-Path $p) { return $p }
    }

    # Check PATH but ensure the directory name contains '45'
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
# FUNCTION Download with Retry and Fallback
#-----------------------------#
function Download-File($url, $destination) {
    # Force TLS 1.2
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

    $tempDownloadPath = $destination + ".tmp"

    if (Test-Path $tempDownloadPath) {
        Remove-Item $tempDownloadPath -Force -ErrorAction SilentlyContinue
    }

    $success = $false
    for ($i = 0; $i -lt 3; $i++) {
        try {
            Write-Output "Attempt $($i+1): Downloading via BITS..."
            # Try BITS first
            Start-BitsTransfer -Source $url -Destination $tempDownloadPath -Priority High -ErrorAction Stop
            
            $success = $true
        }
        catch {
            Write-Warning "BITS failed: $($_.Exception.Message)"
            
            # If BITS fails (e.g., Battery Mode), try Invoke-WebRequest as a fallback within the same attempt
            try {
                Write-Output "BITS failed or suspended. Falling back to Invoke-WebRequest..."
                Invoke-WebRequest -Uri $url -OutFile $tempDownloadPath -UseBasicParsing -ErrorAction Stop
                $success = $true
            }
            catch {
                Write-Warning "Fallback (Invoke-WebRequest) also failed: $($_.Exception.Message)"
                if (Test-Path $tempDownloadPath) { Remove-Item $tempDownloadPath -Force -ErrorAction SilentlyContinue }
            }
        }

        if ($success) {
            # Move the temp file to the final destination
            if (Test-Path $destination) { Remove-Item $destination -Force }
            Move-Item -Path $tempDownloadPath -Destination $destination -Force
            Write-Output "Download successful."
            break
        }
        else {
            Write-Output "Retrying in 3 seconds..."
            Start-Sleep -Seconds 3
        }
    }

    if (-Not $success) {
        Write-Error "Failed to download $url after 3 attempts (both BITS and Invoke-WebRequest failed)."
        Stop-Transcript
        exit 1
    }
}

#-----------------------------------------#
# FUNCTION: Find Quarto
#-----------------------------------------#
function Find-QuartoInstallation {
    try {
        # Check if quarto is already in the current session PATH
        $quartoPath = Get-Command quarto -ErrorAction SilentlyContinue
        if ($quartoPath) {
            $binDir = Split-Path $quartoPath.Source -Parent
            $installRoot = Split-Path $binDir -Parent 
            $versionString = & quarto --version
            
            return @{
                Found   = $true
                Path    = $installRoot
                BinDir  = $binDir
                Version = $versionString.Trim()
            }
        }
    }
    catch {
        Stop-Transcript
        exit 1
    }

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
        return $true
    }
    else {
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
            Write-Output "Added $Directory to system PATH"
            # Update current session PATH
            $env:Path = [Environment]::GetEnvironmentVariable("Path", "Machine")
        }
    }
    catch {
        Write-Output "Error adding $Directory to PATH: $_"
        Stop-Transcript
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
        return $null
    }
}
