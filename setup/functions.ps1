
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
    } catch {
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