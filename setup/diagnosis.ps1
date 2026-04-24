#-----------------------------#
# Script Initialization
#-----------------------------#
param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue"

# Start logging
Start-Transcript -Path $logFile -Append | Out-Null

Write-Output "=========================================="
Write-Output "    KiwiMS Post-Installation Diagnosis    "
Write-Output "=========================================="
Write-Output "Project Root: $basePath"

# Define Portable Paths
$RPortablePath = Join-Path $basePath "R-Portable\bin\Rscript.exe"
$RenvLibrary = Join-Path $basePath "renv\library"
$localPython = Join-Path $basePath "env_kiwims\python.exe"

#-----------------------------#
# 1. System Settings Retrieval
#-----------------------------#
Write-Output "`n[1/6] System Environment"
$isElevated = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
Write-Output "Run as Admin:  $isElevated"
Write-Output "OS Version:    $((Get-CimInstance Win32_OperatingSystem).Caption)"

#-----------------------------#
# 2. Portable Engine Integrity
#-----------------------------#
Write-Output "`n[2/6] Portable Engine Integrity"

if (Test-Path $localPython) {
    Write-Output "[OK] Python env found: $localPython"
} else {
    Write-Output "[FAIL] Python env MISSING at $localPython"
}

if (Test-Path $RPortablePath) {
    Write-Output "[OK] R-Portable found: $RPortablePath"
} else {
    Write-Output "[FAIL] R-Portable MISSING at $RPortablePath"
}

#-----------------------------#
# 3. Portable R & Library Verification
#-----------------------------#
Write-Output "`n[3/6] Portable R & Library Integrity"

# Check if R-Portable exists
if (Test-Path $RPortablePath) {
    $rVersion = & "$RPortablePath" --version
    Write-Output "[OK] R-Portable found: $rVersion"
}
else {
    Write-Output "[FAIL] R-Portable MISSING at $RPortablePath"
}

# Check if the Library folder exists and has content
if (Test-Path $RenvLibrary) {
    $pkgCount = (Get-ChildItem -Path $RenvLibrary -Recurse -Directory -Depth 2).Count
    Write-Output "[OK] Local Library found. Detected approx $pkgCount package folders."
}
else {
    Write-Output "[FAIL] Local renv library MISSING at $RenvLibrary"
}

#-----------------------------#
# 4. renv & Package Health Check
#-----------------------------#
Write-Output "`n[4/6] Package Load Verification"

if ((Test-Path $RPortablePath) -and (Test-Path (Join-Path $basePath "renv.lock"))) {
    Push-Location $basePath
    try {
        $rRootPath = $basePath -replace '\\', '/'
        
        # This script attempts to load critical packages to ensure DLLs are working
        $verifyScript = @"
        setwd('$rRootPath');
        # Activate renv
        source('renv/activate.R');
        
        # List of critical packages to test
        pkgs <- c('shiny', 'reticulate', 'ggplot2');
        results <- sapply(pkgs, function(p) require(p, character.only = TRUE, quietly = TRUE));
        
        cat('\n--- Package Load Test ---\n');
        print(results);
        
        if(all(results)) {
            cat('\n[SUCCESS] All critical packages loaded successfully.\n');
        } else {
            cat('\n[ERROR] Some packages failed to load. Check for missing system DLLs.\n');
        }
"@
        & "$RPortablePath" --no-save --no-restore -e $verifyScript 2>&1 | Out-String | Write-Output
    }
    catch {
        Write-Output "Failed to execute package health check: $($_.Exception.Message)"
    }
    finally {
        Pop-Location
    }
}

#-----------------------------#
# 5. Toolchain Check
#-----------------------------#
Write-Output "`n[5/6] Toolchain Verification"

# Note: For R-Portable, we usually don't need Rtools on the client machine 
# unless they are compiling from source, which they won't do offline.
$rtools = Find-Rtools45Executable
if ($rtools) { Write-Output "[OK] Rtools 4.5: Found." } else { Write-Output "[INFO] Rtools 4.5: Not found (not required for runtime)." }

$quarto = Find-QuartoInstallation
if ($quarto.Found) { Write-Output "[OK] Quarto: Version $($quarto.Version) found." } else { Write-Output "[FAIL] Quarto: Not detected." }

#-----------------------------#
# 6. Summary
#-----------------------------#
Write-Output "`n=========================================="
Write-Output "Diagnosis Finished."
Write-Output "=========================================="
Stop-Transcript