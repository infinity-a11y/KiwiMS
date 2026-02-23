# check-links.ps1

# Quarto version 
$TARGET_VERSION = "1.7.32" 

$URLs = @{
    "RTools 4.5" = "https://cran.r-project.org/bin/windows/Rtools/rtools45/files/rtools45-6768-6492.exe"
    "Quarto CLI" = "https://github.com/quarto-dev/quarto-cli/releases/download/v${TARGET_VERSION}/quarto-${TARGET_VERSION}-win.zip"
    "Miniconda"  = "https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-x86_64.exe"
}

$brokenLinks = @()
Write-Output "Checking third-party installer links..."

foreach ($name in $URLs.Keys) {
    $url = $URLs[$name]
    try {
        $response = Invoke-WebRequest -Uri $url -Method Head -TimeoutSec 15 -ErrorAction Stop
        if ($response.StatusCode -eq 200) {
            Write-Output "[OK] $name"
        } else { throw "Status $($response.StatusCode)" }
    } catch {
        Write-Output "[FAIL] $name"
        $brokenLinks += "- $name"
    }
}

# FINAL LOGIC
if ($brokenLinks.Count -gt 0) {
    $failedList = $brokenLinks -join "`n"
    
    # Save the list to GitHub Outputs
    if ($env:GITHUB_OUTPUT) {
        "failed_installers<<EOF" | Out-File -FilePath $env:GITHUB_OUTPUT -Append
        $failedList | Out-File -FilePath $env:GITHUB_OUTPUT -Append
        "EOF" | Out-File -FilePath $env:GITHUB_OUTPUT -Append
    }
    
    Write-Error "Installer check failed."
    exit 1 
}

exit 0