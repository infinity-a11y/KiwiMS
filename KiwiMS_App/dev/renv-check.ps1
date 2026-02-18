# renv-check.ps1

Write-Host "Starting renv binary update check..." -ForegroundColor Cyan

# Execute the R script and capture its exit status
& Rscript ./check_renv_updates.R
$exitCode = $LASTEXITCODE

Write-Host "`nCheck completed." -ForegroundColor Green

# Signal if updates are available based on exit code
if ($exitCode -eq 10) {
    Write-Host "Updates detected! Setting output for GitHub Actions." -ForegroundColor Yellow
    if ($env:GITHUB_OUTPUT) {
        "updates_found=true" | Out-File -FilePath $env:GITHUB_OUTPUT -Append -Encoding utf8
    }
}
else {
    Write-Host "No updates or error occurred. Exit Code: $exitCode" -ForegroundColor Gray
    if ($env:GITHUB_OUTPUT) {
        "updates_found=false" | Out-File -FilePath $env:GITHUB_OUTPUT -Append -Encoding utf8
    }
}