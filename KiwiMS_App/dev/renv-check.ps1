# renv-check.ps1

Write-Host "Starting renv binary update check..." -ForegroundColor Cyan

# Execute the R script
& Rscript ./KiwiMS_App/dev/check_renv_updates.R
$exitCode = $LASTEXITCODE

Write-Host "`nCheck completed." -ForegroundColor Green

if ($exitCode -eq 10) {
    Write-Host "Updates detected!" -ForegroundColor Yellow
    if ($env:GITHUB_OUTPUT) {
        "updates_found=true" | Out-File -FilePath $env:GITHUB_OUTPUT -Append -Encoding utf8
    }
    exit 0
}
elseif ($exitCode -eq 0) {
    if ($env:GITHUB_OUTPUT) {
        "updates_found=false" | Out-File -FilePath $env:GITHUB_OUTPUT -Append -Encoding utf8
    }
    exit 0
}
else {
    Write-Host "Real Error detected! Exit Code: $exitCode" -ForegroundColor Red
    exit $exitCode
}
