# renv-check.ps1

Write-Output "Starting renv binary update check..."

# Execute the R script
& Rscript ./KiwiMS_App/dev/check_renv_updates.R
$exitCode = $LASTEXITCODE

Write-Output "`nCheck completed."

if ($exitCode -eq 10) {
    Write-Output "Updates detected!"
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
    Write-Output "Real Error detected! Exit Code: $exitCode"
    exit $exitCode
}
