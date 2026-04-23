param(
    [string]$basePath,
    [string]$userDataPath,
    [string]$envName,
    [string]$logFile,
    [string]$installScope
)

$ErrorActionPreference = "Continue"
Start-Transcript -Path $logFile -Append -Force | Out-Null

Write-Output "=========================================="
Write-Output "   Deconvolution Functional Test          "
Write-Output "=========================================="

. "$basePath\functions.ps1"

$condaCmd = Find-CondaExecutable
$RPortablePath = Join-Path $basePath "R-Portable\bin\Rscript.exe"

$guid = [guid]::NewGuid().ToString().Substring(0, 8)
$testTempDir = Join-Path ([System.IO.Path]::GetTempPath()) "KiwiMS_Test_$guid"

Write-Output "Creating temporary test directory: $testTempDir"
New-Item -ItemType Directory -Path $testTempDir -Force | Out-Null

try {
    Write-Output "Writing configuration file ..."
    & $condaCmd run -n $envName "$RPortablePath" "$basePath\make_config.R" "$basePath"

    if (Test-Path "$basePath\resources\config.rds") {
        Copy-Item -Path "$basePath\resources\config.rds" -Destination (Join-Path $testTempDir "config.rds")
    }
    else {
        Write-Output "Warning: config.rds not found in current directory."
    }

    Write-Output "Executing R deconvolution logic..."
    
    $dbPath = Join-Path $testTempDir "results.db"

    # Use double quotes for the PS string so $basePath expands, 
    # but use R's normalized paths to avoid backslash escape issues.
    $normPath = $basePath.Replace("\", "/")
    $rExpression = "source('$normPath/renv/activate.R'); source('$normPath/app/logic/deconvolution_execute.R')"

    & $condaCmd run -n $envName --no-capture-output "$RPortablePath" --vanilla -e "$rExpression" --args "$testTempDir" "$testTempDir" "$basePath" "$testTempDir" "testing" "$dbPath" "FALSE"
    
    Remove-Item -Path "$basePath\resources\config.rds", (Join-Path $testTempDir "config.rds") -Force -ErrorAction SilentlyContinue

    if ($LASTEXITCODE -ne 0) {
        Write-Output "R script failed with exit code $LASTEXITCODE"
    }
}
catch {
    Write-Output "An error occurred during the test: $($_.Exception.Message)"
}
finally {
    if (Test-Path $testTempDir) {
        Write-Output "Cleaning up test directory..."
        Start-Sleep -Seconds 1 
        Remove-Item -Path $testTempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
    
    Write-Output "Deconvolution test complete."
    Stop-Transcript
}