#-----------------------------#
# First-start warm-up
#-----------------------------#
# Run by the installer as the ORIGINAL (non-elevated) user, after compileall.
#
# The first KiwiMS start after an install, and the first deconvolution, are an
# order of magnitude slower than every later one (measured on a fresh Windows 11
# VM: 80 s vs 10 s to open, 170 s vs 16 s before the first sample). Almost none
# of that is KiwiMS's own work: Windows Defender scans every DLL, .pyd and
# package file the first time it is opened, the disk cache is cold, and the
# Python import chain writes two caches into the user profile (matplotlib's
# font list, multiplierz's data directory). Defender remembers files it has
# already cleared, so doing the same loads once here moves that wait into the
# installer's progress bar - where waiting is expected - instead of a terminal
# that looks hung or a spinner stuck on "Initializing".
#
# It loads exactly what a real start does, without serving the app or touching
# any data: rhino::app() sources every module and attaches every package the UI
# uses, then the deconvolution worker's packages; on the Python side the
# worker's own import set plus the two cache-writing modules, and UniDec.exe,
# which prints its usage text and exits when given no configuration file.
#
# Run as the original user on purpose: the Python caches are per-user and must
# land in the profile of whoever will run KiwiMS, not the elevated account.
#
# Never fatal. A warm-up that fails or times out only means the first start is
# slow again, so every failure is logged and the script always exits 0.
param(
    [string]$basePath,
    [string]$logFile,
    # Generous: on the VM above the R side alone took ~70 s cold. Steps still
    # running after this are stopped, so the installer can never hang on them.
    # A parameter only so the timeout path can be tested.
    [int]$timeoutSec = 300
)

$ErrorActionPreference = "Continue"
$ProgressPreference = "SilentlyContinue"

function Write-Log([string] $Message) {
    try { "[$(Get-Date -Format 'HH:mm:ss')] $Message" | Add-Content -Path $logFile -Encoding utf8 } catch {}
}

# Rscript.exe runs R in a child Rterm.exe, so stopping only the process we
# started would leave R running. taskkill /T takes the whole tree.
function Stop-Tree($proc) {
    try { & taskkill.exe /PID $proc.Id /T /F 2>&1 | Out-Null } catch {}
}

$rscript = Join-Path $basePath "R-Portable\bin\Rscript.exe"
$envRoot = Join-Path $basePath "env_kiwims"
$python = Join-Path $envRoot "python.exe"
$unidecExe = Join-Path $envRoot "Lib\site-packages\unidec\bin\unidec.exe"

Write-Log "INFO: First-start warm-up started."

# Same isolation the launcher sets up (see launch.ps1), so this loads the same
# files from the same places a real start does.
$env:R_HOME = Join-Path $basePath "R-Portable"
$env:PYTHONHOME = $null
$env:RETICULATE_PYTHON = $python
$env:RENV_CONFIG_AUTO_SNAPSHOT = "FALSE"
$env:RENV_CONFIG_SYNCHRONIZED_CHECK = "FALSE"
$env:RENV_CONFIG_SANDBOX_ENABLED = "FALSE"
$env:PYTHONNOUSERSITE = "1"

# python.exe needs the conda DLL directories on PATH to import compiled
# extensions (0xC06D007E otherwise). Set before either process starts; R does
# not mind, because it resolves its own DLLs from R_HOME.
$env:PATH = "$envRoot;$envRoot\Library\bin;$env:PATH"

$jobs = @()

# R and Python are started together: they read disjoint parts of the install
# tree, so the scans run side by side instead of one after the other.
if (Test-Path $rscript) {
    $rCmd = "invisible(rhino::app()); " +
    "suppressMessages({library(reticulate); library(DBI); library(RSQLite); library(data.table)})"
    try {
        $jobs += [pscustomobject]@{
            Name = "R"
            Proc = Start-Process -FilePath $rscript `
                -ArgumentList "--no-save --no-restore -e `"$rCmd`"" `
                -WorkingDirectory $basePath -NoNewWindow -PassThru `
                -RedirectStandardOutput (Join-Path $env:TEMP "kiwims_warmup_r_out.txt") `
                -RedirectStandardError  (Join-Path $env:TEMP "kiwims_warmup_r_err.txt")
        }
    }
    catch { Write-Log "WARN: R warm-up not started: $($_.Exception.Message)" }
}
else {
    Write-Log "WARN: R warm-up skipped, $rscript not found."
}

if (Test-Path $python) {
    # `from unidec import tools` pulls in the Waters and Thermo readers, which
    # the worker's first sample would otherwise be the first to load.
    $pyCmd = "import unidec, multiplierz, matplotlib.pyplot; from unidec import tools"
    try {
        $jobs += [pscustomobject]@{
            Name = "Python"
            Proc = Start-Process -FilePath $python `
                -ArgumentList "-W ignore -c `"$pyCmd`"" `
                -WorkingDirectory $envRoot -NoNewWindow -PassThru `
                -RedirectStandardOutput (Join-Path $env:TEMP "kiwims_warmup_py_out.txt") `
                -RedirectStandardError  (Join-Path $env:TEMP "kiwims_warmup_py_err.txt")
        }
    }
    catch { Write-Log "WARN: Python warm-up not started: $($_.Exception.Message)" }
}
else {
    Write-Log "WARN: Python warm-up skipped, $python not found."
}

$timer = [System.Diagnostics.Stopwatch]::StartNew()

# Windows PowerShell only reports ExitCode for a -PassThru process whose handle
# was read while it was still running.
foreach ($job in $jobs) { $null = $job.Proc.Handle }

foreach ($job in $jobs) {
    $remainingMs = [math]::Max(0, $timeoutSec * 1000 - $timer.ElapsedMilliseconds)
    if ($job.Proc.WaitForExit([int] $remainingMs)) {
        Write-Log ("INFO: {0} warm-up finished after {1:N1} s (exit {2})." -f
            $job.Name, $timer.Elapsed.TotalSeconds, $job.Proc.ExitCode)
        if ($job.Proc.ExitCode -ne 0) {
            $errFile = Join-Path $env:TEMP ("kiwims_warmup_{0}_err.txt" -f
                $(if ($job.Name -eq "R") { "r" } else { "py" }))
            Get-Content $errFile -Tail 15 -ErrorAction SilentlyContinue |
                ForEach-Object { Write-Log "  $_" }
        }
    }
    else {
        Stop-Tree $job.Proc
        Write-Log "WARN: $($job.Name) warm-up still running after $timeoutSec s and was stopped."
    }
}

# Exits straight away (code 88) without a configuration file; only the load
# matters here, so its usage text is discarded.
if (Test-Path $unidecExe) {
    try {
        $p = Start-Process -FilePath $unidecExe -WorkingDirectory $env:TEMP -NoNewWindow -PassThru `
            -RedirectStandardOutput (Join-Path $env:TEMP "kiwims_warmup_unidec_out.txt")
        if (-not $p.WaitForExit(60000)) { Stop-Tree $p }
    }
    catch { Write-Log "WARN: UniDec.exe warm-up not started: $($_.Exception.Message)" }
}

Remove-Item (Join-Path $env:TEMP "kiwims_warmup_*.txt") -Force -ErrorAction SilentlyContinue

Write-Log ("INFO: First-start warm-up done after {0:N1} s." -f $timer.Elapsed.TotalSeconds)
exit 0
