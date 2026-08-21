<#
.SYNOPSIS
    Bumps the KiwiMS version from a single source of truth.

.DESCRIPTION
    KiwiMS_App\resources\version.txt is the ONE place the version lives.

    Everything that can read a file at run time or build time reads it directly
    and is never touched by this script:

        - the Shiny app         app\logic\helper_functions.R  get_kiwims_version()
        - the launcher banner   dev\launch.ps1
        - KiwiMS.exe            build-launcher.ps1 (ps2exe -version)
        - the installer         setup_script.iss (ISPP FileRead at compile time)

    Only static documents have to carry a literal copy, so only those are
    rewritten here:

        - README.md   version badge, release links, citation blocks
        - CITATION    version and date-released

    version.txt also holds the Zenodo concept DOI. That DOI is minted once and
    always resolves to the latest release, so it is a constant rather than
    something to bump - it lives there so -Check can catch a version-specific
    DOI being pasted in by mistake.

.EXAMPLE
    .\KiwiMS_App\dev\set-version.ps1 -Version 0.7.3
    Bumps to 0.7.3, dated today.

.EXAMPLE
    .\KiwiMS_App\dev\set-version.ps1 -Version 0.7.3 -ReleaseDate 2026-09-01

.EXAMPLE
    .\KiwiMS_App\dev\set-version.ps1 -Check
    Changes nothing. Reports any document that disagrees with version.txt.
    Exits 1 on drift, 0 when everything is in sync.
#>

[CmdletBinding(DefaultParameterSetName = 'Set')]
param(
    # New version, semver style: 1.2.3 or 1.2.3-rc1
    [Parameter(Mandatory, ParameterSetName = 'Set', Position = 0)]
    [ValidatePattern('^\d+\.\d+\.\d+([-+][0-9A-Za-z.\-]+)?$')]
    [string] $Version,

    # Release date, yyyy-MM-dd. Defaults to today.
    [Parameter(ParameterSetName = 'Set')]
    [ValidatePattern('^\d{4}-\d{2}-\d{2}$')]
    [string] $ReleaseDate = (Get-Date -Format 'yyyy-MM-dd'),

    # Report drift without writing anything.
    [Parameter(Mandatory, ParameterSetName = 'Check')]
    [switch] $Check
)

$ErrorActionPreference = 'Stop'

$appRoot = Split-Path -Parent $PSScriptRoot     # ...\KiwiMS_App
$repoRoot = Split-Path -Parent $appRoot         # ...\KiwiMS
$versionFile = Join-Path $appRoot 'resources\version.txt'

# Matches 1.2.3 as well as 1.2.3-rc1 / 1.2.3+build
$semver = '\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.\-]+)?'
$monthNames = @('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

# --- Helpers ---------------------------------------------------------------------

function Read-TextFile([string] $Path) {
    if (-not (Test-Path -LiteralPath $Path)) { throw "File not found: $Path" }
    # ReadAllText honours a BOM if present and leaves line endings untouched.
    [System.IO.File]::ReadAllText($Path)
}

function Write-TextFile([string] $Path, [string] $Text) {
    # BOM-less UTF-8. Set-Content -Encoding utf8 would add a BOM on PowerShell 5.1,
    # which both YAML parsers and diffs dislike.
    [System.IO.File]::WriteAllText($Path, $Text, (New-Object System.Text.UTF8Encoding $false))
}

function Get-VersionInfo {
    $info = @{}
    foreach ($line in (Get-Content -LiteralPath $versionFile)) {
        if ($line -match '^\s*([^=#\s]+)\s*=\s*(.*)$') { $info[$Matches[1]] = $Matches[2].Trim() }
    }
    if (-not $info.ContainsKey('version')) { throw "No 'version=' entry in $versionFile" }
    $info
}

# Every literal copy of the version that lives outside version.txt.
# Each rule captures the value in group 2, so the same rule serves both the
# comparison and the replacement without needing to know the previous value.
function Get-Rules([string] $ExpectedVersion, [string] $ExpectedDate, [string] $ExpectedDoi) {
    $d = [datetime]::ParseExact($ExpectedDate, 'yyyy-MM-dd', $null)

    # The Zenodo concept DOI. Unlike a version DOI it is minted once and always
    # resolves to the latest release, so it is a constant that must never change -
    # these rules exist to stop a version-specific DOI from creeping back in.
    $doiId = if ($ExpectedDoi -match 'zenodo\.(\d+)') { $Matches[1] } else { $null }
    if (-not $doiId) { throw "Could not read a Zenodo record id from doi '$ExpectedDoi'" }

    @(
        # Matches every zenodo.<id> in the badge, the links, the prose and the
        # BibTeX block. Anchored on "zenodo." so the UniDec DOI is never touched.
        @{ File = 'README.md'; Label = 'zenodo DOI'; Regex = "(zenodo\.)(\d+)()"; Value = $doiId }
        @{ File = 'CITATION'; Label = 'zenodo DOI'; Regex = "(zenodo\.)(\d+)()"; Value = $doiId }
        # The BibTeX key Zenodo generates embeds the record id and the year.
        @{ File = 'README.md'; Label = 'bibtex key year'; Regex = "(@software\{[a-z_]+?_)(\d{4})(_)"; Value = $d.Year.ToString() }
        @{ File = 'README.md'; Label = 'bibtex key DOI id'; Regex = "(@software\{[a-z_]+?_\d{4}_)(\d+)(,)"; Value = $doiId }
        @{ File = 'README.md'; Label = 'version badge'; Regex = "(badge/Version-)($semver)(-E8CB98)"; Value = $ExpectedVersion }
        @{ File = 'README.md'; Label = 'release tag link'; Regex = "(releases/tag/)($semver)()"; Value = $ExpectedVersion }
        @{ File = 'README.md'; Label = 'current version heading'; Regex = "(<b>KiwiMS )($semver)(</b>)"; Value = $ExpectedVersion }
        @{ File = 'README.md'; Label = 'citation title'; Regex = "(KiwiMS: KiwiMS )($semver)()"; Value = $ExpectedVersion }
        @{ File = 'README.md'; Label = 'bibtex version'; Regex = "(version\s*=\s*\{)($semver)(\})"; Value = $ExpectedVersion }
        # Anchored to the @software block so the UniDec @article citation below it,
        # which has its own month/year, is left alone.
        @{ File = 'README.md'; Label = 'bibtex month'; Regex = "(?s)(@software\{.*?month\s*=\s*)([a-z]{3})(,)"; Value = $monthNames[$d.Month - 1] }
        @{ File = 'README.md'; Label = 'bibtex year'; Regex = "(?s)(@software\{.*?year\s*=\s*)(\d{4})()"; Value = $d.Year.ToString() }
        @{ File = 'CITATION'; Label = 'version'; Regex = "(?m)(^version:\s*`")($semver)(`")"; Value = $ExpectedVersion }
        @{ File = 'CITATION'; Label = 'date-released'; Regex = "(?m)(^date-released:\s*)(\d{4}-\d{2}-\d{2})()"; Value = $ExpectedDate }
    )
}

# --- Check mode ------------------------------------------------------------------

if ($Check) {
    $info = Get-VersionInfo
    $expectedVersion = $info['version']
    $expectedDate = if ($info.ContainsKey('release_date')) { $info['release_date'] } else { (Get-Date -Format 'yyyy-MM-dd') }
    $expectedDoi = $info['doi']

    Write-Host "version.txt : $expectedVersion (released $expectedDate, doi $expectedDoi)" -ForegroundColor Cyan

    $problems = @()
    foreach ($rule in (Get-Rules $expectedVersion $expectedDate $expectedDoi)) {
        $text = Read-TextFile (Join-Path $repoRoot $rule.File)
        $found = [regex]::Matches($text, $rule.Regex)
        if ($found.Count -eq 0) {
            $problems += "$($rule.File): no match for '$($rule.Label)' - the rule in set-version.ps1 is stale"
            continue
        }
        foreach ($m in $found) {
            if ($m.Groups[2].Value -ne $rule.Value) {
                $problems += "$($rule.File): $($rule.Label) is '$($m.Groups[2].Value)', expected '$($rule.Value)'"
            }
        }
    }

    if ($problems.Count -gt 0) {
        Write-Host ""
        Write-Host "[DRIFT] Documents disagree with version.txt:" -ForegroundColor Yellow
        $problems | ForEach-Object { Write-Host "        $_" -ForegroundColor Yellow }
        Write-Host ""
        Write-Host "        Fix with: .\KiwiMS_App\dev\set-version.ps1 -Version $expectedVersion -ReleaseDate $expectedDate" -ForegroundColor Yellow
        exit 1
    }

    Write-Host "[OK] README.md and CITATION are in sync with version.txt." -ForegroundColor Green
    exit 0
}

# --- Set mode --------------------------------------------------------------------

$info = Get-VersionInfo
Write-Host ""
Write-Host "  $($info['version']) -> $Version   (released $ReleaseDate)" -ForegroundColor Cyan
Write-Host ""

# 1. The source of truth. Rewrite the known keys in place and leave any others alone.
$lines = Get-Content -LiteralPath $versionFile
$updated = foreach ($line in $lines) {
    if ($line -match '^\s*version\s*=') { "version=$Version" }
    elseif ($line -match '^\s*release_date\s*=') { "release_date=$ReleaseDate" }
    else { $line }
}
Write-TextFile $versionFile (($updated -join "`r`n") + "`r`n")
Write-Host "  resources\version.txt" -ForegroundColor Green

# 2. The static documents that cannot read it.
foreach ($group in (Get-Rules $Version $ReleaseDate $info['doi'] | Group-Object { $_.File })) {
    $path = Join-Path $repoRoot $group.Name
    $text = Read-TextFile $path
    $changed = 0

    foreach ($rule in $group.Group) {
        $hits = [regex]::Matches($text, $rule.Regex).Count
        if ($hits -eq 0) {
            Write-Host "  [WARN] $($group.Name): no match for '$($rule.Label)' - rule is stale" -ForegroundColor Yellow
            continue
        }
        $text = [regex]::Replace($text, $rule.Regex, ('${1}' + $rule.Value + '${3}'))
        $changed += $hits
    }

    Write-TextFile $path $text
    Write-Host "  $($group.Name) ($changed occurrences)" -ForegroundColor Green
}

Write-Host ""
Write-Host "Done. KiwiMS.exe and the installer pick the version up automatically on the" -ForegroundColor Gray
Write-Host "next build - nothing else to edit." -ForegroundColor Gray
Write-Host ""
