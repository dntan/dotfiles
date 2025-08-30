# Run PowerShell as Administrator

# Ensure elevated
$curr = [Security.Principal.WindowsIdentity]::GetCurrent()
$princ = New-Object Security.Principal.WindowsPrincipal($curr)
if (-not $princ.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Error "Please run this script in an elevated (Administrator) PowerShell."
    exit 1
}

# Optional: if this file was downloaded, uncomment to unblock
# Unblock-File -LiteralPath $PSCommandPath

$regPath = 'HKLM:\SYSTEM\CurrentControlSet\Control\Keyboard Layout'
$valName = 'Scancode Map'

# Timestamped backup in %TEMP%
$timestamp = Get-Date -Format 'yyyyMMdd_HHmmss'
$backup = Join-Path $env:TEMP "ScancodeMapBackup_$timestamp.reg"

# Export current key (safe even if value missing)
reg export "HKLM\SYSTEM\CurrentControlSet\Control\Keyboard Layout" "$backup" /y | Out-Null
Write-Host "Backup exported to: $backup"

# Caps Lock (0x3A) -> Left Ctrl (0x1D)
# DWORDs are little-endian; EntryCount=2 (1 mapping + terminator)
$sc = [byte[]](0,0,0,0, 0,0,0,0, 2,0,0,0, 0x1D,0,0x3A,0, 0,0,0,0)

# Ensure key exists and set value
New-Item -Path $regPath -Force | Out-Null
Set-ItemProperty -Path $regPath -Name $valName -Type Binary -Value $sc

# Validate
$applied = (Get-ItemProperty -Path $regPath -Name $valName).$valName
if ($applied -and ($applied -join ',') -eq ($sc -join ',')) {
    Write-Host "Scancode Map applied successfully."
} else {
    Write-Warning "Scancode Map write may have failed. Check permissions and try again."
}

Write-Host "Done. Reboot to apply the remap."

