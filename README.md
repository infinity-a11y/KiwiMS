<img src="app/static/logo_name.png" width="55%"/>


### Installation on Microsoft Windows


Head to an accessible and writable directory where you wish to install KiwiFlow to, e.g. `C:\Users\Admin\Desktop`. Then execute the following command in PowerShell (<i>Run as Administrator</i>). 
``` PowerShell
Invoke-WebRequest -Uri "https://github.com/infinity-a11y/KiwiFlow/archive/refs/heads/master.zip" -OutFile "KiwiFlow-master.zip"; Expand-Archive -Path "KiwiFlow-master.zip" -DestinationPath "." -Force; Remove-Item "KiwiFlow-master.zip"; Rename-Item "KiwiFlow-master" "KiwiFlow"; & ".\KiwiFlow\setup_kiwiflow.ps1"
```