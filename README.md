# <img src="app/static/logo_name.png" width="55%"/>


### Installation on Microsoft Windows


Head to an accessible and writable directory where you wish to install KiwiFlow to, e.g. `C:\Users\Admin\Desktop`. Execute the following command in PowerShell (<i>Run as Administrator</i>) to download the source code and run the setup. 
``` PowerShell
Invoke-WebRequest -Uri "https://github.com/infinity-a11y/KiwiFlow/archive/refs/heads/master.zip" -OutFile "KiwiFlow-master.zip"; Expand-Archive -Path "KiwiFlow-master.zip" -DestinationPath "." -Force; Remove-Item "KiwiFlow-master.zip"; Rename-Item "KiwiFlow-master" "KiwiFlow"; & ".\KiwiFlow\setup_kiwiflow.ps1"
```

After successful setup, KiwiFlow can be launched by clicking the desktop icon created.
