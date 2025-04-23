Set WShell = CreateObject("WScript.Shell")
WShell.Popup "KiwiFlow will open shortly, please wait...", 3, "KiwiFlow", 0
WShell.Run "powershell.exe -NoExit -Command ""conda activate kiwiflow; Rscript -e \""shiny::runApp('C:\\Users\\Admin\\Desktop\\KiwiFlow\\app.R', port=3838, launch.browser = T)\""", 0