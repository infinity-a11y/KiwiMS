@echo off
echo Starting activation test...
call "C:\ProgramData\miniconda3\Scripts\activate.bat" kiwims
if errorlevel 1 (
    echo ACTIVATION FAILED - wrong name/path or permissions?
    exit /b 1
)
echo Activation OK!
echo Current prefix: %CONDA_PREFIX%
echo Python version:
python --version
echo Python exe:
python -c "import sys; print(sys.executable)"
echo llvmlite test:
python -c "import llvmlite.binding; print('OK - DLL loaded')"
pause