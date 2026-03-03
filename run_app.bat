@echo off
setlocal

REM --------------------------------------------
REM Relaunch this batch minimized on first run
REM --------------------------------------------
if /i not "%~1"=="min" (
  start "" /min cmd /c "%~f0" min
  exit /b
)

REM Go to folder where this BAT file lives
cd /d "%~dp0"

REM 1) Open the status page right away (in the default browser)
if exist "www\install_status.html" (
  start "" "%CD%\www\install_status.html"
)

REM 2) Try to find Rscript on PATH
where Rscript >nul 2>nul
if %errorlevel%==0 (
  echo Using Rscript from PATH
  Rscript --vanilla install_and_launch.R
  goto :eof
)

REM 3) Fallback: common R install paths
for /f "delims=" %%R in ('dir /b /ad "C:\Program Files\R\R-*" 2^>nul') do (
  if exist "C:\Program Files\R\%%R\bin\Rscript.exe" (
    echo Using: C:\Program Files\R\%%R\bin\Rscript.exe
    "C:\Program Files\R\%%R\bin\Rscript.exe" --vanilla install_and_launch.R
    goto :eof
  )
)

for /f "delims=" %%R in ('dir /b /ad "C:\Program Files (x86)\R\R-*" 2^>nul') do (
  if exist "C:\Program Files (x86)\R\%%R\bin\Rscript.exe" (
    echo Using: C:\Program Files (x86)\R\%%R\bin\Rscript.exe
    "C:\Program Files (x86)\R\%%R\bin\Rscript.exe" --vanilla install_and_launch.R
    goto :eof
  )
)

echo.
echo ERROR: Rscript.exe not found.
echo Please install R and ensure Rscript is on your PATH, or install R under C:\Program Files\R\.
pause
endlocal
