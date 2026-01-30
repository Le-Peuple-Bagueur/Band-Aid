@echo off
setlocal

REM Go to folder where this BAT file lives
cd /d "%~dp0"

REM Try to find Rscript on PATH
where Rscript >nul 2>nul
if %errorlevel%==0 (
  echo Using Rscript from PATH
  Rscript run_app.R
  goto :eof
)

REM Fallback: common R install paths (adjust if you want)
for /f "delims=" %%R in ('dir /b /ad "C:\Program Files\R\R-*" 2^>nul') do (
  if exist "C:\Program Files\R\%%R\bin\Rscript.exe" (
    echo Using: C:\Program Files\R\%%R\bin\Rscript.exe
    "C:\Program Files\R\%%R\bin\Rscript.exe" run_app.R
    goto :eof
  )
)

for /f "delims=" %%R in ('dir /b /ad "C:\Program Files (x86)\R\R-*" 2^>nul') do (
  if exist "C:\Program Files (x86)\R\%%R\bin\Rscript.exe" (
    echo Using: C:\Program Files (x86)\R\%%R\bin\Rscript.exe
    "C:\Program Files (x86)\R\%%R\bin\Rscript.exe" run_app.R
    goto :eof
  )
)

echo.
echo ERROR: Rscript.exe not found.
echo Please install R and ensure Rscript is on your PATH, or install R under C:\Program Files\R\.
pause
endlocal