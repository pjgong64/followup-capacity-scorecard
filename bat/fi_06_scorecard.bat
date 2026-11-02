@echo off
rem ===========================================================================
rem  fi_06_scorecard.bat -- Study 3 (FI) step 6. No model fitting.
rem  Writes fi_out\fi06_log.txt. Paste that file back into the chat.
rem  Expected run time: a few minutes.
rem ===========================================================================
setlocal

set "RSCRIPT="C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe""
if not exist "C:\Program Files\R\R-4.6.0\bin\x64\Rscript.exe" set "RSCRIPT="

cd /d "%~dp0"

if defined RSCRIPT goto :haveR
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT="%%D\bin\x64\Rscript.exe""
for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT="%%D\bin\x64\Rscript.exe""
for /d %%D in ("%ProgramFiles(x86)%\R\R-*") do if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT="%%D\bin\x64\Rscript.exe""
for /d %%D in ("C:\R\R-*") do if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT="%%D\bin\x64\Rscript.exe""
if defined RSCRIPT goto :haveR
where Rscript >nul 2>&1
if not errorlevel 1 set "RSCRIPT=Rscript"

:haveR
echo.
echo ============== FI step: fi_06_scorecard ==============
echo folder  : %CD%
if not defined RSCRIPT goto :norscript
echo Rscript : %RSCRIPT%
if not exist "fi_06_scorecard.R" goto :missing

%RSCRIPT% fi_06_scorecard.R
if errorlevel 1 (
  echo.
  echo *** fi_06_scorecard.R stopped with an error. Copy everything above into the chat. ***
  goto :end
)
echo.
echo Finished. Open fi_out\fi06_log.txt and fi_out\fi06_scorecard_main.md and paste both into the chat.
goto :end

:norscript
echo Rscript.exe not found. Install R or edit the RSCRIPT line at the top of this file.
goto :end

:missing
echo fi_06_scorecard.R is not in %CD%
goto :end

:end
pause
endlocal
