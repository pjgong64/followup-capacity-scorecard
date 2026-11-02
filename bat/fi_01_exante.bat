@echo off
rem ===========================================================================
rem  fi_01_exante.bat -- Study 3 (FI) step 1. Refits LR and XGB WITHOUT the
rem  16 year-end features (outcome = NPL status on 31 Dec, same date).
rem  Two runs: rolling (fit 2020 -> 2021) then main (fit 2020+2021 -> 2022).
rem  Arms: F+SP (FI's model) and F+SP+EO (internal check for the thesis only).
rem  Copies of the published runners are used; the originals are untouched.
rem  Output: factorial7_out\rolling_2020_to_2021\exante\  and  factorial7_out\exante\
rem  Expected run time: roughly 30-40 minutes in total (reads the 3.8 GB model
rem  file twice). No prompts: "auto" is passed.
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
echo ============== FI step 1: ex-ante refit ==============
echo folder  : %CD%
if not defined RSCRIPT goto :norscript
echo Rscript : %RSCRIPT%
if not exist "fi_01_run_exante.R" goto :missing
if not exist "fi_01_run_exante_rolling.R" goto :missing

echo.
echo ---- 1 of 2: rolling (fit 2020, score 2021) ----
%RSCRIPT% fi_01_run_exante_rolling.R LR XGB arms=F+SP,F+SP+EO auto
if errorlevel 1 (
  echo.
  echo *** the rolling ex-ante run stopped with an error. Copy everything above into the chat. ***
  goto :end
)
echo.
echo ---- 2 of 2: main (fit 2020+2021, score 2022) ----
%RSCRIPT% fi_01_run_exante.R LR XGB arms=F+SP,F+SP+EO auto
if errorlevel 1 (
  echo.
  echo *** the main ex-ante run stopped with an error. Copy everything above into the chat. ***
  goto :end
)
echo.
echo Finished. Paste these two files into the chat:
echo   factorial7_out\rolling_2020_to_2021\exante\factorial7_results.csv
echo   factorial7_out\exante\factorial7_results.csv
echo Then run fi_02_pd_correction.bat and fi_03_capacity_frontier.bat.
goto :end

:norscript
echo Rscript.exe not found. Install R or edit the RSCRIPT line at the top of this file.
goto :end

:missing
echo fi_01_run_exante.R or fi_01_run_exante_rolling.R is not in %CD%
goto :end

:end
pause
endlocal
