@echo off
rem fi_10 -- Study 3 (FI): Section 5 deployment tables + Figure 1. Reads fi_out\fi06_*.rds,
rem fi_out\fi03_frontier.csv and 1_data_before_out.txt. Output fi_out\fi10_*. About 2-3 minutes.
setlocal
cd /d "%~dp0"
set "RSCRIPT="
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT="%%D\bin\x64\Rscript.exe""
if not defined RSCRIPT set "RSCRIPT=Rscript"
%RSCRIPT% fi_10_deployment.R
if errorlevel 1 (echo *** fi_10_deployment.R stopped with an error. Copy everything above into the chat. *** & goto :end)
%RSCRIPT% fi_10_figure1.R
:end
pause
endlocal
