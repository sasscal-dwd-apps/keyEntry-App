
REM Get the path where the files are located
SET mypath=%~dp0
echo %mypath:~0,-1%

REM Replace "\" with "\\" so that the path can be read by R
setlocal ENABLEDELAYEDEXPANSION
set word=\\
set str=%mypath%
set str=%str:\=!word!%
echo %str%
c:

REM set R path
cd "C:\Program Files\R\R-3.3.2\bin\i386"

REM set number of users
set users=1

FOR /L %%i IN (1,1,!users!) DO (
start cmd /k R -e  "setwd('%str%'); usr_nr <<- sprintf('%%02d', %%i); source('master.R')"

)