@echo off
setlocal
set buildtoolsdir=\\bldsrv\tools
set path=%buildtoolsdir%\masm\6.13\bin;%buildtoolsdir%\masm\6.13\binr
nmake
endlocal