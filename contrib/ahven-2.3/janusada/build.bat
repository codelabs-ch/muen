call janusada\update.bat
if ErrorLevel 2 goto abort
call janusada\compile.bat
:abort