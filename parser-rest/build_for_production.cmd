stack clean

stack build


::
:: START of finding the target dist directory (it's a hash, so it changes)
::
@ECHO OFF
cd .stack-work/dist

FOR /D %%a in (*) DO (
  SET pathHash=%%a
  cd ../..
  GOTO ENDLOOP
)
:ENDLOOP
@ECHO ON
::
:: END of finding the target dist directory (it's a hash, so it changes)
::


SET exePath=.stack-work\dist\%pathHash%\build\parser-rest-exe

::
:: Copy assets
::
xcopy /E /H /I /Y .\assets\config .\%exePath%\assets\config
xcopy /E /H /I /Y .\assets\file_db .\%exePath%\assets\file_db


::
:: Switch config to production
::
rename .\%exePath%\assets\config\project-environment.ini project-environment.dev.ini
rename .\%exePath%\assets\config\project-environment.prod.ini project-environment.ini


::
:: Zip assets and exe
::
7z a .\%exePath%\parser-rest.zip .\%exePath%\assets .\%exePath%\parser-rest-exe .\%exePath%\parser-rest-exe.exe
