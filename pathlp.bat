@echo on
setlocal

REM          PathLP WINDOWS and DOS invocation script

REM ---------------------------------------------------
REM You should not touch anything above.

REM You are recommended to avoid usage of letters in {a, b, f, n, r, t, v, x}
REM as first letter of your files or folders.

REM You should fill these fields.

REM You should fill the address of the file xsb.bat (xsb64.bat for 64-bit).
set xsb="O:\Desktop\PathLP\PathLP_public\XSB\bin\xsb64.bat"

REM You can fill the address of your computer temporary files directory.
REM You should use / in place of \, or be careful using \ and put \\ in needed places.
set tempdir=
REM Empty value means allowing to PathLP find such a directory.

REM You should not touch anything below.
REM ---------------------------------------------------

IF NOT DEFINED xsb (
echo You should configure the system by changing the file pathlp.bat 1>&2
endlocal
set errorlevel=1
pause
goto a
)
set pathlp=%0
for %%F in (%pathlp%) do set pathlpadd=%%~dpF
call set pathlpaddr=%%pathlpadd:\=/%%%

 
COLOR A
%xsb% %xsbparam% -l --nobanner --quietload --noprompt -e "catch((['%pathlpaddr%/src/module'], module:'_^pathlp_247_ main'('%*', '%pathlpaddr%', '%tempdir%')), error(A, B, _), (error_handler:get_sys_error_description(error(A, B), M), standard:messageln(M, 2), writeln(2, 'Press Ctrl-Z Enter to exit...'), read(0, _), halt))."
endlocal
COLOR
pause
:a
