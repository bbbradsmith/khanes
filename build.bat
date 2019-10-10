@del temp\*.s
@del temp\*.o
@del temp\khan.nes
@del temp\khan.dbg
@del temp\khan.map

cc65\bin\ca65 -o temp\khan.o -g khan.s
@IF ERRORLEVEL 1 GOTO error

cc65\bin\ld65 -o temp\khan.nes -m temp\khan.map --dbgfile temp\khan.dbg -C nrom128.cfg temp\khan.o
@IF ERRORLEVEL 1 GOTO error

@echo.
@echo.
@echo Build successful!
@pause
@GOTO end
:error
@echo.
@echo.
@echo Build error!
@pause
:end