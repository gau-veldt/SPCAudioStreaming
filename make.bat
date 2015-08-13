@echo off
asar project.asm program.sfc 1>errors.txt 2>&1
rem start ..\..\..\ZSNES\zsnesw.exe -d program.sfc
copy program.sfc program.rom
