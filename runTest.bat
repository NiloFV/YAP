@echo off
call build.bat
START .\build\yap.exe res/simpleTest.yap -v

