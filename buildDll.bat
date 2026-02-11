@echo off
IF NOT EXIST build mkdir build

odin build src -out:build/yap.dll -subsystem:console -show-timings -warnings-as-errors -build-mode:dll -target:windows_amd64 

