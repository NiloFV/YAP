@echo off
IF NOT EXIST build mkdir build

odin build src -out:build/yap.exe -subsystem:console -show-timings -warnings-as-errors