@echo off

if not exist ..\bin\data mkdir ..\bin\data

echo *** Cleaning ***
if exist ..\bin\SimpleGL.exe del ..\bin\SimpleGL.exe

echo *** Compiling ***
build -full -clean -g SimpleGL.d -I..\..\..\..\ -I..\..\..\ext -version=Debug -version=OldDogInput tango-base-dmd.lib

echo *** Moving executable to ..\bin ***
if exist .\SimpleGL.exe move .\SimpleGL.exe ..\bin\

echo *** Copying data to ..\bin\data ***
if exist .\simpleGL_kernel.cubin copy .\simpleGL_kernel.cubin ..\bin\data\
if exist .\simpleGL.cfg copy .\simpleGL.cfg ..\bin\data\
