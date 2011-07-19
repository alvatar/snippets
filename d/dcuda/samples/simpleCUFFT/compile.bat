@echo off

if not exist ..\bin\data mkdir ..\bin\data

echo *** Cleaning ***
if exist ..\bin\SimpleCUFFT.exe del ..\bin\SimpleCUFFT.exe

echo *** Compiling ***
build -full -clean -g SimpleCUFFT.d -I..\..\..\..\ -version=Debug -version=fftRelease tango-base-dmd.lib

echo *** Moving executable to ..\bin ***
if exist .\SimpleCUFFT.exe move .\SimpleCUFFT.exe ..\bin\

echo *** Copying cubin to ..\bin\data ***
if exist .\ComplexPointwiseMulAndScale_kernel.cubin copy .\ComplexPointwiseMulAndScale_kernel.cubin ..\bin\data\
