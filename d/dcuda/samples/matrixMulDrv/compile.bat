@echo off

if not exist ..\bin\data mkdir ..\bin\data

echo *** Cleaning ***
if exist ..\bin\MatrixMulDrv.exe del ..\bin\MatrixMulDrv.exe

echo *** Compiling ***
build -full -clean -g MatrixMulDrv.d -I..\..\..\..\ -version=Debug tango-base-dmd.lib

echo *** Moving executable to ..\bin ***
if exist .\MatrixMulDrv.exe move .\MatrixMulDrv.exe ..\bin\

echo *** Copying cubin to ..\bin\data ***
if exist .\matrixMul_kernel.cubin copy .\matrixMul_kernel.cubin ..\bin\data\
