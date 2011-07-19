@echo off

if not exist ..\bin\data mkdir ..\bin\data

echo *** Cleaning ***
if exist ..\bin\SimpleTextureDrv.exe del ..\bin\SimpleTextureDrv.exe

echo *** Compiling ***
build -full -clean -g SimpleTextureDrv.d -I..\..\..\..\ -version=Debug tango-base-dmd.lib

echo *** Moving executable to ..\bin ***
if exist .\SimpleTextureDrv.exe move .\SimpleTextureDrv.exe ..\bin\

echo *** Copying data to ..\bin\data ***
if exist .\data\simpleTexture_kernel.cubin copy .\data\simpleTexture_kernel.cubin ..\bin\data\
if not exist ..\bin\data\lena_bw.pgm copy .\data\lena_bw.pgm ..\bin\data\
if not exist ..\bin\data\ref_rotated.pgm copy .\data\ref_rotated.pgm ..\bin\data\
