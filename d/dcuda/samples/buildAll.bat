@echo off

echo.
echo "building matrixMulDrv"
cd .\matrixMulDrv\
call .\compile.bat
cd ..

echo.
echo "building simpleTextureDrv"
cd .\simpleTextureDrv\
call .\compile.bat
cd ..\

echo.
echo "building simpleGL"
cd .\simpleGL\
call .\compile.bat
cd ..\

echo.
echo "building simpleCUFFT"
cd .\simpleCUFFT\
call .\compile.bat
cd ..\

