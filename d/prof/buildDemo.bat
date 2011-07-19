rem call refreshDeps
rm .deps
rm GUI.zip
rm xfProf.7z
cp ../hybrid/verdana.ttf ./
cp -R ../hybrid/themes ./
c:\prog\7zip\7z a -r -tzip GUI.zip verdana.ttf themes
rm verdana.ttf
rm -Rf themes
xfbuild Main.d -oR:\xfProf.exe -- -version=DontMountExtra -version=MountZip -inline -release -O -I../.. -I../ext -version=WindowsXP && copy R:\xfProf.exe
upx -9 xfProf.exe
c:\prog\7zip\7z a -r -t7z xfProf.7z xfProf.exe *.dll Main.gui GUI.zip
bash -login -c "scp xfProf.7z h3@team0xf.com:public_html/"
pause
