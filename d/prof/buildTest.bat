call refreshDeps
rm .deps
xfbuild Test.d -oxfProfTest.exe -- -I../.. -g -version=WindowsXP
rm .deps
