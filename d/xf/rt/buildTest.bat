dmc -c rgbe.c -orgbe_c.obj
build -full -clean -I..\.. -I..\ext test.d rgbe_c.obj -inline -release -O
pause
