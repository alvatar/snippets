dmc -c rgbe.c -orgbe_c.obj
rebuild -I..\.. -I..\ext testBSP.d rgbe_c.obj -release -inline -O -full
