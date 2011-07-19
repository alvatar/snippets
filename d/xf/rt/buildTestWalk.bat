dmc -c rgbe.c -orgbe_c.obj
rebuild -I..\.. -I..\ext testWalk.d rgbe_c.obj -full -release -inline -O
