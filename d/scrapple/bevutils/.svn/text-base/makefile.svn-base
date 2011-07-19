# Makefile to build D library bevutils.lib for Win32
# Designed to work with \dm\bin\make.exe
# Targets:
#	make
#		Same as make bevutils.lib
#	make bevutils.lib
#		Build bevutils.lib
#	make clean
#		Delete unneeded files created by build process
# Notes:
#	This relies on LIB.EXE 8.00 or later, and MAKE.EXE 5.01 or later.

CP=cp

CFLAGS=-mn -6 -r

DFLAGS=-O -release -nofloat -w

CC=dmc

DMD=\dmd\bin\dmd

DOC=.\docs

.c.obj:
	$(CC) -c $(CFLAGS) $*

.cpp.obj:
	$(CC) -c $(CFLAGS) $*

.d.obj:
	$(DMD) -c $(DFLAGS) $*

.asm.obj:
	$(CC) -c $*

targets : bevutils.lib beepservice.exe html

OBJS= eventlogger.obj log4d.obj tinyxml.obj propertyfile.obj servicebase.obj serviceimplementation.obj

SRC=	eventlogger.d log4d.d tinyxml.d propertyfile.d servicebase.d serviceimplementation.d

DOCS= eventlogger.html log4d.html tinyxml.html propertyfile.html servicebase.html serviceimplementation.html beepservice.html


bevutils.lib : $(OBJS)
	lib -c -p32 bevutils.lib $(OBJS)

beepservice.exe : $(OBJS) beepservice.d
	$(DMD) -ofbeepservice.exe -I eventlogger.d -I log4d.d -I tinyxml.d -I propertyfile.d -I servicebase.d -I serviceimplementation.d $(DFLAGS) \
	beepservice.d $(OBJS) advapi32.lib

html : $(DOCS)

clean:
	del $(OBJS)
	del beepservice.exe
	del beepservice.map
	del beepservice.obj
	del bevutils.bak



######################################################

eventlogger.obj : eventlogger.d
	$(DMD) -c $(DFLAGS) eventlogger.d
log4d.obj : log4d.d
	$(DMD) -c $(DFLAGS) log4d.d
tinyxml.obj : tinyxml.d
	$(DMD) -c $(DFLAGS) tinyxml.d
propertyfile.obj : propertyfile.d
	$(DMD) -c -I tinyxml.d $(DFLAGS) propertyfile.d
servicebase.obj : servicebase.d
	$(DMD) -c -I eventlogger.d $(DFLAGS) servicebase.d
serviceimplementation.obj : serviceimplementation.d
	$(DMD) -c -I eventlogger.d -I log4d.d -I tinyxml.d -I propertyfile.d -I servicebase.d $(DFLAGS) serviceimplementation.d

######################################################

eventlogger.html : eventlogger.d
	$(DMD) -c -o- -Dddocs eventlogger.d
tinyxml.html : tinyxml.d
	$(DMD) -c -o- -Dddocs tinyxml.d
log4d.html : log4d.d
	$(DMD) -c -o- -Dddocs log4d.d
propertyfile.html : propertyfile.d
	$(DMD) -c -o- -I tinyxml.d -Dddocs propertyfile.d
servicebase.html : servicebase.d
	$(DMD) -c -o- -I eventlogger.d -Dddocs servicebase.d
serviceimplementation.html : serviceimplementation.d
	$(DMD) -c -o- -I eventlogger.d -I log4d.d -I tinyxml.d -I propertyfile.d -I servicebase.d -Dddocs serviceimplementation.d
beepservice.html : beepservice.d
	$(DMD) -c -o- -I eventlogger.d -I log4d.d -I tinyxml.d -I propertyfile.d -I servicebase.d -I serviceimplementation.d -Dddocs beepservice.d
