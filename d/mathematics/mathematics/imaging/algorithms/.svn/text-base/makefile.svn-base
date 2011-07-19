

OBJECTS_mathematics_imaging_algorithms:=$(patsubst %.d, %.o, $(wildcard *.d))
IMPORT_PATH=../../..

all: $(OBJECTS_mathematics_imaging_algorithms)

%.o: %.d
#	dmd -c -fPIC $< -I$(IMPORT_PATH)
	gdc -c $< -I$(IMPORT_PATH)
	

clean:
	@-rm *.o
	@-rm *~
