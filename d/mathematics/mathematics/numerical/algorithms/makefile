

OBJECTS_mathematics_numerical_algorithms:=$(patsubst %.d, %.o, $(wildcard *.d))
IMPORT_PATH=../../..

TARGET_PROJECT=../../../lib/libmathematics-numerical-algorithms.a


all: $(OBJECTS_mathematics_numerical_algorithms)
	ar crs $(TARGET_PROJECT) $(OBJECTS_mathematics_numerical_algorithms)

%.o: %.d
#	dmd -c -fPIC $< -I$(IMPORT_PATH)
	gdc -c $< -I$(IMPORT_PATH)

clean:
	@-rm *.o
	@-rm *~
	@-rm $(TARGET_PROJECT)
	clear
