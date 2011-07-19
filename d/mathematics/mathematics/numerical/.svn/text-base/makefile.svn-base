#Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.

DCOMPILER=gdc

OBJECTS_mathematics_numerical:=$(patsubst %.d, %.o, $(wildcard *.d))
OBJECTS_mathematics_numerical_algorithms:=$(patsubst algorithms/%.d, algorithms/%.o, $(wildcard algorithms/*.d))
#OBJECTS_ALL_mathematics_numerical:= $(OBJECTS_mathematics_numerical) $(OBJECTS_mathematics_numerical_algorithms)
OBJECTS_ALL_mathematics_numerical:= $(OBJECTS_mathematics_numerical) 


#libraries must start with lib
#TARGET_PROJECT=../lib/libmathematics-numerical.a
TARGET_PROJECT=../../lib/libmathematics-numerical.a
IMPORT_PATH=../..


all: $(TARGET_PROJECT)

$(TARGET_PROJECT): $(OBJECTS_mathematics_numerical)
	make -C algorithms
#	cc -shared -o $@ $(OBJECTS_ALL_mathematics_numerical)
	ar rcs $@ $(OBJECTS_ALL_mathematics_numerical)	
#	cc -o $@ $(OBJECTS_ALL_mathematics_numerical)

%.o: %.d
#	$(DCOMPILER) -c -fPIC $< -I$(IMPORT_PATH)
	$(DCOMPILER) -c $< -I$(IMPORT_PATH)

clean:
	-make -C algorithms clean
	@-rm *.o
	@-rm *~
	@-rm $(TARGET_PROJECT)
