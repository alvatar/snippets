
CHICKEN_ROOT?=/usr
CHICKEN_INCLUDE_DIR=$(CHICKEN_ROOT)/include/chicken
CHICKEN_LIB_DIR=$(CHICKEN_ROOT)/lib

test: main.cpp
	$(CXX) -Wno-write-strings -o test main.cpp -lchicken -lc -linterval_graph_connection_generator -L. -L$(CHICKEN_LIB_DIR) -I. -I$(CHICKEN_INCLUDE_DIR) -DC_EMBEDDED -Wl,-rpath=${CHICKEN_LIB_DIR}:.

interval_graph_connection_generator.o: interval_graph_connection_generator.cpp interval_graph_connection_generator.h
	$(CXX) -Wno-write-strings -c -fpic $< -o $@ -I. -I$(CHICKEN_INCLUDE_DIR) -DC_EMBEDDED

%.so: %.o
	$(CXX) -shared -lc -lchicken -L$(CHICKEN_LIB_DIR) -Wl,-rpath=${CHICKEN_LIB_DIR} -o $@ $<

