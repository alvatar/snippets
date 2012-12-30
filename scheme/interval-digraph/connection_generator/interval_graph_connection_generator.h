
#include "connection_generator.h"



class IntervalGraphConnectionGenerator : public ConnectionGenerator 
{
     void *igraph, *source_iset, *target_iset, *remaining_source_iset, *cset, *pmap;
     int source_iset_index, cset_index;

public:

     IntervalGraphConnectionGenerator (int);

     int arity ();
     int size();
     void setMask (std::vector<Mask>& masks, int local);
     void setMask (Mask& mask);
     void start ();
     bool next (int& source, int& target, double* value);

};
