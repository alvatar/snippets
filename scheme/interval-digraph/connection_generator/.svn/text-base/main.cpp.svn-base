
#include <assert.h>
#include <chicken.h>

#include "connection_generator.h"
#include "interval_graph_connection_generator.h"

extern int scheme_initialize ();

typedef C_word value;

int main (int argc, char **argv)
{
     value graph;

     scheme_initialize();

     assert (CHICKEN_load ("interval_digraph_test.scm"));

     assert (CHICKEN_eval_string ("(interval-digraph-test)", &graph));

     IntervalGraphConnectionGenerator igcg = IntervalGraphConnectionGenerator (graph);

     ConnectionGenerator::ClosedInterval ci1 = ConnectionGenerator::ClosedInterval (0, 100);
     ConnectionGenerator::ClosedInterval ci2 = ConnectionGenerator::ClosedInterval (200, 300);

     ConnectionGenerator::ClosedInterval v[] = {ci1, ci2};
     ConnectionGenerator::IntervalSet is;
     is.insert (0, 100);
     is.insert (200, 300);

     ConnectionGenerator::Mask m;
     m.sources = is;
     m.targets = is;

     igcg.setMask (m);

     igcg.start();

     int i=0, j=0; bool more = false;
     double labels[1];

     do {
	  more = igcg.next (i, j, labels);
	  printf ("i,j = %d,%d\n", i,j);
     } while (more);

     return 0;
}
