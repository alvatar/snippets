//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

  ToDo:
      - more unit tests

*/////////////////////////////////////////////////////////////////////////

module nova.math.optimization.partitionedbooleanquadric;


private import nova.math.matrix;
private import nova.ds.graph.weighted;

private import tango.util.log.Log;
private import tango.io.Stdout;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.PBQP");
}

alias Vector!(double) DoubleVector;
alias Matrix!(double) DoubleMatrix;

//////////////////////////////////////////////////////////////////////////
/**

  Implementation of a partitioned binary quadric solver as described
    in papers by Eckstein et al. [SchEck02, SchEck03]
    The partitioned binary quadric problem is a mathematical optimization
    problem. Goal is to minimize a graph with minimal costs.

    Eckstein maps compiler specific tasks like instruction selection
    and register allocation to the PBQ problem, passes it to the solver.
    The solution is then propagated back to the original problem and
    used to create an optimal instruction sequence.

  There is another paper in german which describes the implementation
    of an automatic code generator using a PBQP solver [Jaks04].

  References:

    [SchEck02] B. Scholz and E. Eckstein. Register allocation for
                  irregular architecture. In Proceedings of Languages,
                  Compilers, and Tools for Embedded Systems (LCTES'02)
                  and Software and Compilers for Embedded Systems (SCOPES'02),
                  Berlin, June 2002. ACM.

    [SchEck03] E. Eckstein and B. Scholz. Address mode selection.
                  In Proceedings of the International Symposium of Code
                  Generation and Optimization (CGO'03),
                  San Francisco, March 2003. IEEE/ACM.

      [Jaks04] Hannes Jakschitsch. Befehlsauswahl auf SSA-Graphen.
               Diplomarbeit am IPD Goos, Universität Karlsruhe, 2004

*/////////////////////////////////////////////////////////////////////////
class PBQPSolver
{
  alias int Node;

  /// inserts node into the graph
  void insertNode(int u, DoubleVector costs)
  {
    graph.insertNode(u, costs);
  }

  /// add costs to cost vector of a node
  void addNodeCosts(int u, DoubleVector costs)
  {
    graph.addNodeCosts(u, costs);
  }

  /// get node costs
  DoubleVector getNodeCosts(int u)
  {
    return graph.getNodeCosts(u);
  }

  /// add edge costs (matrix)
  void addEdgeCost(int u, int v, DoubleMatrix costs)
  {
    // does the edge u-v exists ?
    if (u == v)
    {
      DoubleVector diag = costs.diagonalize();
      addNodeCosts(v, diag);
    }
    else
    {
      DoubleMatrix ecosts = graph.getEdgeCosts(u,v);

      if (ecosts !is null)
      {
        if ( u < v)
        {
          ecosts += costs;
        }
        else
        {
          ecosts += costs.transpose();
        }
      }
      else
      {
        ecosts = ((u < v) ? costs : costs.transpose());
        graph.insertEdge(u, v, ecosts);
      }
    }
  }

  /// remove bucket from bucket list
  void removeBucket(int deg, int u)
  {
    NodeList list;
    foreach(int pos, int n; bucketList[deg])
    {
      if (n != u)
      {
        list ~= n;
      }
    }

    bucketList[deg] = list;
  }

  /// reorder node
  void reorderNode(int u)
  {
    // get current node degree
    int newdeg = graph.degree(u);
    int olddeg = nodeDegree[u];

    // reorder bucket from only
    //   if degree of node has changed.
    if (newdeg != olddeg)
    {
      removeBucket(olddeg, u);
      bucketList[newdeg] ~= u;
    }
  }

  /// reorder adjacency nodes
  void reorderAdjNodes(int u)
  {
    foreach(graph.Edge edge; graph.getAdjList(u))
    {
      reorderNode(edge.to);
    }
  }

/*****************************************************************************
 * edge functions
 ****************************************************************************/

  /**
    insert edge to graph
    (does not check whether edge exists in graph)
  */
  void insertEdge(int u, int v, DoubleMatrix costs)
  {
    // create entry for u -> v
    graph.insertEdge(u, v, costs);

    // create entry for v -> u
    graph.insertEdge(v, u, costs);
  }

  /// delete edge
  void deleteEdge(int u,int v)
  {
    // remove entry for u -> v
    graph.removeEdge(u, v);

    // remove entry for v -> u
    graph.removeEdge(v, u);
  }

  /// create bucket list
  void createBucketList()
  {
    // determine max degree
    int maxDegree = 2;
    foreach(graph.AdjacencyList adj; graph)
    {
      if (maxDegree <= adj.degree)
        maxDegree = adj.degree;
    }

    // resize bucketList
    nodeDegree.length = maxDegree + 1;
    bucketList.length = maxDegree + 1;

    // insert nodes to the list
    foreach(graph.AdjacencyList adj; graph)
    {
      nodeDegree[adj.source]  = adj.degree;
      bucketList[adj.degree] ~= adj.source;
    }
  }

/**********************************************************************************
 * pop functions
 **********************************************************************************/

  /// pop node of certain degree
  int popNode(int degree)
  {
    int u;

    int len = bucketList[degree].length;
    if (len)
    {
      // get first bucket of bucket list
      // and remove bucket
      u = bucketList[degree][length-1];
      bucketList[degree].length = len - 1;

      return u;
    }

    return -1;
  }

  /// pop node with maximal degree
  int popMaxNode()
  {
    int deg;
    int u;

    for(deg = bucketList.length-1; deg > 2; deg--)
    {
      int len = bucketList[deg].length;
      if (len)
      {
        u = bucketList[deg][length-1];
        bucketList[deg].length = len - 1;

        return u;
      }
    }
    return -1;
  }

  /*****************************************************************************
   * PBQP simplification for trivial nodes
   ****************************************************************************/

  /// remove trivial node with cost vector length of one
  void disconnectTrivialNode(int u)
  {
    DoubleMatrix  c_uv;
    DoubleVector  c_v;

    // add edge costs to node costs of adj. nodes
    foreach(graph.Edge edge; graph.getAdjList(u))
    {
      Node v = edge.to;

      // convert matrix to cost vector offset for adj. node
      c_uv = edge.costs;
      c_v  = c_uv.getRow(0);
      addNodeCosts(v, c_v);

      deleteEdge(u, v);
    }
  }

  /// find all trivial nodes and disconnect them
  void eliminateTrivialNodes()
  {
    foreach(graph.Node node; graph)
    {
      if (node.costs.length == 1)
      {
        disconnectTrivialNode(node.id);
      }
    }
  }

  /*****************************************************************************
   * Normal form for PBQP
   ****************************************************************************/

  /**
    simplify a cost matrix. If the matrix
    is independent, then simplify_matrix
    returns TRUE - otherwise FALSE. In
    vectors u and v the offset values of
    the decomposition are stored.
    */
  bool normalizeMatrix(DoubleMatrix m, DoubleVector u, DoubleVector v)
  {
    int rows,
        cols;
    int r,
        c;

    assert( u.length > 0);
    assert( v.length > 0);

    rows = m.Rows;
    cols = m.Columns;

    assert(rows == u.length);
    assert(cols == v.length);

    // determine u vector
    for(r=0; r<rows ;r++)
    {
      double min = m.rowMin(r);
      u.Data[r] += min;
      if (!(min == double.infinity))
      {
        m.subRow(r, min);
      }
      else
      {
        m.setRow(r, 0);
      }
    }

    // determine v vector
    for(c=0;c<cols;c++)
    {
      double min = m.colMin(c);
      v.Data[c] += min;
      if (!(min == double.infinity))
      {
        m.subCol(c,min);
      }
      else
      {
        m.setCol(c,0);
      }
    }

    // determine whether matrix is
    // independent or not.
    return m.isZero();
  }

  /// simplify single edge
  void simplifyEdge(int u,int v)
  {
    DoubleMatrix costs;
    bool          is_zero;

    // swap u and v  if u > v in order to avoid un-necessary
    // tranpositions of the cost matrix
    if (u > v)
    {
      int swap = u;
      u = v;
      v = swap;
    }

    // get cost matrix and simplify it
    costs   = graph.getEdgeCosts(u,v);
    is_zero = normalizeMatrix(costs, getNodeCosts(u), getNodeCosts(v));

    // delete edge
    if (is_zero)
    {
      graph.removeEdge(u,v);
//      changed = true;
    }
  }

  /**
    normalize cost matrices and remove
    edges in PBQP if they ary independent,
    i.e. can be decomposed into two
    cost vectors.
    */
  void eliminateIndependentEdges()
  {
    int u,v;
    int step = 1;

    for(u=0; u < getNumNodes(); u++)
    {
      foreach(graph.Edge edge; graph.getAdjList(u))
      {
        if (u < edge.to)
        {
            simplifyEdge(u, edge.to);
        }
      }
    }
  }

  /*****************************************************************************
   * PBQP reduction rules
   ****************************************************************************/

  /**
    RI reduction
    This reduction rule is applied for nodes
    of degree one.
    */
  void apply_RI(int x)
  {
    int y,
        i,j;
    int xlen;
    int ylen;
    DoubleMatrix c_yx;
    DoubleVector c_x, delta;

    assert(x >= 0 && x < getNumNodes());

    /* get adjacence matrix */
    graph.AdjacencyList vlist = graph.getAdjList(x);

    graph.Edge ev = vlist.getEdgeAtIndex(0);
    y = ev.to;

    // determine length of cost vectors for node x and y
    xlen = getNodeCosts(x).length;
    ylen = getNodeCosts(y).length;

    // get cost vector c_x and matrix c_yx
    c_x  = getNodeCosts(x);
    c_yx = graph.getEdgeCosts(y, x);

    // allocate delta vector
    delta = new DoubleVector(ylen);

    // compute delta vector
    for(i=0; i<ylen; i++)
    {
      double min =  c_yx[i,0] + c_x[0];

      for(j=0; j<xlen; j++)
      {
        double c =  c_yx[i,j] + c_x[j];
        if ( c < min )
          min = c;
      }
      delta[i] = min;
    }

    // add delta vector
    getNodeCosts(y) += delta;

    // delete node x
    removeNode(x);

    // reorder adj. nodes of node x
    reorderAdjNodes(x);

    // push node x on stack
    stack ~= x;
  }

  /**
    RII reduction
    This reduction rule is applied for nodes
    of degree two.
  */
  void apply_RII(int x)
  {
    int y,z;
    int xlen,ylen,zlen;
    int i,j,k;

    DoubleMatrix c_yx, c_zx;
    DoubleVector cx;
    DoubleMatrix delta;

    assert(x >= 0 && x < getNumNodes());

    // get adjacence matrix
    graph.AdjacencyList vlist = graph.getAdjList(x);

    // get adjacence matrix
    y = vlist.getEdgeAtIndex(0).to;
    z = vlist.getEdgeAtIndex(1).to;

    // determine length of cost vectors for node x and y
    xlen = getNodeCosts(x).length;
    ylen = getNodeCosts(y).length;
    zlen = getNodeCosts(z).length;

    // get cost vector c_x and matrix c_yx
    cx   = getNodeCosts(x);
    c_yx = graph.getEdgeCosts(y,x);
    c_zx = graph.getEdgeCosts(z,x);

    // allocate delta matrix
    delta = new DoubleMatrix(ylen, zlen);

    // compute delta matrix
    for(i=0;i<ylen;i++)
    {
      for(j=0;j<zlen;j++)
      {
        double min = c_yx[i,0] + c_zx[j,0] + cx[0];
        for(k=1; k < xlen; k++)
        {
          double c = c_yx[i,k] + c_zx[j,k] + cx[k];
          if ( c < min )
          {
            min = c;
          }
        }
        delta[i,j] = min;
      }
    }

    // add delta matrix
    addEdgeCost(y,z,delta);

    // delete node x
    removeNode(x);

    // simplify cost matrix c_yz
    simplifyEdge(y,z);

    // reorder adj. nodes
    reorderAdjNodes(x);

    // push node x on stack
    stack ~= x;

  }

  /**
    RN reduction
    This reduction rule is applied for nodes
    of degree N.
    */
  void apply_RN(int x)
  {
    int    sol,
           min_sol = 0,
           xlen;
    double min = 0;

    xlen = getNodeCosts(x).length;

    // after application of RN rule no optimality
    //   can be guaranteed!
    optimal = false;

    // determine local minimum
    for(sol=0; sol < xlen; sol++)
    {
      double h = getNodeCosts(x)[sol];

      foreach(graph.Edge edge; graph.getAdjList(x))
      {
        int y = edge.to;
        DoubleMatrix c_xy = graph.getEdgeCosts(x, y);
        DoubleVector v    = getNodeCosts(y).dup();

        v.addRow(c_xy, sol);
        h = h + v.min;
      }
      if (h < min || sol == 0)
      {
        min = h;
        min_sol = sol;
      }
    }
    solution[x] = min_sol;

    // add solution costs to minimum
    minimum = getNodeCosts(x)[min_sol];

    // add cost vectors to adj. nodes of node x
    foreach(graph.Edge edge; graph.getAdjList(x))
    {
      int y = edge.to;
      DoubleMatrix c_xy = graph.getEdgeCosts(x,y);
      getNodeCosts(y).addRow(c_xy,min_sol);
    }

    // push node x on stack
    stack ~= x;

    // delete node x
    removeNode(x);

    // reorder adj. nodes of node x
    reorderAdjNodes(x);
  }

  /*****************************************************************************
   * PBQP graph parsing
   ****************************************************************************/

  /// reduce pbqp problem (first phase)
  void reducePBQP()
  {
    int u;
    int step=1;
    static char txt[100];

    for(;;)
    {
      if (bucketList[1].length != 0)
      {
        u = popNode(1);

        apply_RI(u);
      }
      else if (bucketList[2].length != 0)
      {
        u = popNode(2);

        apply_RII(u);
      }
      else if ((u = popMaxNode()) != -1)
      {
        apply_RN(u);
      }
      else
      {
        break;
      }
    }
  }

  /*****************************************************************************
   * PBQP back propagation
   ****************************************************************************/

  /**
    determine solution of a reduced node. Either
    RI or RII was applied for this node.
    */
  void determineSolution(int x)
  {
    DoubleVector v = getNodeCosts(x).dup();

    foreach(graph.Edge edge; graph.getAdjList(x))
    {
      int y     = edge.to;
      int y_sol = solution[y];

      DoubleMatrix c_yx = graph.getEdgeCosts(y,x);

      v.addRow(c_yx, y_sol);
    }
    solution[x] = v.minIdx();
  }

  /// prints the solution
  void printSolution()
  {
    Stdout("solution: ").newline;
    foreach(int key, int sol; solution)
    {
      Stdout.formatln("({}, {}) ", key, sol);
    }
    Stdout.newline;
  }

  /// back popagation phase of PBQP
  void backPropagate()
  {
    printSolution();

    for(int i=stack.length-1; i >= 0; i--)
    {
      int x = stack[i];

      reinsertNode(x);
      if (!(x in solution))
      {
        determineSolution(x);
      }
    }
  }

  // solve trivial nodes of degree zero
  void solveTrivialNodes()
  {
    while (bucketList[0].length != 0)
    {
      int u = popNode(0);

      solution[u] = getNodeCosts(u).minIdx;

      minimum += getNodeCosts(u)[solution[u]];
    }
  }

  /*****************************************************************************
   * debug facilities
   ****************************************************************************/
  void check()
  {
  }

  /*****************************************************************************
   * PBQP solve routines
   ****************************************************************************/

   /// solve PBQP problem
  void solve()
  {
    // check vector & matrix dimensions
    check();

    // simplify PBQP problem
    // eliminate trivial nodes, i.e.
    // nodes with cost vectors of length one.
    eliminateTrivialNodes();

    // eliminate edges with independent
    // cost matrices and normalize matrices
    eliminateIndependentEdges();

    // create bucket list for graph parsing
    createBucketList();

    // reduce phase
    reducePBQP();

    // solve trivial nodes
    solveTrivialNodes();

    // back propagation phase
    backPropagate();

    solved = true;
  }

  /// get minimum of PBQP
  double getMin()
  {
    return minimum;
  }

  /// get solution of a node
  int getSolution(int u)
  {
    return solution[u];
  }

  /// get number of nodes
  int getNumNodes()
  {
    return graph.getNumNodes;
  }

  /// is solution optimal
  bool isOptimal()
  {
    return optimal;
  }

  protected:

/*****************************************************************************
 * node functions
 ****************************************************************************/

    ///
    void reinsertNode(int u)
    {
      // reinsert all edges pointing to the node
      foreach(graph.Edge edge; graph.getAdjList(u))
      {
        graph.insertEdge(edge.to, edge.from, edge.costs);
      }
    }

    /// remove node
    void removeNode(int u)
    {
      // remove all edges pointing to the node
      foreach(graph.Edge edge; graph.getAdjList(u))
      {
        graph.removeEdge(edge.to, edge.from);
      }
    }

    this()
    {
      solved  = false;
      optimal = true;
      graph   = new WeightedGraph!(DoubleVector, DoubleMatrix);
      minimum = 0.0;
    }

  // members
  protected:
    alias int       NodeId;

    bool            solved;                   /// true if solved

    bool            optimal;                  /// true if optimal

    WeightedGraph!(DoubleVector, DoubleMatrix) graph;
                                              /// internal graph representation

    typedef Node[]  NodeList;

    /**
      the bucketlist holds a list of nodes
      for every possible degree of a node (see createBucketList)
        */
    NodeList[]      bucketList;

    /**
      used by reorder node to determine previous
      node degree
        */
    int[]           nodeDegree;

    double          minimum;                  /// current minimum
    int[NodeId]     solution;                 /// order of node reduction
    int[]           stack;                    /// for backpropagation

}

unittest
{
  logger.info("Start unittest");

  PBQPSolver solver = new PBQPSolver();

  DoubleVector[] vertexCosts;

  solver.insertNode(0, new DoubleVector(6.0, 7.0));

  solver.insertNode(1, new DoubleVector(5.0, 3.0));

  solver.insertEdge(0, 1, new DoubleMatrix(2, 2,
                                           2.0, 7.0,
                                           0.0, 9.0));

  logger.info("Try solve...\n");

  solver.solve();

  Stdout.format("solver.min = {}", solver.getMin).newline;

  logger.info("Done");
}
