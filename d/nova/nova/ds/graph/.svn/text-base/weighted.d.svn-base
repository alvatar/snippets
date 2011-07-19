//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.graph.weighted;

private import tango.util.log.Log;
private import tango.io.Stdout;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.graph.weighted");
}

//////////////////////////////////////////////////////////////////////////
/**

  WeightedGraph template which handles node and edge costs.
  Int values are used for NodeIds.

  Parameters:
    NodeCosts = type used to express node costs
    EdgeCosts = type used to express edge costs

  Usage patterns:

  1. Introduce link(s) in node class
  ---
  Graph!(double, double) g = new Graph!(double, double);
  ---

  2. insert edges with associated costs
  ---
  g.insertEdge(1, 2, 2.2);
  g.insertEdge(1, 3, 3.3);
  g.insertEdge(3, 4, 4.4);
  g.insertEdge(4, 5, 5.5);
  g.insertEdge(4, 6, 6.6);
  g.insertEdge(5, 7, 7.7);
  ---

  3. add node costs
  ---
  g.addNodeCosts(1, 1.1);
  g.addNodeCosts(2, 2.1);
  g.addNodeCosts(3, 3.1);
  g.addNodeCosts(4, 4.1);
  g.addNodeCosts(5, 5.1);
  g.addNodeCosts(6, 6.1);
  g.addNodeCosts(7, 7.1);
  g.addNodeCosts(8, 8.1);
  ---

  3. output graph
  ---
  g.print();
  ---

  4. traverse nodes
  ---
  foreach(g.Node n; g)
  {
    writef(std.string.toString(n.id), " ", std.string.toString(n.costs), "\n");
  }
  ---

*/////////////////////////////////////////////////////////////////////////
class WeightedGraph(NodeCosts, EdgeCosts)
{
  public:
    alias int NodeId;

    //////////////////////////////////////////////////////////////////////////
    /**

      represents edges in the weigthed graph

    */////////////////////////////////////////////////////////////////////////
    static class Edge
    {
      public:
        NodeId    from;       /// node id of the node the edge originates
        NodeId    to;         /// node id of the node the edge points to
        EdgeCosts costs;      /// costs of edge

        this (NodeId f, NodeId t, EdgeCosts c)
        {
          from  = f;
          to    = t;
          costs = c;
        }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      represents nodes in the weigthed graph

    */////////////////////////////////////////////////////////////////////////
    static class Node
    {
      public:
        NodeId      id;       /// node id
        NodeCosts   costs;    /// node costs

        this(NodeId i, NodeCosts c)
        {
          id    = i;
          costs = c;
        }
    }

  public:
    /// supports foreach over internal adjacency list
    int opApply(int delegate(inout AdjacencyList) dg)
    {
      int res = 0;
      foreach(AdjacencyList adjlist; nodes)
      {
        res = dg(adjlist);
        if (res) break;
      }
      return res;
    }

    /// supports foreach over internal node set
    int opApply(int delegate(inout Node) dg)
    {
      int res = 0;
      foreach(NodeId id, NodeCosts costs; nodecosts)
      {
        Node n = new Node(id, costs);
        res = dg(n);
        if (res) break;
      }
      return res;
    }

  public:
    //////////////////////////////////////////////////////////////////////////
    /**

      implements adjacency list

    */////////////////////////////////////////////////////////////////////////
    static class AdjacencyList
    {
      alias WeightedGraph.Edge Edge;

      /// source node (for convenience)
      NodeId              source;

      /// target nodes (edges)
      NodeId[NodeId]      targets;

      /// edge costs
      EdgeCosts[NodeId]   edgecosts;

      this (NodeId from)
      {
        source = from;
      }

      /// supports foreach over edges
      int opApply(int delegate(inout Edge) dg)
      {
        int res = 0;
        foreach(NodeId t; targets)
        {
          Edge e = new Edge(source, t, edgecosts[t]);
          res = dg(e);
          if (res) break;
        }
        return res;
      }

      /// get edge at given index
      Edge getEdgeAtIndex(int idx)
      {
        NodeId n = targets.values[idx];
        Edge e = new Edge(source, n, edgecosts[n]);
        return e;
      }

      /// add edge to new target
      void addTarget(NodeId n, EdgeCosts c)
      {
        targets[n]    = n;
        edgecosts[n]  = c;
      }

      /// remove edge to target
      void removeTarget(NodeId n)
      {
        targets.remove(n);
        edgecosts.remove(n);
      }

      /// get costs for a edge
      EdgeCosts getTargetCosts(NodeId n)
      {
        assert(n in edgecosts);
        return edgecosts[n];
      }

      /// get node degree
      int degree()
      {
        return targets.length;
      }
    }

    /// insert edge
    void insertEdge(NodeId from, NodeId to, EdgeCosts c)
    {
      if (!(from in nodes))
        nodes[from] = new AdjacencyList(from);

      nodes[from].addTarget(to, c);
    }

    /// remove edge
    void removeEdge(NodeId from, NodeId to)
    {
      assert(from in nodes);
      if (!(from in nodes))
        return;

      nodes[from].removeTarget(to);
    }

    /// insert node
    void insertNode(NodeId n, NodeCosts c)
    {
      if (!(n in nodes))
        nodes[n] = new AdjacencyList(n);

      nodecosts[n] = c;
    }

    /// remove node
    void removeNode(NodeId n)
    {
      if (n in nodes)
      {
        nodes.remove(n);
        nodecosts.remove(n);
      }
    }

    /**
      Adds node costs to a given node. If node is not present it will
      be inserted and the initial costs are set.
    */
    void addNodeCosts(NodeId n, NodeCosts c)
    {
      if (!(n in nodes))
      {
        nodes[n] = new AdjacencyList(n);
        nodecosts[n] = c;
      }
      else
      {
        nodecosts[n] += c;
      }
    }

    /// get node costs for a given node
    NodeCosts getNodeCosts(NodeId n)
    {
      assert(n in nodes);
      if (!(n in nodes))
        return NodeCosts.init;

      return nodecosts[n];
    }

    /// get edge costs for a specific edge
    EdgeCosts getEdgeCosts(NodeId from, NodeId to)
    {
      assert(from in nodes);
      if (!(from in nodes))
        return EdgeCosts.init;

      return nodes[from].getTargetCosts(to);
    }

    /// get number of nodes
    int getNumNodes()
    {
      return nodes.length;
    }

    /// get number of edges
    int getNumEdges()
    {
      int num = 0;
      foreach(AdjacencyList adj; nodes)
      {
        num += adj.degree;
      }
      return num;
    }

    /// get degree of a given node
    int degree(NodeId n)
    {
      return nodes[n].degree;
    }

    /// get adjacency list of a given node
    AdjacencyList getAdjList(NodeId n)
    {
      return nodes[n];
    }

    /// prints graph
    void print()
    {
      foreach(AdjacencyList adj; nodes)
      {
        Stdout(adj.source) (" -> ");
        foreach(Edge e; adj)
        {
          Stdout(e.to) (" ");
        }
        Stdout.newline;
      }
    }

    /// adjacency list for each node
    AdjacencyList[NodeId] nodes;

    /// costs for each node
    NodeCosts[NodeId]     nodecosts;
}

unittest
{
  logger.info("Start unittest");

  WeightedGraph!(double, double) g = new WeightedGraph!(double, double);

  g.insertEdge(1, 2, 2.2);
  g.insertEdge(1, 3, 3.3);
  g.insertEdge(3, 4, 4.4);
  g.insertEdge(4, 5, 5.5);
  g.insertEdge(4, 6, 6.6);
  g.insertEdge(5, 7, 7.7);

  g.addNodeCosts(1, 1.1);
  g.addNodeCosts(2, 2.1);
  g.addNodeCosts(3, 3.1);
  g.addNodeCosts(4, 4.1);
  g.addNodeCosts(5, 5.1);
  g.addNodeCosts(6, 6.1);
  g.addNodeCosts(7, 7.1);
  g.addNodeCosts(8, 8.1);

  g.print();

  foreach(g.Node n; g)
  {
    Stdout.format("{} {}", n.id, n.costs).newline;
  }

  logger.info("Done");
}
