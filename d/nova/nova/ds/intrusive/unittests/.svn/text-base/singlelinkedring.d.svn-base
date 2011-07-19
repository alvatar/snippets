//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.unittests.singlelinkedring;

private import nova.ds.intrusive.singlelinkedring;

private import tango.util.log.Log;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.singlelinkedring");
}

// define a node that could be part of two different lists
class TwoListNode
{
  public:
    int id;

  public:
    ChildLink!(TwoListNode) link1;   // link for list 1
    ChildLink!(TwoListNode) link2;   // link for list 2

    this(int _id)
    {
      id    = _id;
    }

    ~this()
    {
    }
}

// Define accessor class.
static class RingAccessor1
{
  public:
    alias TwoListNode   ChildType;

  public:
    static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
    {
      return &c.link1;
    };
}

// Define accessor class.
static class RingAccessor2
{
  public:
    alias TwoListNode   ChildType;

  public:
    static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
    {
      return &c.link2;
    };
}

// instantiate the specific manager classes
SingleLinkedRing!(RingAccessor1) ring1;
SingleLinkedRing!(RingAccessor2) ring2;

// create nodes and connect to the two lists
TwoListNode createNodes(int num)
{
  debug(singlelinkedring) Stdout("Creating nodes").newline;

  TwoListNode tail;
  tail = new TwoListNode(0);
  ring1.addHead(null, tail);
  ring2.addHead(null, tail);
  for (int idx = 1; idx < num; ++idx)
  {
    TwoListNode a = new TwoListNode(idx);

    ring1.addHead(tail, a);
    ring2.addHead(tail, a);
    tail = a;
  }
  return tail;
}

// test traversal
void testTraverse()
{
  int numNodes = 5;

  TwoListNode tail = createNodes(numNodes);

  // auto sprint = new Sprint!(char);
  // logger.info( sprint("  node: {}", trav.id) );

  logger.info("Traverse with foreach");
  {
    int cmpid = 0;
    ring1.Iterator it = new ring1.Iterator(tail);
    foreach(TwoListNode trav; it)
    {
      assert(cmpid == trav.id);
      cmpid++;
    }
  }

  logger.info("Traverse with opPostInc");
  {
    int cmpid = 0;
    ring2.Iterator it = new ring2.Iterator();
    TwoListNode trav = it.begin(tail);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid++;
      trav = it++;
    }
  }

  logger.info("Remove all nodes");
  {
    int cmpid = numNodes - 1;
    TwoListNode trav = tail;
    while (null !is trav)
    {
      assert(cmpid == trav.id);
      cmpid--;
      trav = ring1.remove(trav, trav);
    }
  }

  logger.info("Remove with opPostInc");
  {
    int cmpid = 0;
    ring2.Iterator it = new ring2.Iterator();
    TwoListNode trav = it.begin(tail);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid++;
      ring2.remove(tail, trav);
      trav = it++;
    }
  }
}


unittest
{
  logger.info("Start unittest");

  testTraverse();

  logger.info("Done");
}

