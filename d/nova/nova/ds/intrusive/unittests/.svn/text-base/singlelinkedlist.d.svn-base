//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.unittests.singlelinkedlist;

private import nova.ds.intrusive.singlelinkedlist;

private import tango.util.log.Log;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.singlelinkedlist");
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

// parent node for list 1
class ListParentOne
{
  public:
    ParentLink!(TwoListNode) parent;
}

// Define accessor class.
static class ListParentOneAccessor
{
  public:
    alias ListParentOne ParentType;
    alias TwoListNode   ChildType;

  public:
    static ParentLink!(TwoListNode)* getparent(ListParentOne p)
    {
      return &p.parent;
    };

    static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
    {
      return &c.link1;
    };
}

// parent node for list 2
class ListParentTwo
{
  public:
    ParentLink!(TwoListNode) parent;
}

// Define accessor class.
static class ListParentTwoAccessor
{
  public:
    alias ListParentTwo ParentType;
    alias TwoListNode   ChildType;

  public:
    static ParentLink!(TwoListNode)* getparent(ListParentTwo p)
    {
      return &p.parent;
    };

    static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
    {
      return &c.link2;
    };
}

// instantiate the specific manager classes
SingleLinkedList!(ListParentOneAccessor) list1;
SingleLinkedList!(ListParentTwoAccessor) list2;

// create nodes and connect to the two lists
void createNodes(ListParentOne par1, ListParentTwo par2, int num)
{
  logger.info("Creating nodes");

  for (int idx = 0; idx < num; ++idx)
  {
    TwoListNode a = new TwoListNode(idx);
    list1.addTail(par1, a);
    list2.addHead(par2, a);
  }
}

// test traversal
void testTraverse()
{
  auto ListParentOne  par1 = new ListParentOne;
  auto ListParentTwo  par2 = new ListParentTwo;

  int numNodes = 5;

  createNodes(par1, par2, numNodes);

  logger.info("Traverse with foreach");
  {
    int cmpid = 0;
    list1.Iterator it = new list1.Iterator(par1);
    foreach(TwoListNode trav; it)
    {
      assert(cmpid == trav.id);
      cmpid++;
    }
  }

  logger.info("Traverse with opPostInc");
  {
    int cmpid = numNodes - 1;
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.begin(par2);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid--;
      trav = it++;
    }
  }

  logger.info("Remove with foreach");
  {
    int cmpid = 0;
    list1.Iterator it = new list1.Iterator(par1);
    foreach(TwoListNode trav; it)
    {
      assert(cmpid == trav.id);
      cmpid++;
      list1.remove(par1, trav);
    }
  }

  logger.info("Remove with opPostInc");
  {
    int cmpid = numNodes - 1;
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.begin(par2);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid--;
      list2.remove(par2, trav);
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

