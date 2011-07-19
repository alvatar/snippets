//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.unittests.doublelinkedlist;

import nova.ds.intrusive.doublelinkedlist;

private import tango.util.log.Log;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.doublelinkedlist");
}

class TwoListNode
{
  public:
    int id;

    ChildLink!(TwoListNode) link1;
    ChildLink!(TwoListNode) link2;

    this(int _id)
    {
      id    = _id;
    }

    ~this()
    {
    }
}

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

    static ParentLink!(TwoListNode)* getparent(ListParentOne p)
    {
      return &p.parent;
    };

    static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
    {
      return &c.link1;
    };
}

class ListParentTwo
{
  public:
    ParentLink!(TwoListNode) parent;

}

// Define accessor class.
static class ListParentTwoAccessor
{
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


alias DoubleLinkedList!(ListParentOneAccessor) List1;
alias DoubleLinkedList!(ListParentTwoAccessor) List2;

List1 list1;
List2 list2;

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

  logger.info("Traverse with opPostDec");
  {
    int cmpid = 0;
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.end(par2);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid++;
      trav = it--;
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

  logger.info("Remove with opPostDec");
  {
    int cmpid = 0;
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.end(par2);
    while (trav)
    {
      assert(cmpid == trav.id);
      cmpid++;
      trav = it--;
    }
  }
}

unittest
{
  logger.info("Start unittest");

  testTraverse();

  logger.info("Done");
}


