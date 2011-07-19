//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.unittests.doublelink;

private import nova.ds.intrusive.doublelink;

private import tango.util.log.Log;
private import tango.text.convert.Sprint;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.doublelinkedlist");
}

class TwoLinkedNode
{
  public:
    int id;

    NodeLink!(ParentOne) link1;
    NodeLink!(ParentTwo) link2;

    this(int _id)
    {
      id    = _id;
    }

    ~this()
    {
    }
}

class ParentOne
{
  public:
    ParentLink!(TwoLinkedNode) parent;
}

// Define accessor class.
class AccessorOne
{
  static ParentLink!(TwoLinkedNode)* getparent(ParentOne p)
  {
    return &p.parent;
  };

  static NodeLink!(ParentOne)*  getchild(TwoLinkedNode c)
  {
    return &c.link1;
  };
}

class ParentTwo
{
  public:
    ParentLink!(TwoLinkedNode) parent;
}

// Define accessor class.
class AccessorTwo
{
  static ParentLink!(TwoLinkedNode)* getparent(ParentTwo p)
  {
    return &p.parent;
  };

  static NodeLink!(ParentTwo)*  getchild(TwoLinkedNode c)
  {
    return &c.link2;
  };
}

alias DoubleLink!(ParentOne, TwoLinkedNode, AccessorOne) DoubleLink1;
alias DoubleLink!(ParentTwo, TwoLinkedNode, AccessorTwo) DoubleLink2;

DoubleLink1 link1;
DoubleLink2 link2;

unittest
{
  logger.info("Start unittest");

  ParentOne  par1 = new ParentOne;
  ParentTwo  par2 = new ParentTwo;

  TwoLinkedNode child = new TwoLinkedNode(0);
  link1.connect(par1, child);
  link2.connect(par2, child);

  DoubleLink1.disconnect(child);
  DoubleLink2.disconnect(child);

  logger.info("Done");
}
