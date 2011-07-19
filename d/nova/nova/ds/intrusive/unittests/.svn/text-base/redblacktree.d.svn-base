//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.unittests.redblacktree;

private import nova.ds.intrusive.redblacktree;

private import tango.util.log.Log;

private Logger logger;
static this()
{
  logger = Log.getLogger("unittests.redblacktree");
}

class TreeNode
{

  public:
    int id;

  public:
    ChildLink!(TreeNode) link;   // link for rbtree

    this(int _id)
    {
      id    = _id;
    }

    ~this()
    {
    }

    char[] toString()
    {
      return tango.text.convert.Integer.toUtf8(id);
    }
}

// parent node for list 2
class TreeParent
{
  public:
    ParentLink!(TreeNode) parent;

}

// Define accessor class.
static class TreeParentAccessor
{
  public:
    alias TreeParent ParentType;
    alias TreeNode   ChildType;
    alias int        KeyType;

  public:
    /// parent access
    static ParentLink!(TreeNode)* getparent (TreeParent p)
    {
      return &p.parent;
    }

    /// child access
    static ChildLink!(TreeNode)*  getchild  (TreeNode c)
    {
      return &c.link;
    };

    // key/node comparison
    static int compareKey (TreeNode cur, in int key)
    {
      return cur.id - key;
    }

    static int compareNode(TreeNode lhs, TreeNode rhs)
    {
      return lhs.id - rhs.id;
    }
}


// aliases for specific manager classes
alias RedBlackTree!(TreeParentAccessor) Tree;

// instantiate the specific manager classes
Tree tree;

// create nodes and connect to the two lists
void createNodes(TreeParent p, int num)
{
  logger.info("Creating nodes");

  for (int idx = 0; idx < num; ++idx)
  {
    TreeNode a = new TreeNode(idx);
    tree.insert(p, a);
  }
}

// test traversal
void testTraverse()
{
  auto TreeParent  p = new TreeParent;

  createNodes(p, 10);

  tree.PrintSideWays(p);
}

unittest
{
  logger.info("Start unittest");

  testTraverse();

  logger.info("Done");
}
