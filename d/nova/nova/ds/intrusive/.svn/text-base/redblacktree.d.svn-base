//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.redblacktree;

import tango.io.Stdout;

/// Link element introduced into the parent
struct ParentLink(ChildType)
{
  ChildType root;
}

enum NodeColor
{
  RedNode   = 0,
  BlackNode = 1
};

/// Link element introduced into the child
struct ChildLink(ChildType)
{
  ChildType left  = null;
  ChildType right = null;
  NodeColor color = NodeColor.RedNode;
}

//////////////////////////////////////////////////////////////////////////
/**

  An intrusive red/black tree implementation.

  $(IMG image/redblacktree.png)

  Usage patterns:

  1. Introduce link into child node class
  ---
  class TreeNode
  {
    ...
    public :
      ChildLink!(TreeNode) link;   // link for rbtree
  ---

  2. Introduce link into parent node class
  ---
  class TreeParent
  {
    public :
      ParentLink!(TreeNode) parent;
  ---

  3. Define accessor class (a static class is sufficient)
  ---
  static class TreeParentAccessor
  {
    public :
      alias TreeParent ParentType;
      alias TreeNode   ChildType;
      alias int        KeyType;

    public :
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
  ---

  4. Now we are ready to use it:
  ---
  // aliases for specific manager classes
  alias RedBlackTree!(TreeParentAccessor) Tree;

  // instantiate the specific manager classes
  Tree tree;

  int main()
  {
    auto TreeParent  p = new TreeParent;

    // create nodes and connect to the two lists
    for (int idx = 0; idx < num; ++idx)
    {
      TreeNode a = new TreeNode(idx);
      tree.insert(p, a);
    }

    tree.PrintSideWays(p);
  }
  ---

*/////////////////////////////////////////////////////////////////////////
class RedBlackTree(alias Accessor)
{
  private:
    alias Accessor.ParentType ParentType;
    alias Accessor.ChildType  ChildType;
    alias Accessor.KeyType    KeyType;

    /// child access
    static ChildType getleft (ChildType cur)                    { return Accessor.getchild(cur).left;     }
    static void      setleft (ChildType cur, ChildType n)       { Accessor.getchild(cur).left = n;        }
    static ChildType getright(ChildType cur)                    { return Accessor.getchild(cur).right;    }
    static void      setright(ChildType cur, ChildType n)       { Accessor.getchild(cur).right = n;       }
    static NodeColor getcolor(ChildType cur)                    { return Accessor.getchild(cur).color;    }
    static void      setcolor(ChildType cur, NodeColor color)   { Accessor.getchild(cur).color = color;   }

    /// parent access
    static ChildType getroot(ParentType cur)                    { return Accessor.getparent(cur).root;    }
    static void      setroot(ParentType cur, ChildType n)       { Accessor.getparent(cur).root = n;       }

    /// key  comparison
    static int       compareKey (ChildType cur, in KeyType key) { return Accessor.compareKey(cur, key);   }
    /// node comparison
    static int       compareNode(ChildType lhs, ChildType rhs)  { return Accessor.compareNode(lhs, rhs);  }


    // The PtrPack struct is used to to save function-calling overhead
    struct PtrPack
    {
      ChildType t;  // The current node
      ChildType p;  // Parent
      ChildType g;  // Grandparent
      ChildType gg; // Great Grandparent
      ChildType s;  // Sibling
      ChildType rs; // Right children of s
      ChildType ls; // Left children of s
      ChildType m;  // Matching node
      ChildType pm; // Parent of matching node
    };

    // Helper functions
    static ChildType InsBalance   (ChildType Root, PtrPack pp)
    {
      ChildType cofgg;                  // New child of great-grandparent
      int side;

      side = pp.gg && getright(pp.gg) is pp.g;

      if (getleft(pp.g) is pp.p)
      {
         if (getright(pp.p) is pp.t)
         {                              // Do double rotate
            setleft(pp.g,  getright(pp.t));
            setright(pp.t, pp.g);
            setright(pp.p, getleft(pp.t));
            setleft(pp.t,  pp.p);
            pp.p         = pp.gg;
            cofgg        = pp.t;
         }
         else
         {                              // Do single rotate right
            setleft(pp.g,  getright(pp.p));
            setright(pp.p, pp.g);
            cofgg        = pp.p;
         }
      }
      else
      {                                 // getright(pp.g) is pp.p
        if (getleft(pp.p) is pp.t)
        {                               // Do double rotate
           setright(pp.g, getleft(pp.t));
           setleft(pp.t,  pp.g);
           setleft(pp.p,  getright(pp.t));
           setright(pp.t, pp.p);
           pp.p         = pp.gg;
           cofgg        = pp.t;
        }
        else
        {                               // Do single rotate left
           setright(pp.g,  getleft(pp.p));
           setleft(pp.p,   pp.g);
           cofgg         = pp.p;
        }
      }

      setcolor(cofgg, NodeColor.BlackNode);
      setcolor(pp.g,  NodeColor.RedNode);

      if (pp.gg)
      {
         if (side)
           setright(pp.gg, cofgg);
         else
           setleft(pp.gg, cofgg);
      }
      else Root = cofgg;

      return Root;

    }

    static ChildType DoReplacement(ChildType Root, PtrPack pp)
    {
      // At this point, pp.m is the node to delete, pp.pm is it's parent,
      // pp.p is the replacement node, pp.g is it's parent. If pp.m has no
      // successor, pp.p will = pp.m, and replacement is the non-null
      // child of pp.m.

      if (pp.m)
      {                       // Matching node was found
        if (pp.p           is pp.m ||
            getleft(pp.m)  is null ||
            getright(pp.m) is null)
        {
           // No successor, and/or at least one child null
           // Get non-null child, if any, else pp.p will be null
           pp.p = (getleft(pp.m) ? getleft(pp.m) : getright(pp.m));
        }
        else
        {
          // pp.m has a successor to use as a replacement
          if (pp.g !is pp.m)
          {
            // Successor is not immediate child of pp.m, so detach
            // from where it is, attach to where pp.m is
            setleft(pp.g,  getright(pp.p));
            setright(pp.p, getright(pp.m));
          }
          // Finish attaching where pp.m is.
          setleft(pp.p, getleft(pp.m));
        }
        // pp.p should have same color as pp.m since it's going where pp.m was
        if (pp.p)
          setcolor(pp.p, getcolor(pp.m));
      }

      // Fixup pp.pm child link to be pp.p
      if (pp.m)
      {
        if (pp.pm)
        {
          if (getleft(pp.pm) is pp.m)
            setleft(pp.pm, pp.p);
          else
            setright(pp.pm, pp.p);
        }
        else
          Root = pp.p; // New Root, possibly null
      }
      return Root;
    }

    static ChildType DelBalance   (ChildType Root, PtrPack pp)
    {
      if ((pp.t           is  null                 ||
           getcolor(pp.t) ==  NodeColor.BlackNode) &&
           pp.s           !is null                 &&
           getcolor(pp.s) ==  NodeColor.RedNode)
      {
        // Case: pp.t is black, pp.s red. pp.t might be null,
        // but pp.s and pp.p must not be.

        // Fix pp.g child to be pp.s. Also pp.s may become pp.p of pp.m
        if (pp.g)
        {
          if (getleft(pp.g) is pp.p)
            setleft(pp.g, pp.s);
          else
            setright(pp.g, pp.s);
        }
        else
          Root = pp.s;

        if (pp.p is pp.m)
          pp.pm = pp.s;

        // Finish rotating
        if (getleft(pp.p) is pp.t)
        {
          // RotateLeft(pp.p);
          setright(pp.p, pp.ls);
          setleft(pp.s, pp.p);
          pp.g = pp.s;
          pp.s = pp.ls;
        }
        else
        {
          // RotateRight(pp.p);
          setleft(pp.p, pp.rs);
          setright(pp.s, pp.p);
          pp.g = pp.s;
          pp.s = pp.rs;
        }

        // Fixup children of pp.s
        if (pp.s)
        {
          pp.ls = getleft(pp.s);
          pp.rs = getright(pp.s);
        }

        // Fixup colors
        setcolor(pp.p, NodeColor.RedNode);
        setcolor(pp.g, NodeColor.BlackNode);
      }

      if (pp.t                                                &&
          getcolor(pp.t)            == NodeColor.BlackNode    &&
          (getleft(pp.t)            is null              ||
           getcolor(getleft(pp.t))  == NodeColor.BlackNode)   &&
          (getright(pp.t)           is null              ||
           getcolor(getright(pp.t)) == NodeColor.BlackNode))
      {
        // Case: pp.t is a single binary node with two black children.
        if (pp.s                                              &&
            getcolor(pp.s)            == NodeColor.BlackNode  &&
            (getleft(pp.s)            is null              ||
             getcolor(getleft(pp.s))  == NodeColor.BlackNode) &&
            (getright(pp.s)           is null              ||
             getcolor(getright(pp.s)) == NodeColor.BlackNode))
        {

         // Case: pp.t and pp.s are single binary nodes with two black children..
         setcolor(pp.p, NodeColor.BlackNode);
         setcolor(pp.t, NodeColor.RedNode);
         setcolor(pp.s, NodeColor.RedNode);

        }
        else if (pp.ls && getcolor(pp.ls) == NodeColor.RedNode)
        {

           // Case: Tree is a single binary node with two black children.
           // pp.s is pair of binary nodes-one red, one black or pp.s is a set
           // of three binary nodes with a black parent and red child nodes.
           // pp.ls is red or pp.rs might be red.

          if (getleft(pp.p) is pp.t)
          {
            // Fix colors
            setcolor(pp.ls, getcolor(pp.p));
            setcolor(pp.p,  NodeColor.BlackNode);
            setcolor(pp.t,  NodeColor.RedNode);
            // Fix pp.g child to be pp.ls. ALSo pp.ls may become pp.p of pp.m
            if (pp.g)
            {
              if (getleft(pp.g) is pp.p)
                setleft(pp.g, pp.ls);
              else
                setright(pp.g, pp.ls);
            }
            else Root = pp.ls;
            if (pp.p is pp.m) pp.pm = pp.ls;

            // Finish: DoubleRotateLeft(pp.s, pp.p);
            setright(pp.p,  getleft(pp.ls));
            setleft(pp.ls,  pp.p);
            setleft(pp.s,   getright(pp.ls));
            setright(pp.ls, pp.s);
            pp.g          = pp.ls;
            // Do not fix pp.s or pp.ls since they get reassigned
            // at the top of next loop
          }
          else
          {
            // Fix colors
            setcolor(pp.s,  getcolor(pp.p));
            setcolor(pp.ls, NodeColor.BlackNode);
            setcolor(pp.p,  NodeColor.BlackNode);
            setcolor(pp.t,  NodeColor.RedNode);
            // Fix pp.g child to be pp.s. Also pp.s may become pp.p of pp.m
            if (pp.g)
            {
              if (getleft(pp.g) is pp.p)
                setleft(pp.g, pp.s);
              else
                setright(pp.g, pp.s);
            }
            else Root = pp.s;
            if (pp.p is pp.m) pp.pm = pp.s;
            // Finish: RotateRight(pp.p);
            setleft(pp.p,  pp.rs);
            setright(pp.s, pp.p);
            pp.g         = pp.s;
            // Do not fix pp.s or pp.ls since they get reassigned
            // at the top of next loop
          }
        }
        else if (pp.rs && getcolor(pp.rs) is NodeColor.RedNode)
        {
          // Case: pp.t is a single binary node with two black children.
          // pp.s is a pair of binary nodes-one red, one black.
          // pp.ls is black, pp.rs is red.

          if (getleft(pp.p) is pp.t)
          {
            // Fix colors
            setcolor(pp.rs, NodeColor.BlackNode);
            setcolor(pp.s,  getcolor(pp.p));
            setcolor(pp.p,  NodeColor.BlackNode);
            setcolor(pp.t,  NodeColor.RedNode);
            // Fix pp.g child to be pp.s. Also, pp.s may become pp.p of pp.m
            if (pp.g)
            {
              if (getleft(pp.g) is pp.p)
                setleft(pp.g, pp.s);
              else
                setright(pp.g, pp.s);
            }
            else Root = pp.s;
            if (pp.p is pp.m) pp.pm = pp.s;
            // Finish: RotateLeft(pp.p);
            setright(pp.p, pp.ls);
            setleft(pp.s,  pp.p);
            pp.g         = pp.s;
            // Do not fix pp.s or pp.ls since they get reassigned
            // at the top of next loop
          }
          else
          {
            // Fix colors
            setcolor(pp.rs, getcolor(pp.p));
            setcolor(pp.p , NodeColor.BlackNode);
            setcolor(pp.t , NodeColor.RedNode);
            // Fix pp.g child to become pp.rs. ALSo, pp.rs may become pp.p of pp.m.
            if (pp.g)
            {
              if (getleft(pp.g) is pp.p)
                setleft(pp.g, pp.rs);
              else
                setright(pp.g, pp.rs);
            }
            else Root = pp.rs;
            if (pp.p is pp.m) pp.pm = pp.rs;
            // Finish: DoubleRotateRight(pp.s, pp.p);
            setleft(pp.p,   getright(pp.rs));
            setright(pp.rs, pp.p);
            setright(pp.s,  getleft(pp.rs));
            setleft(pp.rs,  pp.s);
            pp.g          = pp.rs;
            // Do not fix pp.s or pp.ls since they get reassigned
            // at the top of next loop
          }
        }
      }
      return Root;
    }

    /// returns height of tree at node root
    static int       height(in ChildType root)
    {
      int h = 0;
      if (root !is null)
      {
        int lh = height(getleft (root));
        int rh = height(getright(root));
        h = ((lh > rh) ? lh : rh) + 1;
      }
      return h;
    }

    /// returns number of nodes within tree
    static int       count (ChildType root)
    {
      int n = 0;
      if (root !is null)
      {
        ++n;
        n += count(getleft (root));
        n += count(getright(root));
      }
      return n;
    }

    /// get child node with "minimum" value
    static ChildType getMin (in ChildType root)
    {
      if (root)
      {
        while(getright(root)) root = getright(root);
      }
      return root;
    }

    /// get child node with "maximum" value
    static ChildType getMax (in ChildType root)
    {
      if (root)
      {
        while(getleft(root)) root = getleft(root);
      }
      return root;
    }

  public:
    /// insert child node into tree
    static int       insert    (ParentType parent, ChildType node)
    in
    {
      assert(parent);
      assert(node);
    }
    body
    {
      int bInserted = 0;

      if (getleft(node) || getright(node))
      {
        // insert() not executed, object already in a rbtree
        assert(false);
        // return bInserted;
      }
      else
      {
        PtrPack pp;
        int side;

        pp.t = getroot(parent); pp.p = null; pp.g = null; pp.gg = null;

        while (pp.t && (compareNode(node, pp.t) != 0))
        {
          // If on a set of three binary nodes with a black
          // parent node and red child nodes, split it into
          // two single binary nodes with two black children.
          if ((getleft(pp.t)  && getcolor(getleft(pp.t))  == NodeColor.RedNode) &&
              (getright(pp.t) && getcolor(getright(pp.t)) == NodeColor.RedNode))
          {
            setcolor(pp.t,           NodeColor.RedNode);
            setcolor(getleft(pp.t),  NodeColor.BlackNode);
            setcolor(getright(pp.t), NodeColor.BlackNode);

            // Check for two reds in a row, and adjust if so
            if (pp.p && getcolor(pp.p) == NodeColor.RedNode)
            {
              setroot(parent, InsBalance(getroot(parent), pp));
            }
          }
          pp.gg = pp.g; pp.g = pp.p; pp.p = pp.t;

          // go to left if node < current node
          if (compareNode(pp.t, node) < 0)
          {
            pp.t  = getleft(pp.t);
            side = 0;
          }
          else
          {
            pp.t  = getright(pp.t);
            side = 1;
          }
        }

        // Reached the bottom, with no matching node
        if (pp.t is null)
        {
          bInserted = 1;

          // now insert node
          //
          pp.t = node;
          setcolor(pp.t, NodeColor.RedNode);

          if (pp.t is null) return 0; // Couldn't add
          if (pp.p)
          {
            if (side)
            {
              setright(pp.p, pp.t);
            }
            else
            {
              setleft(pp.p, pp.t);
            }
          }
          else
            setroot(parent, pp.t);

          // Check for two reds in a row, and adjust if so
          if (pp.p && getcolor(pp.p) == NodeColor.RedNode)
          {
            setroot(parent, InsBalance(getroot(parent), pp));
          }
        }

        // Root always made black
        setcolor(getroot(parent), NodeColor.BlackNode);

        return bInserted;
      }
    }

    /// remove child node which matches key from the tree
    static ChildType remove    (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      PtrPack pp;

      if (!(parent && getroot(parent))) return null;

      pp.t = getroot(parent); pp.p = null; pp.g = null;
      pp.m = null; pp.pm = null;

      while (pp.t)
      {
        // Go down the tree one level, searching for node to delete
        if (compareKey(pp.t, compare) == 0)
        {
          pp.m  = pp.t;   // Record matching node
          pp.pm = pp.p;   // And it's parent
        }

        // Update ancestor pointers
        pp.g = pp.p; pp.p = pp.t;

        if (compareKey(pp.t, compare) < 0)
        {
          pp.t  = getleft (pp.p);
          pp.s = getright(pp.p);
        }
        else
        {
          // Walk down even if it matches, to look for successor
          pp.t  = getright(pp.p);
          pp.s = getleft (pp.p);
        }

        if (pp.s)
        {
          pp.ls = getleft(pp.s);
          pp.rs = getright(pp.s);
        }
        else
        {
          pp.ls = null;
          pp.rs = null;
        }
        setroot(parent, DelBalance(getroot(parent), pp));
      }

      setroot(parent, DoReplacement(getroot(parent), pp));
      if (getroot(parent))
        setcolor(getroot(parent), NodeColor.BlackNode);

      // clear connection to tree
      if (pp.m)
      {
        setleft(pp.m,  null);
        setright(pp.m, null);
      }

      return pp.m; // Node to delete
    }

    /// find child node which matches key
    static ChildType find      (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType t = getroot(parent);
      while (t)
      {
        if (compareKey(t, compare) == 0) break;
        t = ((compareKey(t, compare) < 0) ? getleft(t) : getright(t));
      }
      return t;
    }

    /// find child node which is "above" given key
    static ChildType findUpper (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType t   = getroot(parent);
      ChildType erg = null;
      while (t)
      {
        if (compareKey(t, compare) > 0)
        {
          // if (t > key) -> remember it
          // and descend right subtree
          erg = t;
          t = getright(t);
        }
        else
        {
          // if (t < key) descend left subtree
          t = getleft(t);
        }
      }
      return erg;
    }

    /// find child node which is "below" given key
    static ChildType findLower (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType t   = getroot(parent);
      ChildType erg = null;
      while (t)
      {
        if (compareKey(t, compare) < 0)
        {
          // if (t < key) -> remember it
          // and descend left subtree
          erg = t;
          t = getleft(t);
        }
        else
        {
          // if (t >= key) descend right subtree
          t = getright(t);
        }
      }
      return erg;
    }

    /// find child node which is above or equal to the given key
    static ChildType findUpperOrEq (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType t   = getroot(parent);
      ChildType erg = null;
      while (t)
      {
        if (compareKey(t, compare) > 0)
        {
          // if t > key descend right subtree
          t = getright(t);
        }
        else
        {
          // if (t <= key) -> remember it
          // and descend left subtree
          erg = t;
          t   = getleft(t);
        }
      }
      return erg;
    }

    /// find child node which is below or equal to the given key
    static ChildType findLowerOrEq (ParentType parent, in KeyType compare)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType t = getroot(parent);
      ChildType erg = null;
      while (t)
      {
        if (compareKey(t, compare) < 0)
        {
          // if (t < key) descend left subtree
          t = getleft(t);
        }
        else
        {
          // if (t >= key) -> remember it
          // and descend right subtree
          erg = t;
          t = getright(t);
        }
      }
      return erg;
    }

    /// clear tree
    static void      clear     (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      setroot(parent, null);
    }

    /// returns 1 when tree is empty
    static int       isEmpty   (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      return (getroot(parent) is null);
    }

    /// determine height of tree
    static int       height    (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      ChildType root = getroot(parent);
      int result = 0;
      if (root !is null)
      {
        result = height(root);
      }
      return result;
    }

    /// return number of nodes maintained by tree
    static int       count     (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      return count(getroot(parent));
    }

    /// return node with minimum key
    static ChildType getMin    (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      return getMin(getroot(parent));
    }

    /// return node with maximum key
    static ChildType getMax    (ParentType parent)
    in
    {
      assert(parent);
    }
    body
    {
      return getMax(getroot(parent));
    }

    /// debug output; prints tree sideways
    static void PrintSideWays  (ParentType parent)
    {
      _PrintSideWays(getroot(parent), 0);
    }

    static void _PrintSideWays (in ChildType child, in int space)
    {
      while(child)
      {
        space += 2;
        _PrintSideWays(getleft(child), space);
        // printf("%*.*s\n", space, child.toString);
        for (int idx = 0; idx < space; idx++)
        {
          Stdout(" ");
        }
        Stdout(child.toString).newline;
        child = getright(child);
      }
    }

}
