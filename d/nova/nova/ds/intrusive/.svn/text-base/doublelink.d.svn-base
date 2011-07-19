//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.doublelink;

struct ParentLink(ChildType)
{
  ChildType  child;
}

struct NodeLink(ParentType)
{
  ParentType parent;
}

//////////////////////////////////////////////////////////////////////////
/**

  A doublelink is a two way relation between a parent and a child

  $(IMG image/doublelink.png)

  Usage patterns:

  1. Introduce link(s) in child class
  ---
  class TwoListNode
  {
    ...
    public :
      NodeLink!(TwoLinkedNode) link1;
      NodeLink!(TwoLinkedNode) link2;
    ...
  }
  ---

  2. Introduce link(s) into parent class
  ---
  class ParentOne
  {
    public :
      ParentLink!(TwoLinkedNode) parent;
  }

  class ParentTwo
  {
    public :
      ParentLink!(TwoLinkedNode) parent;
  }
  ---

  3. Define accessor class(es)
  ---
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
  ---

  5. Instantiate manager class(es)
  ---
  alias DoubleLink!(ParentOne, TwoLinkedNode, AccessorOne) DoubleLink1;
  alias DoubleLink!(ParentTwo, TwoLinkedNode, AccessorTwo) DoubleLink2;

  DoubleLink1 link1;
  DoubleLink2 link2;
  ---

  4. Create parent instances
  ---
  int main()
  {
    ParentOne  par1 = new ParentOne;
    ParentTwo  par2 = new ParentTwo;
  ---

  5. Connect child via DoubleLink
  ---
    TwoLinkedNode child = new TwoLinkedNode(0);
    link1.connect(par1, child);
    link2.connect(par2, child);
  ---

  5. Disonnect child via DoubleLink
  ---
    DoubleLink1.disconnect(child);
    DoubleLink1.disconnect(child);
  ---

*/////////////////////////////////////////////////////////////////////////
class DoubleLink(ParentType, ChildType, alias Accessor)
{
  private:
    // parent access
    static ChildType  getchild(ParentType cur)                { return Accessor.getparent(cur).child; }
    static void       setchild(ParentType cur, ChildType n)   { Accessor.getparent(cur).child = n;    }

    // child access
    static ParentType getparent(ChildType cur)                { return Accessor.getchild(cur).parent; }
    static void       setparent(ChildType cur, ParentType p)  { Accessor.getchild(cur).parent = p;    }

  public:
    //////////////////////////////////////////////////////////////////////////
    /**

      connects a parent and a child child

      Params: p = reference parent child
              c = reference child child

    */////////////////////////////////////////////////////////////////////////
    static void connect   (ParentType p, ChildType c)
    in
    {
      // check parameters
      assert(p);
      assert(c);

      // check if DoubleLinkChild already connected
      assert(null is getparent(c));

      // check if DoubleLinkParent already connected
      assert(null is getchild(p));
    }
    body
    {
      setchild  (p, c);
      setparent(c, p);
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      disconnects a doublelink

      Params: p = reference to parent

    */////////////////////////////////////////////////////////////////////////
    static void disconnect(ParentType p)
    in
    {
      // check parameters
      assert(p);
    }
    body
    {
      if (getchild(p))
      {
        setparent(getchild(p), null);
        setchild (p,           null);
      }
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      disconnects a doublelink

      Params: c = reference to child

    */////////////////////////////////////////////////////////////////////////
    static void disconnect(ChildType c)
    in
    {
      // check parameters
      assert(c);
    }
    body
    {
      if (getparent(c))
      {
        setchild(getparent(c), null);
        setparent(c,           null);
      }
    }
}
