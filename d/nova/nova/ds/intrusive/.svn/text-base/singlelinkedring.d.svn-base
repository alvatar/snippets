//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.singlelinkedring;

/// Link element introduced into the child node
struct ChildLink(ChildType)
{
  ChildType next = null;
}

//////////////////////////////////////////////////////////////////////////
/**

  An intrusive single linked ring connects nodes via a single next
  pointer. The linked nodes forms a ring.

  $(IMG image/singlelinkedring.png)

  Usage patterns:

  1. Introduce link(s) in node class
  ---
  class TwoListNode
  {
    ...
    public :
      ChildLink!(TwoListNode) link1;
      ChildLink!(TwoListNode) link2;
    ...
  }
  ---

  2. Define accessor classes
  ---
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
  ---

  2. Define accessor classes
  ---
  // instantiate the specific manager classes
  SingleLinkedRing!(RingAccessor1) ring1;
  SingleLinkedRing!(RingAccessor2) ring2;
  ---

*/////////////////////////////////////////////////////////////////////////
class SingleLinkedRing(alias Accessor)
{
  private:
    /// type traits
    alias Accessor.ChildType  ChildType;

    /// node access
    static ChildType getnext(ChildType cur)               { return Accessor.getnode(cur).next;   }
    static void      setnext(ChildType cur, ChildType n)  { Accessor.getnode(cur).next = n;      }

  public:
    /// add child to head of ring
    static ChildType addHead(ChildType tail, ChildType c)
    in
    {
      assert(c);
    }
    body
    {
      // check if c is not ring
      assert(null is getnext(c));

      if(null !is tail)
      {
        setnext(c, getnext(tail));
        setnext(tail, c);
        return tail;
      }
      else
      {
        // form initial ring
        setnext(c, c);
        return c;
      }
    }

    /// add child to tail of the ring
    static ChildType addTail(ChildType tail, ChildType c)
    in
    {
      assert(c);
    }
    body
    {
      // check if c is not ring
      assert(null is getnext(c));

      addHead(tail, c);
      return c;
    }

    /// append child 'c' to ring 'tail' after child 'after'
    static ChildType append(ChildType tail, ChildType after, ChildType c)
    in
    {
      assert(tail);
      assert(after);
      assert(c);
    }
    body
    {
      // check if after is in a ring
      assert(null !is getnext(after));

      // check if c is not in a ring
      assert(null is getnext(c));

      setnext(c, getnext(after));
      setnext(after, c);
      if (tail is after)
      {
        tail = c;
      }
      return tail;
    }

    /// remove a child c from ring t
    static ChildType remove(ChildType tail, ChildType c)
    in
    {
      assert(tail);
      assert(c);
    }
    body
    {
      // check if c is in a ring
      assert(null !is getnext(c));

      ChildType trav, next;
      for ( trav = tail, next = null;
            trav !is tail || null is next;
            trav = next)
      {
        next = getnext(trav);

        if (next is c)
          break;
      }

      if (null is trav || getnext(trav) !is c )
      {
        // c not in ring
        assert(false);
      }

      if (c is trav)
      {
        tail = null;
      }
      else if (c is tail)
      {
        tail = trav;
      }
      setnext(trav, getnext(c));
      setnext(c, null);
      return tail;
    }

    /// count number of childs maintained by parent p
    static int      count  (ChildType tail)
    {
      Iterator it = new Iterator(tail);

      int cnt = 0;
      foreach(ChildType trav; it)
      {
        ++cnt;
      }
      return (cnt);
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Iterator class, supports forward traversal over single linked ring

    */////////////////////////////////////////////////////////////////////////
    static class Iterator
    {
      private:
        /// Node access
        static ChildType getnext(ChildType cur)   { return Accessor.getnode(cur).next;   }

      public:
        /// support for foreach statements
        int opApply(int delegate(inout ChildType) dg)
        {
          int res = 0;

          ChildType curnode = tail;
          while (curnode)
          {
            // remember reference to next node
            // before calling delegate:
            //  current node could be removed from list
            ChildType next = getnext(curnode);
            res = dg(next);

            if (tail == next)
            {
              next = null;
            }
            curnode = next;

            if (res) break;
          }
          return res;
        }

        /// support for ++ operator
        ChildType opPostInc()
        {
          if (null is tail)
            return null;

          ChildType result = forward;

          if (null !is forward)
          {
            if (forward is tail)
            {
              forward = null;
            }
            else
            {
              forward = getnext(forward);
            }
          }
          return (result);
        }

        this()
        {
          tail    = null;
          forward = null;
        }

        this(ChildType c)
        {
          begin(c);
        }

        /// used to initialize iterator
        ChildType begin(ChildType c)
        {
          tail = c;

          reset();

          return opPostInc();
        }

        /// resets iterator to the beginning of the list
        void reset()
        {
          if (tail)
            forward = getnext(tail);

          if (forward is tail)
            forward = null;
        }

      private:
        ChildType   tail;       /// reference to list to be able reset the iterator
        ChildType   forward;    /// next node to be save to remove the
                                /// current node from the list during list traversal
    }
}

