//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.singlelinkedlist;

/// Link element introduced into the parent
struct ParentLink(ChildType)
{
  ChildType head;
  ChildType tail;
}

/// Link element introduced into the child
struct ChildLink(ChildType)
{
  ChildType next;
}

//////////////////////////////////////////////////////////////////////////
/**

  An intrusive singlelinked list consists of a parent maintaining a
  head and tail reference into a singlelinked ring of childs.

  $(IMG image/singlelinkedlist.png)

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

  2. Introduce link(s) into parent class

  ---
  class ListParentOne
  {
    public :
      ParentLink!(TwoListNode) parent;
  }

  class ListParentTwo
  {
    public :
      ParentLink!(TwoListNode) parent;
  }
  ---

  3. Define accessor class(es)
  ---
  static class ListParentOneAccessor
  {
    public :
      alias ListParentOne ParentType;
      alias TwoListNode ChildType;

      static ParentLink!(TwoListNode)* getparent(ListParentOne p)
      {
        return &p.parent;
      };

      static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
      {
        return &c.link1;
      };
  }

  static class ListParentTwoAccessor
  {
    alias ListParentTwo ParentType;
    alias TwoListNode ChildType;

    public :
      static ParentLink!(TwoListNode)* getparent(ListParentTwo p)
      {
        return &p.parent;
      };

      static ChildLink!(TwoListNode)*  getnode(TwoListNode c)
      {
        return &c.link2;
      };
  }
  ---

  4. Create parent instances
  ---
  int main()
  {
    ListParentOne  par1 = new ListParentOne;
    ListParentTwo  par2 = new ListParentTwo;
  ---

  5. Append child nodes
  ---
    for (int idx = 0; idx < num; ++idx)
    {
      TwoListNode a = new TwoListNode(idx);
      list1.addTail(par1, a);
      list2.addHead(par2, a);
    }
  ---

  6. Traverse child nodes  via foreach
  ---
    list1.Iterator it = new list1.Iterator(par1);
    foreach(TwoListNode trav; it)
    {
      printf("  node: %i\n", trav.id);
    }
  ---

  7. Traverse child nodes  forward via opPostInc
  ---
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.begin(par2);
    while (trav)
    {
      printf("  node: %i\n", trav.id);
      trav = it++;
    }
  ---

  9. Remove child nodes within foreach loop
  ---
    list1.Iterator it = new list1.Iterator(par1);
    foreach(TwoListNode trav; it)
    {
      list1.remove(par1, trav);
    }
  ---

  10. Remove child nodes within opPostInc loop
  ---
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.begin(par2);
    while (trav)
    {
      printf("  remove: %i\n", trav.id);
      list2.remove(par2, trav);
      trav = it++;
    }
  ---

*/////////////////////////////////////////////////////////////////////////
class SingleLinkedList(alias Accessor)
{
  private:
    /// type traits
    alias Accessor.ParentType ParentType;
    alias Accessor.ChildType  ChildType;

    /// node access
    static ChildType getnext(ChildType cur)               { return Accessor.getnode(cur).next;   }
    static void      setnext(ChildType cur, ChildType n)  { Accessor.getnode(cur).next = n;      }

    /// parent access
    static ChildType gethead(ParentType cur)              { return Accessor.getparent(cur).head; }
    static void      sethead(ParentType cur, ChildType n) { Accessor.getparent(cur).head = n;    }
    static ChildType gettail(ParentType cur)              { return Accessor.getparent(cur).tail; }
    static void      settail(ParentType cur, ChildType n) { Accessor.getparent(cur).tail = n;    }

  public:
    /// assign a child (drops previous connected ring)
    static void     assign (ParentType  p, ChildType c)
    {
      settail(p, c);
      sethead(p, (c !is null) ? (getnext(c)) : (null));
    }

    /// add child to tail
    static void addTail(ParentType p, ChildType n)
    in
    {
      assert(p);
      assert(n);
    }
    body
    {
      if (null !is getnext(n))
      {
        assert(false);
        // warning("addTail() not executed, object already in a collection=",c);
      }
      else
      {
        if (gettail(p))
        {
          setnext(n, gethead(p));
          setnext(gettail(p), n);
          settail(p, n);
        }
        else
        {
          sethead(p, n);
          settail(p, n);
          setnext(n, n);
        }
      }
    }

    /// append child c after child t
    static ChildType append(ChildType t, ChildType c)
    in
    {
      assert(t);
      assert(c);
    }
    body
    {
      if (null !is getnext(c))
      {
        // addTail() not executed, object already in a collection
        assert(false);
      }
      else if (null is t)
      {
        // no valid tail node, create circular list
        setnext(c, c);
        return c;
      }
      else
      {
        // t points to the tail node - append and return new tail
        setnext(c, getnext(t));
        setnext(t, c);
        return c;
      }
    }

    /// add child to head of list
    static void     addHead(ParentType  p, ChildType c)
    in
    {
      assert(p);
      assert(c);
    }
    body
    {
      if (null !is getnext(c))
      {
        // addHead() not executed, object already in a collection
        assert(false);
      }
      else
      {
        if(gethead(p))
        {
          setnext(c, gethead(p));
          sethead(p, c);
          setnext(gettail(p), c);
        }
        else
        {
          sethead(p, c);
          settail(p, c);
          setnext(c, c);
        }
      }
    }

    /// prepend a child c before child t
    static ChildType prepend(ChildType    t, ChildType c)
    in
    {
      assert(t);
      assert(c);
    }
    body
    {
      if (null !is getnext(c))
      {
        assert(false);
        // warning("addHead() not executed, object already in a collection=",c);
      }
      else if (null is t)
      {
        // no valid tail node, create circular list
        setnext(c, c);
        return c;
      }
      else
      {
        // t points to the tail node - append, tail stays the same
        setnext(c, getnext(t));
        setnext(t, c);
        return t;
      }
    }

    /// remove a child c from list maintained by parent p
    static void     remove (ParentType  p, ChildType c)
    in
    {
      assert(p);
      assert(c);
    }
    body
    {
      ChildType trav;
      ChildType tail;
      tail =
      trav = gettail(p);

      do
      {
        if (getnext(trav) is c)
        {
          if (trav is c)
          {
            // only element in list clear list
            sethead(p, null);
            settail(p, null);
          }
          else
          {
            // chain out
            setnext(trav, getnext(c));
            if(gethead(p) is c)
            {
              sethead(p, getnext(c));
            }

            if(gettail(p) is c)
            {
              settail(p, getnext(c));
            }
          }
          setnext(c, null);
          break;
        }

        trav = getnext(trav);
      }
      while (trav !is tail)
    }

    /// count number of childs maintained by parent p
    static int      count  (ParentType  p)
    {
      Iterator it = new Iterator(p);

      int cnt = 0;
      foreach(ChildType trav; it)
      {
        ++cnt;
      }
      return (cnt);
    }

    //////////////////////////////////////////////////////////////////////////
    /**

      Iterator class, supports forward traversal over single linked list

    */////////////////////////////////////////////////////////////////////////
    static class Iterator
    {
      private:
        /// Node access
        static ChildType getnext(ChildType cur)   { return Accessor.getnode(cur).next;   }

        /// parent access
        static ChildType gethead(ParentType cur)  { return Accessor.getparent(cur).head; }

      public:
        /// support for foreach statements
        int opApply(int delegate(inout ChildType) dg)
        {
          int res = 0;

          ChildType curnode = gethead(list);
          while (curnode)
          {
            // remember reference to next node
            // before calling delegate:
            //  current node could be removed from list
            ChildType next = getnext(curnode);
            if (gethead(list) == next)
            {
              next = null;
            }
            res = dg(curnode);
            curnode = next;

            if (res) break;
          }
          return res;
        }

        /// support for ++ operator
        ChildType opPostInc()
        {
          if (null is list)
            return null;

          ChildType result = forward;

          if (null !is forward)
          {
            if (forward is gettail(list))
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
          list    = null;
          forward = null;
        }

        this(ParentType p)
        {
          begin(p);
        }

        /// used to initialize iterator
        ChildType begin(ParentType p)
        {
          list = p;

          reset();

          return (gethead(list));
        }

        /// resets iterator to the beginning of the list
        void reset()
        {
          if (list)
            forward = getnext(gethead(list));

          if (forward is gethead(list))
            forward = null;
        }

      private:
        ParentType  list;       /// reference to list to be able reset the iterator
        ChildType   forward;    /// next node to be save to remove the
                                /// current node from the list during list traversal
    }
}
