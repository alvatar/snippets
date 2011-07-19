//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.intrusive.doublelinkedlist;

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
  ChildType prev;
}

//////////////////////////////////////////////////////////////////////////
/**

  An intrusive doublelinked list consists of a parent maintaining a
  head and tail reference into a doublelinked ring of childs.

  $(IMG image/doublelinkedlist.png)

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

  8. Traverse child nodes  forward via opPostDec
  ---
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.end(par2);
    while (trav)
    {
      printf("  node: %i\n", trav.id);
      trav = it--;
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

  10. Remove child nodes within opPostDec loop
  ---
    list2.Iterator it = new list2.Iterator();
    TwoListNode trav = it.end(par2);
    while (trav)
    {
      printf("  node: %i\n", trav.id);
      trav = it--;
    }
  ---

*/////////////////////////////////////////////////////////////////////////
class DoubleLinkedList(alias Accessor)
{
  private:
    /// type traits
    alias Accessor.ParentType ParentType;
    alias Accessor.ChildType  ChildType;

    /// child access
    static ChildType  getnext(ChildType cur)                { return Accessor.getnode(cur).next;   }
    static void       setnext(ChildType cur, ChildType n)   { Accessor.getnode(cur).next = n;      }
    static ChildType  getprev(ChildType cur)                { return Accessor.getnode(cur).prev;   }
    static void       setprev(ChildType cur, ChildType n)   { Accessor.getnode(cur).prev = n;      }

    /// parent access
    static ChildType  gethead(ParentType cur)               { return Accessor.getparent(cur).head; }
    static void       sethead(ParentType cur, ChildType n)  { Accessor.getparent(cur).head = n;    }
    static ChildType  gettail(ParentType cur)               { return Accessor.getparent(cur).tail; }
    static void       settail(ParentType cur, ChildType n)  { Accessor.getparent(cur).tail = n;    }

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
        // addTail() not executed, object already in a collection
        assert(false);
      }
      else
      {
        if (gettail(p))
        {
          setnext(n, gethead(p));
          setprev(n, gettail(p));
          setnext(gettail(p), n);
          setprev(gethead(p), n);
          settail(p, n);
        }
        else
        {
          sethead(p, n);
          settail(p, n);
          setprev(n, n);
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
        setprev(c, c);
        return c;
      }
      else
      {
        // t points to the tail node - append and return new tail
        ChildType next = getnext(t);
        setprev(c, t);
        setprev(next, c);
        setnext(c, next);
        setnext(t, c);
        return c;
      }
    }

    /// add child to head of list
    static void addHead(ParentType  p, ChildType c)
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
          setprev(c, gettail(p));
          setnext(gettail(p), c);
          setprev(gethead(p), c);
          sethead(p, c);
        }
        else
        {
          sethead(p, c);
          settail(p, c);
          setprev(c, c);
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
        // addHead() not executed, object already in a collection
        assert(false);
      }
      else if (null is t)
      {
        // no valid tail node, create circular list
        setprev(c, c);
        setnext(c, c);
        return c;
      }
      else
      {
        // t points to the tail node - append, tail stays the same
        ChildType next = getnext(t);
        setprev(c, t);
        setprev(next, c);
        setnext(c, next);
        setnext(t, c);
        return t;
      }
    }

    /// remove a child c from list maintained by parent p
    static void remove(ParentType  p, ChildType c)
    in
    {
      assert(p);
      assert(c);
    }
    body
    {
      ChildType prev = getprev(c);
      ChildType next = getnext(c);

      if( (!gettail(p)) ||
          (!gethead(p)) ||
          (!prev)       ||
          (!next)
        )
      {
        // corrupted collection, cannot remove
        assert(false);
      }
      else
      {
        if(prev is c)
        {
          settail(p, null);
          sethead(p, null);
        }
        else
        {
          if(gettail(p) is c)
          {
            settail(p, prev);
          }
          setnext(prev, next);
          setprev(next, prev);
          sethead(p, getnext(gettail(p)));
        }
        setnext(c, null);
        setprev(c, null);
      }
    }

    /// count number of childs maintained by parent p
    static int count  (ParentType  p)
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

      Iterator class, supports forward and backward traversal over
      doublelinked list

    */////////////////////////////////////////////////////////////////////////
    static class Iterator
    {
      private:
        // Node access
        static ChildType getnext(ChildType cur)   { return Accessor.getnode(cur).next;   }

        // parent access
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

        /// support for -- operator
        ChildType opPostDec()
        {
          if (null is list)
            return null;

          ChildType result = backward;

          if (null !is backward)
          {
            if (backward is gethead(list))
            {
              backward = null;
            }
            else
            {
              backward = getprev(backward);
            }
          }
          return (result);
        }

        this()
        {
          list     = null;
          forward  = null;
          backward = null;
        }

        this(ParentType p)
        {
          begin(p);
        }

        /// set iterator to beginning of list
        ChildType begin(ParentType p)
        {
          list = p;

          if (list)
          {
            forward  = getnext(gethead(list));
            backward = getprev(gethead(list));
          }
          if (forward is gethead(list))
          {
            forward  = null;
            backward = null;
          }

          return (gethead(list));
        }

        /// set iterator to end of list
        ChildType end(ParentType p)
        {
          list = p;

          if (list)
          {
            forward  = getnext(gettail(list));
            backward = getprev(gettail(list));
          }
          if (forward is gettail(list))
          {
            forward  = null;
            backward = null;
          }

          return (gettail(list));
        }

      private:
        ParentType  list;       /** reference to list to be able reset the iterator  */
        ChildType   forward;    /** forward node to be save to remove the
                                    current node from the list during list traversal */
        ChildType   backward;   /** backward node to be save to remove the
                                    current node from the list during list traversal */
    }
}
