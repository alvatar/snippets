// lrucache.d
// to test for correctness, compile thus:
// dmd -version="testLRUCache" lrucache.d
//
/**
  *   Author:  Charles Hixson, charleshixsn@earthlink.net
  *   Date:    Nov. 8, 2007
  *   Version: 0.2.b
  *         (Note:  Version 0.1 was lacking a version number)
  *   License: MIT license
  *         (Note:  Some future version will switch to GPL...but this stuff
  *          *SHOULD* be public domain.)
  *   Copyright:  Charles Hixson, all rights reserved.  (But see the license.)
  */

import   std.conv;
import   std.stdio;

/**
   *  Key must implement opCmp and be hashable
   *  Cache's should not be too large, so the size is a ushort.  That's excessive.
   *  If you want a cache nearly that large, use a database or something.
   */
class LRUCache(Key, Body)
{
private:
   typedef  ushort   NodeId;
   Body[Key]   bodies;
   NodeId[Key] nodIds;
   Body        _invalid;
   ushort      _maxLength;
   NodeId      maxNode;

public:
   this  (ushort maxLength, Body invalid)
   {  _invalid    =  invalid;
      _maxLength  =  maxLength;
   }


   const (Body)   opIndex  (Key k)
   {  if (auto b  =  k  in bodies)
      {  nodIds[k]   =  ++maxNode;
         if (maxNode >= _maxLength - 2)   renumber;
         return   *b;
      }
      return   _invalid;
   }

   const (Body)   opIndexAssign  (Body b, Key k)
   {  //Node n;
      if (b == _invalid)   return   _invalid;
      nodIds[k]   =  ++maxNode;
      bodies[k]   =  b;
      if (bodies.length > _maxLength)
      {  // cache is too large, so remove approx. the 1/4 least recently used
         ////  N.B.:  Expiration is based on access time.  Creation time is (so far) unused.
         ulong sum   =  0;
         NodeId   maxT  =  0;
         NodeId   minT  =  maxNode;
         NodeId   meanT;
         foreach  (nId; nodIds)
         {  sum   += nId;
            if (nId < minT)  minT  =  nId;
            if (nId > maxT)  maxT  =  nId;
         }
         meanT =  cast(NodeId)(sum / nodIds.length);
         NodeId   limit =  cast(NodeId)(minT + (meanT - minT) / 2);
         foreach  (Key key, NodeId nId; nodIds)
         {  if (nId <= limit)
            {  nodIds.remove(key);
               bodies.remove(key);
            }
            else      nodIds[key]  =  cast(NodeId)(nodIds[key] - limit + 1);
         }
         maxNode  =  cast(NodeId)(maxNode - limit + 1);
      }
      if (maxNode >= _maxLength - 2)   renumber;
      return   b;
   }

   int   length() {  return   bodies.length; }

   void  remove   (Key k)
   {  if (k in bodies)
      {  nodIds.remove(k);
         bodies.remove(k);
      }
   }

   bool  contains (Key k)
   {  if (k in nodIds)
      {  nodIds[k]   =  ++maxNode;
         return   true;
      }
      return   false;
   }

   ///   for diagnostic purposes only.  Do NOT use this value.
   ulong   nodeId(Key k)
   {  if (auto nId   =  k in nodIds)  return   *nId;   return   0; }

   /**
   *  renumber the nodes.  This is a separate routine to allow different
   *  algorithms to be easily inserted.  I've picked quick & easy.
   */
   private  void  renumber()
   {  NodeId   nId   =  0;
      foreach  (Key key, NodeId node; nodIds)   nodIds[key] =  ++nId;
      maxNode  =   cast(NodeId)((nId > 0) ?  nId - 1  :  0);
   }
   version  (testLRUCache)
   {  void  dump()
      {  writef("there are %s entries in the cache\n", bodies.length);
         Key[] keys  =  bodies.keys;
         for   (int i = 0; i < keys.length;  i++)
         {  writef ("%2s", keys[i]);
            writef (": %10s", nodIds[keys[i]]);
            writef (": %s\n", bodies[keys[i]]);
         }
      }
   }
}


version  (testLRUCache)
{
   void  main()
   {  auto  l  =  new   LRUCache!(int, char)(10, cast(char)0);
      string   s  =  "This is a rather long string of quasi-random text,";
      int   i  =  0;
      foreach  (char c; s)
      {  writef ("adding %d\n", i);
         l[i++]   =  c;
      }
      l.dump;
      for   (i = 0;  i < s.length;  i++)
      {  if (l.contains(i))   writef ("l[%2s] = '%s', id = %s\n", i, l[i], l.nodeId(i));
         //else                 writef ("l[%2s] = **missing**\n", i);
      }
      auto  l2 =  new   LRUCache!(char, int)(10, -17);
      //string s  =  "This is a rather long string of quasi-random text,";
      //int i  =  0;
      i  =  0;
      foreach  (char c; s)
      {  writef ("adding %d\n", i);
         l2[c] =  ++i;
      }
      l2.dump;
      // this next bit shows that accesses change the nodeId
      // remember that nodeId should not be used for any purpose outside of the class...
      // it's highly unstable.
      for   (i = 0;  i < s.length;  i++)
      {  if (l2.contains(s[i]))
                  writef ("l2[%2s] = '%s', id = %s\n", s[i], l2[s[i]], l2.nodeId(s[i]));
         //else
         //       writef ("l2[%2s] = **missing**\n", s[i]);
      }
   }
}
