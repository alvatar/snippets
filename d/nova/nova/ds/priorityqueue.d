//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.ds.priorityqueue;

private import tango.util.log.Log;

//////////////////////////////////////////////////////////////////////////
/**

  Implementation of a heap base priority queue as described in [Sedgewick]

  References:

  [Sedgewick] Sedgewick, Algorithmen in C++

*/////////////////////////////////////////////////////////////////////////

struct PriorityQueue(ValueType)
{
  alias int         KeyType;

  protected struct ElementType
  {
    KeyType   key;
    ValueType value;
  }

  protected ElementType[]  mData;       // contains key/value pairs
  protected int            mCount;      // number of elements


  invariant
  {
    assert( mCount <= mData.length );
  }

  bool empty()
  {
    return (mCount == 0);
  }

  /// insert a value at position key
  void insert(KeyType key, ValueType value)
  {
    capacity(mCount+1);
    mData[mCount].key   = key;
    mData[mCount].value = value;
    ++mCount;
    fixupTail();
  }

  /// remove and return the value with the smallest key
  ValueType remove()
  {
    ValueType val = mData[0].value;
    mData[0] = mData[--mCount];
    mData[mCount].key = KeyType.init;
    fixupHead();
    return val;
  }

  /// fixup head after removal
  private void fixupHead()
  {
    TypeInfo ti = typeid(ElementType);
    int idx = 0;
    for(;;)
    {
      int nxt = 2 * idx + 1;
      if (nxt >= mCount)
        break;
      if ((nxt != mCount-1) &&
          (mData[nxt].key > mData[nxt+1].key))
        nxt++;
      if (mData[idx].key > mData[nxt].key)
      {
        ElementType swap = mData[idx];
        ti.swap(&mData[idx],&mData[nxt]);
        idx = nxt;
      }
      else
      {
        break;
      }
    }
  }

  /// fixup tail after insertion
  private void fixupTail()
  {
    TypeInfo ti = typeid(ElementType);
    int idx = mCount - 1;

    int nxt = (idx - 1 ) >> 1;
    while ((idx > 0) && (mData[idx].key < mData[nxt].key))
    {
      ti.swap(&mData[idx],&mData[nxt]);
      idx = nxt;
      nxt = (idx - 1) >> 1;
    }
  }

  /// Ensure the minimum capacity of heap.
  private void capacity(int cap)
  {
    if (cap > mData.length)
    {
      cap = (cap+1) * 2;
      mData.length = cap;
    }
  }
}

unittest
{
  auto logger = Log.getLogger("unittests.priorityqueue");

  logger.info("Start unittest");

  PriorityQueue!(int) pq;

  const int numNodes   = 100;

  assert(pq.empty());

  for (int idx=0; idx < numNodes; ++idx)
  {
    pq.insert(idx, idx);
  }

  for (int idx=0; idx < numNodes; ++idx)
  {
    int val = pq.remove();
    assert (val == idx);
  }

  for (int idx=numNodes-1; idx >= 0; --idx)
  {
    pq.insert(idx, idx);
  }

  for (int idx=0; idx < numNodes; ++idx)
  {
    int val = pq.remove();
    assert (val == idx);
  }

  assert(pq.empty());

  logger.info("Done");
}

