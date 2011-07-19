/**A struct to allocate memory in a strictly first-in last-out order for
 * things like scratch space.  Technically, memory can safely escape the
 * scope in which it was allocated.  However, this is a very bad idea
 * unless being done within the private API of a class, struct or nested
 * function, where it can be guaranteed that LIFO will not be violated.
 *
 * Under the hood, this works by allocating large blocks (currently 4 MB)
 * from the GC, and sub-allocating these as a stack.  Very large allocations
 * (currently > 4MB) are simply performed on the heap.  There are two ways to
 * free memory:  Calling TempAlloc.free() frees the last allocated block.
 * Calling TempAlloc.frameFree() frees all memory allocated since the last
 * call to TempAlloc.frameInit().
 *
 * All allocations are aligned on 16-byte boundaries using padding, since on x86,
 * 16-byte alignment is necessary to make SSE2 work.  Note, however, that this
 * is implemented based on the assumption that the GC allocates using 16-byte
 * alignment (which appears to be true in druntime.)
 *
 * Author:  David Simcha
 * License: Use and redistribution in both binary and source form, both
 *          modified and unmodified, for both commercian and non-
 *          commercial purposes is hereby permitted subject to:
 *
 *          1. The author(s) hereby disclaim all warranties, both express
 *             and implied.
 *          2. A statement acknowledging the contributions of the authors
 *             of this module would be appreciated but is not required.
 *
 * Bugs:
 *     No data stored in memory allocated by TempAlloc is scanned by the
 *     GC.  This reflects its intended use as a scratch space to store
 *     things like simple primitives.  Due to false pointer issues, etc.
 *     making this memory be scanned by the GC would just plain be a bad
 *     idea.  DO NOT store references to reference types in TempAlloc-
 *     allocated memory unless you also store a reference in memory that is
 *     scanned by the GC.
 */

module tempalloc;

import core.memory, core.thread, std.traits, std.c.stdio : stderr;

// C functions, marked w/ nothrow.
extern(C) nothrow int fprintf(void*, in char *,...);
extern(C) nothrow void exit(int);

///
struct TempAlloc {
private:
    struct Stack(T) {  // Simple, fast stack w/o error checking.
        private size_t capacity;
        private size_t index;
        private T* data;
        private enum sz = T.sizeof;

        private static size_t max(size_t lhs, size_t rhs) pure nothrow {
            return (rhs > lhs) ? rhs : lhs;
        }

        void push(T elem) nothrow {
            if (capacity == index) {
                capacity = max(16, capacity * 2);
                data = cast(T*) ntRealloc(data, capacity * sz, cast(GC.BlkAttr) 0);
                data[index..capacity] = T.init;  // Prevent false ptrs.
            }
            data[index++] = elem;
        }

        T pop() nothrow {
            index--;
            auto ret = data[index];
            data[index] = T.init;  // Prevent false ptrs.
            return ret;
        }
    }

    struct Block {
        size_t used = 0;
        void* space = null;
    }

    final class State {
        size_t used;
        void* space;
        size_t totalAllocs;
        void*[] lastAlloc;
        uint nblocks;
        uint nfree;
        size_t frameIndex;

        // inUse holds info for all blocks except the one currently being
        // allocated from.  freelist holds space ptrs for all free blocks.
        Stack!(Block) inUse;
        Stack!(void*) freelist;

        ~this() {  // Blocks are pretty large.  Prevent false ptrs.
            ntFree(lastAlloc.ptr);
            while(nblocks > 1) {
                ntFree((inUse.pop()).space);
                nblocks--;
            }
            ntFree(space);
            while(nfree > 0) {
                ntFree(freelist.pop);
                nfree--;
            }
        }
    }

    // core.thread.Thread.thread_needLock() is nothrow (read the code if you
    // don't believe me) but not marked as such because nothrow is such a new
    // feature in D.  This is a workaround until that gets fixed.
    static enum tnl = cast(bool function() nothrow) &thread_needLock;

    enum blockSize = 4U * 1024U * 1024U;
    enum nBookKeep = 1_048_576;  // How many bytes to allocate upfront for bookkeeping.
    enum alignBytes = 16U;
    static __thread State state;
    static State mainThreadState;

    static void die() nothrow {
        fprintf(stderr, "TempAlloc error: Out of memory.\0".ptr);
        exit(1);
    }

    static void doubleSize(ref void*[] lastAlloc) nothrow {
        size_t newSize = lastAlloc.length * 2;
        void** ptr = cast(void**)
        ntRealloc(lastAlloc.ptr, newSize * (void*).sizeof, GC.BlkAttr.NO_SCAN);

        if (lastAlloc.ptr != ptr) {
            ntFree(lastAlloc.ptr);
        }

        lastAlloc = ptr[0..newSize];
    }

    static void* ntMalloc(size_t size, GC.BlkAttr attr) nothrow {
        try { return GC.malloc(size, attr); } catch { die(); }
    }

    static void* ntRealloc(void* ptr, size_t size, GC.BlkAttr attr) nothrow {
        try { return GC.realloc(ptr, size, attr); } catch { die(); }
    }

    static void ntFree(void* ptr) nothrow {
        try { GC.free(ptr); } catch {}
    }

    static size_t getAligned(size_t nbytes) pure nothrow {
        size_t rem = nbytes % alignBytes;
        return (rem == 0) ? nbytes : nbytes - rem + alignBytes;
    }

    static State stateInit() nothrow {
        State stateCopy;
        try { stateCopy = new State; } catch { die(); }

        with(stateCopy) {
            space = ntMalloc(blockSize, GC.BlkAttr.NO_SCAN);
            lastAlloc = (cast(void**) ntMalloc(nBookKeep, GC.BlkAttr.NO_SCAN))
                        [0..nBookKeep / (void*).sizeof];
            nblocks++;
        }

        state = stateCopy;
        if (!tnl())
            mainThreadState = stateCopy;
        return stateCopy;
    }

public:
    /**Allows caller to cache the state class on the stack and pass it in as a
     * parameter.  This is ugly, but results in a speed boost that can be
     * significant in some cases because it avoids a thread-local storage
     * lookup.  Also used internally.*/
    static State getState() nothrow {
        // Believe it or not, even with builtin TLS, the thread_needLock()
        // is worth it to avoid the TLS lookup.
        State stateCopy = (tnl()) ? state : mainThreadState;
        return (stateCopy is null) ? stateInit : stateCopy;
    }

    /**Initializes a frame, i.e. marks the current allocation position.
     * Memory past the position at which this was last called will be
     * freed when frameFree() is called.  Returns a reference to the
     * State class in case the caller wants to cache it for speed.*/
    static State frameInit() nothrow {
        return frameInit(getState);
    }

    /**Same as frameInit() but uses stateCopy cached on stack by caller
     * to avoid a thread-local storage lookup.  Strictly a speed hack.*/
    static State frameInit(State stateCopy) nothrow {
        with(stateCopy) {
            if (totalAllocs == lastAlloc.length) // Should happen very infrequently.
                doubleSize(lastAlloc);
            lastAlloc[totalAllocs] = cast(void*) frameIndex;
            frameIndex = totalAllocs;
            totalAllocs++;
        }
        return stateCopy;
    }

    /**Frees all memory allocated by TempAlloc since the last call to
     * frameInit().*/
    static void frameFree() nothrow {
        frameFree(getState);
    }

    /**Same as frameFree() but uses stateCopy cached on stack by caller
    * to avoid a thread-local storage lookup.  Strictly a speed hack.*/
    static void frameFree(State stateCopy) nothrow {
        with(stateCopy) {
            while (totalAllocs > frameIndex + 1) {
                free(stateCopy);
            }
            frameIndex = cast(size_t) lastAlloc[--totalAllocs];
        }
    }

    /**Purely a convenience overload, forwards arguments to TempAlloc.malloc().*/
    static void* opCall(T...)(T args) nothrow {
        return TempAlloc.malloc(args);
    }

    /**Allocates nbytes bytes on the TempAlloc stack.  NOT safe for real-time
     * programming, since if there's not enough space on the current block,
     * a new one will automatically be created.  Also, very large objects
     * (currently over 4MB) will simply be heap-allocated.
     *
     * Bugs:  Memory allocated by TempAlloc is not scanned by the GC.
     * This is necessary for performance and to avoid false pointer issues.
     * Do not store the only reference to a GC-allocated object in
     * TempAlloc-allocated memory.*/
    static void* malloc(size_t nbytes) nothrow {
        return malloc(nbytes, getState);
    }

    /**Same as malloc() but uses stateCopy cached on stack by caller
    * to avoid a thread-local storage lookup.  Strictly a speed hack.*/
    static void* malloc(size_t nbytes, State stateCopy) nothrow {
        nbytes = getAligned(nbytes);
        with(stateCopy) {
            void* ret;
            if (blockSize - used >= nbytes) {
                ret = space + used;
                used += nbytes;
            } else if (nbytes > blockSize) {
                ret = ntMalloc(nbytes, GC.BlkAttr.NO_SCAN);
            } else if (nfree > 0) {
                inUse.push(Block(used, space));
                space = freelist.pop;
                used = nbytes;
                nfree--;
                nblocks++;
                ret = space;
            } else { // Allocate more space.
                inUse.push(Block(used, space));
                space = ntMalloc(blockSize, GC.BlkAttr.NO_SCAN);
                nblocks++;
                used = nbytes;
                ret = space;
            }
            if (totalAllocs == lastAlloc.length) {
                doubleSize(lastAlloc);
            }
            lastAlloc[totalAllocs++] = ret;
            return ret;
        }
    }

    /**Frees the last piece of memory allocated by TempAlloc.  Since
     * all memory must be allocated and freed in strict LIFO order,
     * there's no need to pass a pointer in.  All bookkeeping for figuring
     * out what to free is done internally.*/
    static void free() nothrow {
        free(getState);
    }

    /**Same as free() but uses stateCopy cached on stack by caller
    * to avoid a thread-local storage lookup.  Strictly a speed hack.*/
    static void free(State stateCopy) nothrow {
        with(stateCopy) {
            void* lastPos = lastAlloc[--totalAllocs];

            // Handle large blocks.
            if (lastPos > space + blockSize || lastPos < space) {
                ntFree(lastPos);
                return;
            }

            used = (cast(size_t) lastPos) - (cast(size_t) space);
            if (nblocks > 1 && used == 0) {
                freelist.push(space);
                Block newHead = inUse.pop;
                space = newHead.space;
                used = newHead.used;
                nblocks--;
                nfree++;

                if (nfree >= nblocks * 2) {
                    foreach(i; 0..nfree / 2) {
                        ntFree(freelist.pop);
                        nfree--;
                    }
                }
            }
        }
    }
}

/**Allocates an array of type T and size size using TempAlloc.
 * Note that appending to this array using the ~= operator,
 * or enlarging it using the .length property, will result in
 * undefined behavior.  This is because, if the array is located
 * at the beginning of a TempAlloc block, the GC will think the
 * capacity is as large as a TempAlloc block, and will overwrite
 * adjacent TempAlloc-allocated data, instead of reallocating it.
 *
 * Bugs: Do not store the only reference to a GC-allocated reference object
 * in an array allocated by newStack because this memory is not
 * scanned by the GC.*/
T[] newStack(T)(size_t size) nothrow {
    size_t bytes = size * T.sizeof;
    T* ptr = cast(T*) TempAlloc.malloc(bytes);
    return ptr[0..size];
}

/**Same as newStack(size_t) but uses stateCopy cached on stack by caller
* to avoid a thread-local storage lookup.  Strictly a speed hack.*/
T[] newStack(T)(size_t size, TempAlloc.State state) nothrow {
    size_t bytes = size * T.sizeof;
    T* ptr = cast(T*) TempAlloc.malloc(bytes, state);
    return ptr[0..size];
}

/**Concatenate any number of arrays of the same type, placing results on
 * the TempAlloc stack.*/
T[0] stackCat(T...)(T data) {
    foreach(array; data) {
        static assert(is(typeof(array) == typeof(data[0])));
    }

    size_t totalLen = 0;
    foreach(array; data) {
        totalLen += array.length;
    }
    auto ret = newStack!(Mutable!(typeof(T[0][0])))(totalLen);

    size_t offset = 0;
    foreach(array; data) {
        ret[offset..offset + array.length] = array[0..$];
        offset += array.length;
    }
    return cast(T[0]) ret;
}

/**Creates a duplicate of an array on the TempAlloc stack.*/
auto tempdup(T)(T[] data) nothrow {
    alias Mutable!(T) U;
    U[] ret = newStack!(U)(data.length);
    ret[] = data[];
    return ret;
}

/**Same as tempdup(T[]) but uses stateCopy cached on stack by caller
 * to avoid a thread-local storage lookup.  Strictly a speed hack.*/
auto tempdup(T)(T[] data, TempAlloc.State state) nothrow {
    alias Mutable!(T) U;
    U[] ret = newStack!(U)(data.length, state);
    ret[] = data;
    return ret;
}

/**A string to mixin at the beginning of a scope, purely for
 * convenience.  Initializes a TempAlloc frame using frameInit(),
 * and inserts a scope statement to delete this frame at the end
 * of the current scope.
 *
 * Slower than calling free() manually when only a few pieces
 * of memory will be allocated in the current scope, due to the
 * extra bookkeeping involved.  Can be faster, however, when
 * large amounts of allocations, such as arrays of arrays,
 * are allocated, due to caching of data stored in thread-local
 * storage.*/
invariant char[] newFrame =
    "TempAlloc.frameInit; scope(exit) TempAlloc.frameFree;";

unittest {
    /* Not a particularly good unittest in that it depends on knowing the
     * internals of TempAlloc, but it's the best I could come up w/.  This
     * is really more of a stress test/sanity check than a normal unittest.*/

     // First test to make sure a large number of allocations does what it's
     // supposed to in terms of reallocing lastAlloc[], etc.
     enum nIter =  TempAlloc.blockSize * 5 / TempAlloc.alignBytes;
     foreach(i; 0..nIter) {
         TempAlloc(TempAlloc.alignBytes);
     }
     assert(TempAlloc.getState.nblocks == 5);
     assert(TempAlloc.getState.nfree == 0);
     foreach(i; 0..nIter) {
        TempAlloc.free;
    }
    assert(TempAlloc.getState.nblocks == 1);
    assert(TempAlloc.getState.nfree == 2);

    // Make sure logic for freeing excess blocks works.  If it doesn't this
    // test will run out of memory.
    enum allocSize = TempAlloc.blockSize / 2;
    void*[] oldStates;
    foreach(i; 0..50) {
        foreach(j; 0..50) {
            TempAlloc(allocSize);
        }
        foreach(j; 0..50) {
            TempAlloc.free;
        }
        oldStates ~= cast(void*) TempAlloc.state;
        oldStates ~= cast(void*) TempAlloc.mainThreadState;
        TempAlloc.state = null;
        TempAlloc.mainThreadState = null;
    }
    oldStates = null;

    // Make sure data is stored properly.
    foreach(i; 0..10) {
        TempAlloc(allocSize);
    }
    foreach(i; 0..5) {
        TempAlloc.free;
    }
    GC.collect;  // Make sure nothing that shouldn't is getting GC'd.
    void* space = TempAlloc.mainThreadState.space;
    size_t used = TempAlloc.mainThreadState.used;

    TempAlloc.frameInit;
    // This array of arrays should not be scanned by the GC because otherwise
    // bugs caused th not having the GC scan certain internal things in
    // TempAlloc that it should would not be exposed.
    uint[][] arrays = (cast(uint[]*) GC.malloc((uint[]).sizeof * 10,
                       GC.BlkAttr.NO_SCAN))[0..10];
    foreach(i; 0..10) {
        uint[] data = newStack!(uint)(250_000);
        foreach(j, ref e; data) {
            e = j * (i + 1);  // Arbitrary values that can be read back later.
        }
        arrays[i] = data;
    }

    // Make stuff get overwrriten if blocks are getting GC'd when they're not
    // supposed to.
    GC.minimize;  // Free up all excess pools.
    uint[][] foo;
    foreach(i; 0..40) {
        foo ~= new uint[1_048_576];
    }
    foo = null;

    for(size_t i = 9; i != size_t.max; i--) {
        foreach(j, e; arrays[i]) {
            assert(e == j * (i + 1));
        }
    }
    TempAlloc.frameFree;
    assert(space == TempAlloc.mainThreadState.space);
    assert(used == TempAlloc.mainThreadState.used);
    while(TempAlloc.state.nblocks > 1 || TempAlloc.state.used > 0) {
        TempAlloc.free;
    }
    fprintf(stderr, "Passed TempAlloc test.\n\0".ptr);
}
