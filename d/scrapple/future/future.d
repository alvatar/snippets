/**
 * future -- Simple concurrent execution
 * Written by Daniel Keep.
 * Released to the Public Domain.
 *
 * Quick Summary
 * =============
 * 
 * auto result = future(some_function, arg1, arg2, more_args);
 *
 * This executes "some_function" with the specified arguments
 * asynchronously, allowing you to do other things while it runs.  When you
 * need the return value of the function call, you can use:
 *
 *   result.value
 *
 * This will BLOCK your current thread until the asynchronous call has
 * finished.  So the idea is to start off asynchronous calls as early as
 * possible.
 *
 * Or, if the function has no return value, but you need to wait until its'
 * finished, use this:
 *
 *   result.wait
 *
 * The code below should be fairly clear, so if you're wondering what it's
 * really doing under the hood, read the code :)
 */
module future;

import std.thread : Thread;

/**
 * This class is what we'll use to store the results of our "future"
 * calls in.  All that nasty private stuff is basically just bookkeeping for
 * the thread we'll spawn off, which allows us to kick off the thread, and
 * then store the result when it's finished.
 */
class FutureResult(tFn, tArgs...)
{
    private
    {
        alias tTaskResult!(tFn) tResult; // result type of future call

        tFn fn; // function to call
        tArgs args; // arguments to pass to it
        bool got_result = false; // have we got a result yet?
        Thread thread; // which thread are we using?

        static if( !is( tResult == void ) )
            tResult result; // what is the result?

        // start is used as the entry point to the thread.  All it's doing
        // is calling the function, saving the return value (if there is
        // one), and then setting the "got_result" flag.
        int start()
        {
            static if( !is( tResult == void ) )
                result = fn(args);
            else
                fn(args);

            got_result = true;
            return 0;
        }
    }

    static if( !is( tResult == void ) )
    {
        /**
         * Either returns the value if we already know what it is, or else
         * block until we do.
         */
        tResult value()
        {
            while( !got_result )
                Thread.yield();
            return result;
        }
    }
    else
    {
        /**
         * Wait until the function call has returned.
         */
        void wait()
        {
            while( !got_result )
                Thread.yield();
        }
    }
}

/* These two templates are used to get the return type of a function.  In a
 * case of "do as I say, not as I do", YOU should use std.traits.ReturnType
 * in your code :3
 */
private template tTaskResult(tFn)
{
    alias tiTaskResult!(tFn).result tTaskResult;
}

private template tiTaskResult(tFn)
{
    static if( is( tFn tRealFn == delegate ) )
        alias tiTaskResult!(tRealFn).result result;
    
    else static if( is( tFn tReturn == return ) )
        alias tReturn result;

    else
        static assert("Cannot derive result from function type.");
}

/**
 * Performs a function call asynchronously.  The "future" refers both to the
 * fact that the result of this function is intended to be used at some time
 * in the future, and that its' basically what the "future" keyword in C++0x
 * is supposed to do.
 *
 * The way it works is quite simple: when you have some lengthy function
 * call to make, call the function as early as possible, and store away the
 * return.  Then, when you actually need the result, you ask for it.  If the
 * function call completed in the background, it will return the result
 * immediately.  If it hasn't, then it will block until the result is
 * available.  For example:
 *
 * -------------------------------------------------------------------------
 * import std.stdio;
 * import etc.future;
 *
 * int meaningOfLife()
 * {
 *     // Some long, complex calculations.
 *     return 42;
 * }
 *
 * void main()
 * {
 *     auto meaning = future(meaningOfLife);
 *
 *     // More complicated computations.
 *
 *     writefln("Meaning of life is: %d", meaning.value);
 * }
 * -------------------------------------------------------------------------
 */
FutureResult!(tFn, tArgs) future(tFn, tArgs...)(tFn fn, tArgs args)
{
    // Create and fill in the result class instance.
    auto result = new FutureResult!(tFn, tArgs);

    result.fn = fn;

    foreach( i,a ; args )
        result.args[i] = a;

    // Kick off the thread, and return the instance.
    result.thread = new Thread(&result.start);
    result.thread.start();

    return result;
}

unittest
{
    int meaningOfLife(int init)
    {
        int result = init;
        for( int i=1; i<10_000; i++ )
            result = (result * i) + (i << 1) - (result >> 1);
        
        // Bah; stuff that!
        return 42;
    }

    void shortPause(int loops)
    {
        while( loops )
            loops--;
    }

    scope life = future(&meaningOfLife, 0xBeef);
    assert( life.value == 42 );

    scope pause = future(&shortPause, 1_000);
    pause.wait();
}

// Compile with "-version=future_main -unittest" to run this module by
// itself.
version( future_main )
{
    import std.stdio;

    void main()
    {
        writefln("Tests complete.");
    }
}

