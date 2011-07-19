
module dcairo.cairo;

public import dcairo.cairo_types;
public import dcairo.cairo_functions;

private import dloader.library_loader;
package SharedLib libCairo;


import std.conv;


/**
 * This function will attempt to load the cairo library.
 */
public void cairo_load( immutable(char)[] libName )
{
    if( libCairo !is null )
        return;
    
    libCairo = loadSharedLib(libName);

    /* Try to find out what version of cairo we've got.  If it's too old,
     * throw an exception explaining this.
     */
    auto versionnum = (cast(pf_cairo_version)
            getProc(libCairo,"cairo_version"))();

    version( cairo_1_9 )
        auto expected_version = CAIRO_VERSION_ENCODE(1,9,0);
    else
        auto expected_version = CAIRO_VERSION_ENCODE(1,8,0);
    
    if( versionnum < expected_version )
        throw new CairoVersionException(versionnum, expected_version);

    // Now we can actually load the functions.
    cairo_loadprocs(libCairo);
}

/// ditto
public void cairo_load()
{
    version(Windows)
        cairo_load("libcairo-2.dll");
    else version(linux)
        cairo_load("libcairo.so");
    else
    {
        pragma(msg, "I'm sorry, but implicit loading is not supported on"
                    " your platform.  Please call cairo_load with the"
                    " name of the library to load.");
        static assert(0);
    }
}

/**
 * This will unload the cairo library from memory.  This will be
 * called automatically when your program terminates
 */
public void cairo_unload()
{
    if( libCairo is null )
        return;
    
    unloadSharedLib(libCairo);
    libCairo = null;
}

version( Tango )
{
    import tango.text.convert.Integer : toUtf8;
    alias toUtf8 intToString;
}
else
{
    import std.string : toString;
    alias toString intToString;
}

private immutable(char)[] verToString(int ver)
{
    return to!string(ver/100_00)
        ~ "." ~ to!string((ver/100)%100)
        ~ "." ~ to!string(ver%100);
}

private class CairoVersionException : Exception
{
    this(int got, int expected)
    {
        this("expected cairo version "~verToString(expected)
                ~", got version "~verToString(got)~".");
    }

    this(immutable(char)[] msg)
    {
        super(msg);
    }
}

static ~this()
{
    cairo_unload();
}

