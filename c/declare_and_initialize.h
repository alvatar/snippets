// Both declare and initialize variables in one module while in other modules using that module, merely declare them as externs.
#ifdef DEFINE_MYHEADER_GLOBALS
#define GLOBAL
#define INIT(x, y) (x) = (y)
#else
#define GLOBAL extern
#define INIT(x, y)
#endif

GLOBAL int INIT(x, 0);
GLOBAL int somefunc(int a, int b);

// With that, the code which defines x and somefunc does:
#define DEFINE_MYHEADER_GLOBALS
#include "the_above_header_file.h"

// while code that's merely using x and somefunc() does:
#include "the_above_header_file.h"
