/* to initialize structures without needing to call memset. */

struct something X = {0};

/* This will initialize all of the members of the struct (or array) to zero (but not any padding bytes - use memset if you need to zero those as well). */
