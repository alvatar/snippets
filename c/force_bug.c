/* Force a compilation error if condition is true */
#define BUILD_BUG_ON(condition) ((void)sizeof(char[1 - 2*!!(condition)]))

/* So, in your code, if you have some structure which must be, say a multiple of 8 bytes in size, maybe because of some hardware constraints, you can do: */
BUILD_BUG_ON((sizeof(struct mystruct) % 8) != 0);
