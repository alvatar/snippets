/* hashes.h */

/* bitsizeof is C++, sigh */
#define bitsizeof(t) (sizeof(t) * CHAR_BIT)

/* Adaptors */

typedef struct {
	uint32_t hash;
} hashctx;
