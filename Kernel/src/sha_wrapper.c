/*
 * Configuration wrapper for sha.c
 */

#include "config.h"
#include <stddef.h>

#if SIZEOF_INT != 4
#error "sha.c requires 32-bit int"
#endif

#undef LITTLE_ENDIAN
#ifndef WORDS_BIGENDIAN
#define LITTLE_ENDIAN
#endif

#define MEMORY_ONLY

#include "sha.c"

