#ifndef __GNUC__
/*
 * Must override the definition in generic/getopt.h.
 */
#include "/usr/include/getopt.h"
#else
#include "../generic/getopt.h"
#endif
