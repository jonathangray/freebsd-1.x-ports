/* dummy stddef.h, in case you don't have one */

#ifndef offsetof
#define offsetof(type, mem) ((char *)&((type *)NULL)->mem - (char *)NULL)
#endif
