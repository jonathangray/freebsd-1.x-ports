/* data.h - header file for data.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)data.h	1.2 15/9/92 (UKC) */

typedef union {
	char vl_char;
	unsigned char vl_uchar;
	short vl_short;
	unsigned short vl_ushort;
	int vl_int;
	unsigned int vl_uint;
	int vl_long;
	unsigned long vl_ulong;
	int vl_ints[2];	/* for illegal double values */
	float vl_float;
	double vl_double;
	int vl_logical;
	taddr_t vl_addr;
} value_t;

#ifdef IS_BIG_ENDIAN
#define DOUBLE_MSW	0
#define DOUBLE_LSW	1
#endif

#ifdef IS_LITTLE_ENDIAN
#define DOUBLE_MSW	1
#define DOUBLE_LSW	0
#endif

void dump_uarea_to_file PROTO((const char *name));
void dump_stack_to_file PROTO((const char *name));

int dgets PROTO((taddr_t addr, char *optr, int max_nbytes));
int dread PROTO((taddr_t addr, char *buf, int nbytes));
int dread_fpval PROTO((taddr_t addr, bool is_reg, bool is_double, char *buf));
int dwrite PROTO((taddr_t addr, const char *buf, int nbytes));
taddr_t regno_to_addr PROTO((int regno));
const char *get_real PROTO((value_t vl, bool want_hex, bool is_float));
