#include <stdio.h>
#include <local/menu3.h>

static MENU MM1 = {
	0x40,
	0,
	"back",
	114,
	-1,
	-1,
	102,
	91,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM2 = {
	0x40,
	0,
	"backwards",
	98,
	45,
	0,
	153,
	99,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM3 = {
	0x40,
	0,
	"forwards",
	102,
	153,
	0,
	261,
	99,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM4 = {
	0x408,
	153,
	NULL,
	0,
	45,
	0,
	261,
	99,
	0,
	0,
	0,
	NULL,
	&MM2,
	&MM3,
	NULL,
	NULL,
};
static MENU MM5 = {
	0x260,
	7832,
	"search",
	115,
	102,
	-1,
	206,
	91,
	0,
	0,
	0,
	NULL,
	&MM4,
	NULL,
	NULL,
	NULL,
};
static MENU MM6 = {
	0x8,
	102,
	NULL,
	114,
	-1,
	-1,
	206,
	91,
	0,
	0,
	0,
	NULL,
	&MM1,
	&MM5,
	NULL,
	NULL,
};
static MENU MM7 = {
	0x40,
	0,
	"up",
	117,
	206,
	-1,
	309,
	91,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM8 = {
	0x40,
	0,
	"down",
	100,
	309,
	-1,
	413,
	91,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM9 = {
	0x8,
	309,
	NULL,
	117,
	206,
	-1,
	413,
	91,
	0,
	0,
	0,
	NULL,
	&MM7,
	&MM8,
	NULL,
	NULL,
};
MENU src_men = {
	0x408,
	206,
	NULL,
	0,
	-1,
	-1,
	413,
	91,
	0,
	-30584,
	0,
	NULL,
	&MM6,
	&MM9,
	NULL,
	NULL,
};
