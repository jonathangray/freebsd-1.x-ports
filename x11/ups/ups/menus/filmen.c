#include <stdio.h>
#include <local/menu3.h>

static MENU MM1 = {
	0x40,
	0,
	"expand",
	97,
	-1,
	-1,
	134,
	41,
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
	3320,
	"collapse",
	99,
	134,
	-1,
	270,
	41,
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
	0x8,
	134,
	NULL,
	97,
	-1,
	-1,
	270,
	41,
	0,
	0,
	0,
	NULL,
	&MM1,
	&MM2,
	NULL,
	NULL,
};
static MENU MM4 = {
	0x40,
	0,
	"add expr",
	120,
	270,
	-1,
	406,
	41,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM5 = {
	0x40,
	0,
	"source",
	101,
	406,
	-1,
	542,
	41,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM6 = {
	0x8,
	406,
	NULL,
	120,
	270,
	-1,
	542,
	41,
	0,
	0,
	0,
	NULL,
	&MM4,
	&MM5,
	NULL,
	NULL,
};
MENU fil_men = {
	0x408,
	270,
	NULL,
	0,
	-1,
	-1,
	542,
	41,
	0,
	-30584,
	0,
	NULL,
	&MM3,
	&MM6,
	NULL,
	NULL,
};
