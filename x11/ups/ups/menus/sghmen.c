#include <stdio.h>
#include <menu3.h>

static MENU MM1 = {
	0x40,
	0,
	"expand",
	97,
	21,
	186,
	322,
	234,
	-30584,
	-30584,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
static MENU MM2 = {
	0x40,
	-30584,
	"collapse",
	99,
	322,
	186,
	622,
	234,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
MENU sgh_men = {
	0x408,
	322,
	NULL,
	0,
	21,
	186,
	622,
	234,
	0,
	0,
	0,
	NULL,
	&MM1,
	&MM2,
	NULL,
	NULL,
};
