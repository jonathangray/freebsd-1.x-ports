#include <stdio.h>
#include <local/menu3.h>

static MENU MM1 = {
	0x40,
	393,
	"remove",
	100,
	-1,
	-1,
	278,
	40,
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
	"source",
	101,
	278,
	-1,
	557,
	40,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
};
MENU bpt_men = {
	0x408,
	278,
	NULL,
	0,
	-1,
	-1,
	557,
	40,
	27493,
	0,
	0,
	NULL,
	&MM1,
	&MM2,
	NULL,
	NULL,
};
