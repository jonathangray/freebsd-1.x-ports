/* 
 *	genbox : Empty box driver for fig2dev translator
 *
*/
#include <stdio.h>
#include "object.h"
#include "fig2dev.h"

void genbox_option(opt, optarg)
char opt, *optarg;
{
  	switch (opt) {

	case 's':
	case 'f':
	case 'm':
	case 'L':
		break;

 	default:
		put_msg(Err_badarg, opt, "box");
		exit(1);
	}
}

void genbox_start(objects)
F_compound	*objects;
{
	double ppi;

	if (0 == (ppi = (double)objects->nwcorner.x)) {
	    fprintf(stderr, "Resolution is zero!! default to 80 ppi\n");
	    ppi = 80.0;
	    }

	/* draw box */
        fprintf(tfp, "\\makebox[%.3fin]{\\rule{0in}{%.3fin}}\n",
		(urx-llx)*mag/ppi, (ury-lly)*mag/ppi);
	}

struct driver dev_box = {
	genbox_option,
	genbox_start,
	gendev_null,
	gendev_null,
	gendev_null,
	gendev_null,
	gendev_null,
	gendev_null,
	INCLUDE_TEXT
};
