/* framed.c - binary for the frame deamon */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_framed_c_sccsid[] = "@(#)framed.c	1.10 25/4/92 (UKC)";

#include "wn_framed.h"

main(argc, argv)
int argc;
char **argv;
{
	_wn_framed(argc, argv);
	exit(1);
}
