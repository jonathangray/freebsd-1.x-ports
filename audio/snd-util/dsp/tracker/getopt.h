/* getopt.h 
	vi:se ts=3 sw=3:
 */
/* $Id: getopt.h,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: getopt.h,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:48:38  espie
 * Small changes.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 1.4  1993/12/04  16:12:50  espie
 * Prototypes only there.
 *
 * Revision 1.3  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 */

struct long_option
	{
	char *fulltext;
	int argn;
	char abbrev;
	int code;
	};

/* n = getlongopt(argc, argv, options):
 * try to parse options out of argv, using
 * ways similar to standard getopt
 */
XT int getlongopt P((int argc, char *argv[], struct long_option *options));

XT int optind;

XT char *optarg;

