/* getopt.c 
	vi:se ts=3 sw=3:
 */

/* $Id: getopt.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: getopt.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:48:21  espie
 * Small changes.
 *
 * Revision 1.2  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 1.5  1993/12/04  16:12:50  espie
 * New getopt semantics.
 *
 * Revision 1.4  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 1.3  1993/01/26  14:10:38  espie
 * Fixed up stupdi end of file bug.
 *
 * Revision 1.2  1992/11/27  10:29:00  espie
 * General cleanup
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "getopt.h"

ID("$Id: getopt.c,v 1.1 1994/02/19 16:03:08 ache Exp $")
int optind = 1;
char *optarg = 0;
LOCAL not_an_option = 0;

LOCAL int parse_option(argv, option)
char *argv[];
struct long_option *option;
	{
	optind++;
	if (option->argn)
		optarg = argv[optind++];
	return option->code;
	}

int getlongopt(argc, argv, options)
int argc;
char *argv[];
struct long_option *options;
	{
	if (not_an_option == optind)
		return -1;
	if (optind >= argc)
		return -1;
	if (argv[optind][0] == '-')
		{
		char *match = argv[optind]+1;
		if (strlen(match) == 1)
			{
			if (match[0] == '-')
				{
				not_an_option = ++optind;
				return -1;
				}
			while(options->fulltext)
				{
				if (options->abbrev == match[0])
					return parse_option(argv, options);
				else
					options++;
				}
			return -1;
			}
		else
			{
			int max_match = 0;
			struct long_option *best = 0;

			while (options->fulltext)
				{
				int i;
				for (i = 0; ; i++)
					{
					if (options->fulltext[i] == 0 && match[i] == 0)
						return parse_option(argv, options);
					if (match[i] == 0)
						{
						if (i > max_match)
							{
							max_match = i;
							best = options;
							}
						break;
						}
					if (tolower(options->fulltext[i]) != tolower(match[i]))
						break;
					}
				options++;
				}
			if (max_match < 3)
				{
				fprintf(stderr, "Unrecognized option: %s\n", match);
				return -1;
				}
			return parse_option(argv, best);
			}
		}
	else
		return -1;
	}
