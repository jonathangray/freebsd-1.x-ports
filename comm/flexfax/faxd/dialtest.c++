/*	$Header: /a/cvs/386BSD/ports/comm/flexfax/faxd/dialtest.c++,v 1.1 1993/08/31 23:42:54 ljo Exp $
/*
 * Copyright (c) 1990, 1991, 1992, 1993 Sam Leffler
 * Copyright (c) 1991, 1992, 1993 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */
/*
 * Program for interactively debugging dial string rules.
 *
 * Usage: dialtest dial-rules-file
 */
#include "DialRules.h"

static int
prompt()
{
    printf("ready> "); fflush(stdout);
    return (1);
}

int
main(int argc, char* argv[])
{
    if (argc != 2) {
	fprintf(stderr, "usage: %s dialrules\n", argv[0]);
	exit(-1);
    }
    DialStringRules rules(argv[1]);
    rules.setVerbose(TRUE);
    rules.def("AreaCode", "415");
    rules.def("CountryCode", "1");
    rules.def("InternationalPrefix", "011");
    rules.def("LongDistancePrefix", "1");
    if (rules.parse()) {
	char line[1024];
	while (prompt() && fgets(line, sizeof (line), stdin)) {
	    char* cp = strchr(line, '\n');
	    if (cp)
		*cp = '\0';
	    if (cp = strchr(line, '(')) {
		char* ep = strchr(cp, ')');
		if (ep)
		    *ep = '\0';
		fxStr set(line, cp-line);
		fxStr result = rules.applyRules(set, cp+1);
		printf("%s(%s) = \"%s\"\n", (char*) set, cp+1, (char*) result);
	    } else {
		fxStr c = rules.canonicalNumber(line);
		fxStr d = rules.dialString(line);
		printf("canonical = \"%s\" dial-string = \"%s\"\n",
		    (char*) c, (char*) d);
	    }
	}
    }
    return (0);
}
