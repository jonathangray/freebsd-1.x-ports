/* query_user.c - ask the user a yes/no question */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_query_user_sccsid[] = "@(#)query_user.c	1.6 26/4/92 (UKC)";

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <local/ukcprog.h>

#include "utils.h"

int
query_user(prompt, yes, no, p_isyes)
const char *prompt, *yes, *no;
bool *p_isyes;
{
	char response[40], *nlpos, *s;
	bool yesno;

	yesno = strcmp(yes, "yes") == 0 && no != NULL && strcmp(no, "no") == 0;

	for (;;) {
		fputs(prompt, stdout);
		fflush(stdout);
		if (fgets(response, sizeof(response), stdin) == NULL) {
			if (ferror(stdin))
				errf("Read error in stdin (%m)");
			else {
				puts("^D");
				errf("Unexpected EOF in stdin");
			}
			return -1;
		}

		if ((nlpos = strchr(response, '\n')) == NULL) {
			int ch;

			if (no != NULL) {
				errf(
			"Response (\"%s ...\") too long - answer `%s'",
						       response, yes);
			}
			else {
				errf(
			"Response (\"%s ...\") too long - answer `%s' or `%s'",
						       response, yes, no);
			}

			while ((ch = getchar()) != '\n' && ch != EOF)
				;
			continue;
		}
		*nlpos = '\0';

		for (s = response; *s != '\0'; ++s) {
			if (isupper(*s))
				*s = tolower(*s);
		}

		if (strcmp(response, yes) == 0 ||
					(yesno && strcmp(response, "y") == 0)) {
			if (p_isyes != NULL)
				*p_isyes = TRUE;
			return 0;
		}
		if (no != NULL && (strcmp(response, no) == 0 ||
				      (yesno && strcmp(response, "n") == 0))) {
			if (p_isyes == NULL)
				return -1;
			*p_isyes = FALSE;
			return 0;
		}
		
		if (no != NULL)
			errf("Unexpected response \"%s\" - answer `%s' or `%s'",
							response, yes, no);
		else
			errf("Unexpected response \"%s\" - answer `%s'",
							response, yes);
	}
}
