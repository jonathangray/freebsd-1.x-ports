/* $Id: artcheck.c,v 1.4 1994/02/22 01:43:35 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

/* A program to check an article's validity and print warnings if problems
** are found.
**
** Usage: artcheck <article> <maxLineLen> <newsgroupsFile> <activeFile>
*/

#include "EXTERN.h"
#include "common.h"
#include "config.h"

#define MAXNGS 100

int
main(argc, argv)
int argc;
char *argv[];
{
    FILE *fp, *fp_active = NULL, *fp_ng = NULL;
    char buff[LBUFLEN], *cp, *cp2;
    char *ngptrs[MAXNGS];
    int nglens[MAXNGS];
    int foundactive[MAXNGS];
    int i, col, max_col_len, line_num = 0, ngcnt = 0, ngleft;
    int found_newsgroups = 0;

    if (argc != 5 || !(max_col_len = atoi(argv[2]))) {
	fprintf(stderr, "\
Usage: artcheck <article> <maxLineLen> <newsgroupsFile> <activeFile>\n");
	exit(1);
    }

    if ((fp = fopen(argv[1], "r")) == NULL) {
	fprintf(stderr, "artcheck: unable to open article `%s'.\n", argv[1]);
	exit(1);
    }

    /* Check the header for proper format and report on the newsgroups */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	buff[strlen(buff)-1] = '\0';
	if (!*buff)
	    break;
	if (*buff == ' ' || *buff == '\t')
	    continue;
	if (!(cp = index(buff, ':'))) {
	    printf("\nERROR: line %d is an invalid header line:\n%s\n",
		   line_num, buff);
	    break;
	}
	if (cp[1] != ' ' && cp[1] != '\0') {
	    printf("\n\
ERROR: header on line %d does not have a space after the colon:\n%s\n",
		   line_num, buff);
	}
	if (cp - buff == 10 && strnEQ(buff, "Newsgroups", 10)) {
	    found_newsgroups = 1;
	    for (cp = buff + 11; *cp == ' '; cp++)
		;
	    if (index(cp, ' ')) {
		printf("\n\
ERROR: the \"Newsgroups:\" line has spaces in it that MUST be removed. The\n\
only allowable space is the one separating the colon (:) from the contents.\n\
Use a comma (,) to separate multiple newsgroup names.\n");
		continue;
	    }
	    while (*cp) {
		if (!(cp2 = index(cp, ',')))
		    cp2 = cp + strlen(cp);
		else
		    *cp2++ = '\0';
		if (ngcnt < MAXNGS) {
		    nglens[ngcnt] = strlen(cp);
		    foundactive[ngcnt] = 0;
		    ngptrs[ngcnt] = malloc(nglens[ngcnt]+1);
		    if (!ngptrs[ngcnt]) {
			fprintf(stderr,"Out of memory.\n");
			exit(1);
		    }
		    strcpy(ngptrs[ngcnt], cp);
		    ngcnt++;
		}
		cp = cp2;
	    }
	    if (!ngcnt) {
		printf("\n\
ERROR: the \"Newsgroups:\" line lists no newsgroups.\n");
		continue;
	    }
	}
    }
    if (!found_newsgroups) {
	printf("\nERROR: the \"Newsgroups:\" line is missing from the header.\n");
    }

    /* Check the body of the article for long lines */
    while (fgets(buff, LBUFLEN, fp)) {
	line_num++;
	buff[strlen(buff)-1] = '\0';
	col = 0;
	for (cp = buff; *cp; cp++) {
	    if (*cp == '\t')
		col += 8 - (col%8);
	    else
		col++;
	}
	if (col > max_col_len) {
	    printf("\n\
Warning: posting exceeds %d columns.  Line %d is the first long one:\n%s\n",
		   max_col_len, line_num, buff);
	    break;
	}
    }
    if (ngcnt) {
	struct stat st;
	if (stat(argv[3], &st) != -1 && st.st_size > 0)
	    fp_ng = fopen(argv[3], "r");
	if (stat(argv[4], &st) != -1 && st.st_size > 0)
	    fp_active = fopen(argv[4], "r");
    }
    if (ngcnt && (fp_ng != NULL || fp_active != NULL)) {
	/* Print a note about each newsgroup */
	printf("\nYour article's newsgroup%s:\n", ngcnt == 1? "" : "s");
	if (fp_active == NULL) {
	    for (i = 0; i < ngcnt; i++) {
		foundactive[i] = 1;
	    }
	} else {
	    ngleft = ngcnt;
	    while (fgets(buff, LBUFLEN, fp_active)) {
		if (!ngleft)
		    break;
		for (i = 0; i < ngcnt; i++) {
		    if (!foundactive[i]) {
			if ((buff[nglens[i]] == '\t' || buff[nglens[i]] == ' ')
			  && strnEQ(ngptrs[i], buff, nglens[i])) {
			    foundactive[i] = 1;
			    ngleft--;
			}
		    }
		}
	    }
	    fclose(fp_active);
	}
	if (fp_ng != NULL) {
	    ngleft = ngcnt;
	    while (fgets(buff, LBUFLEN, fp_ng)) {
		if (!ngleft)
		    break;
		for (i = 0; i < ngcnt; i++) {
		    if (foundactive[i] && ngptrs[i]) {
			if ((buff[nglens[i]] == '\t' || buff[nglens[i]] == ' ')
			  && strnEQ(ngptrs[i], buff, nglens[i])) {
			    cp = &buff[nglens[i]];
			    *cp++ = '\0';
			    while (*cp == ' ' || *cp == '\t')
				cp++;
			    if (cp[0] == '?' && cp[1] == '?')
				cp = "[no description available]\n";
			    printf("%-23s %s", buff, cp);
			    free(ngptrs[i]);
			    ngptrs[i] = 0;
			    ngleft--;
			}
		    }
		}
	    }
	    fclose(fp_ng);
	}
	for (i = 0; i < ngcnt; i++) {
	    if (!foundactive[i]) {
		printf("%-23s ** invalid news group -- check spelling **\n",
		   ngptrs[i]);
		free(ngptrs[i]);
	    } else if (ngptrs[i]) {
		printf("%-23s [no description available]\n", ngptrs[i]);
		free(ngptrs[i]);
	    }
	}
    }
    return 0;
}
