/* $Id: getactive.c,v 1.5 1994/02/22 01:45:58 nate Exp $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "nntpclient.h"

void finalize _((int));

int debug = 0;			/* make nntpclient.c happy */

int
main(argc, argv)
int argc;
char *argv[];
{
    char command[32];
    char *action;
    register FILE *actfp;

    if (argc < 2 || argc > 3) {
	fprintf(stderr, "Usage: getactive [active|distributions|newsgroups|subscriptions] filename\n");
	exit(1);
    }
    if (argc == 2)
	action = "ACTIVE";
    else {
	action = argv[1];
	argc--;
	argv++;
    }
    if (!nntp_connect())
	finalize(1);
    sprintf(command,"LIST %s",action);
    nntp_command(command); 
#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	fprintf(stderr,"getactive: Can't get %s file from server.\n",action);
	fprintf(stderr, "Server said: %s\n", ser_line);
	finalize(1);
    }

    actfp = fopen(argv[1], "w");
    if (actfp == NULL) {
	perror(argv[1]);
	finalize(1);
    }

    while (nntp_gets(ser_line, sizeof ser_line) >= 0) {
	if (NNTP_LIST_END(ser_line))	/* while there's another line */
	    break;			/* get it and write it to */
	if (actfp != NULL) {		/* the temporary active file */
	    fputs(ser_line, actfp);
	    putc('\n', actfp);
	}
    }

    if (ferror(actfp)) {
	perror(argv[1]);
	finalize(1);
    }
    if (fclose(actfp) == EOF) {
	perror(argv[1]);
	finalize(1);
    }

#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    nntp_close(TRUE);
    return 0;
}

/* return ptr to little string in big string, NULL if not found */

char *
instr(big, little, case_matters)
char *big, *little;
bool_int case_matters;
{
    register char *t, *s, *x;

    for (t = big; *t; t++) {
	for (x=t,s=little; *s; x++,s++) {
	    if (!*x)
		return Nullch;
	    if (case_matters == TRUE) {
		if(*s != *x)
		    break;
	    } else {
		register char c,d;
		if (isupper(*s)) 
		    c = tolower(*s);
		else
		    c = *s;
		if (isupper(*x)) 
		    d = tolower(*x);
		else
		    d = *x;
		if ( c != d )
		    break;
	   }
	}
	if (!*s)
	    return t;
    }
    return Nullch;
}

void
finalize(num)
int num;
{
    nntp_close(TRUE);
    exit(num);
}

char
nntp_handle_timeout(strict)
bool_int strict;
{
    fatal_error("\n503 Server timed out.\n");
    return NNTP_CLASS_FATAL;
}
