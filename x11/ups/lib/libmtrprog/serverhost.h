/* serverhost.h - header file for serverhost.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)serverhost.h	1.2 4/7/91 (UKC) */

struct serverhostent {
	const char *sh_servname;	/* name of the service (arbitrary) */
	const char **sh_hosts;		/* NULL terminated list of hosts */
};

typedef enum { SH_CLOSED, SH_OPEN, SH_EOF, SH_ERROR } serverhost_status_t;

serverhost_status_t getserverhoststatus PROTO((void));
int setserverhostent PROTO((void));
struct serverhostent *getserverhostent PROTO((void));
struct serverhostent *getserverhostbyservname PROTO((const char *servname));
void endserverhostent PROTO((void));
