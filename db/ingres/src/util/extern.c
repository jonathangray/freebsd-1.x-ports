/*-
 * Copyright (c) 1994 Westley Computing Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by Westley Computing Ltd
 * 4. Neither the name of Westley Computing Ltd nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WESTLEY COMPUTING LTD. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL WESTLEY COMPUTING LTD OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	Written by: Alistair G. Crooks, (agc@uts.amdahl.com)
 *
 *	$Id: extern.c,v 1.2 1994/06/19 05:38:40 alm Exp $
 */
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <resp.h>
#include <ingres.h>

#include "protos.h"

/* -------------- resp_t stuff is here ----------------------- */

static resp_t	r;

resp_t *
getresp(void)
{
	return(&r);
}

/* ------------- tid_t conversion stuff is here -------------- */

int
tid2int(tid_t t)
{
	int	i;

	(void) memcpy(&i, &t, sizeof(i));
	return(i);
}

void
int2tid(tid_t *tp, int i)
{
	(void) memcpy(tp, &i, sizeof(tid_t));
}

/* --------------- system info stuff is here ------------------ */

typedef struct entstr {
	char	*e_name;	/* name of variable */
	char	e_isint;	/* 1 if value is an integer */
	union {
		void	*u_ptr;		/* value of variable pointer */
		int	u_int;		/* integer value of variable */
	} e_un;
} ent_t;

static ent_t	*sysinfov;	/* system information vector */
static int	sysinfoc;	/* # of entries */

static char *
strnsave(char *s, int n)
{
	char	*cp;

	cp = xalloc(n, 0, 1);
	(void) strncpy(cp, s, n);
	cp[n] = 0;
	return(cp);
}

/* find the entry with 'name' as the name */
static ent_t *
findent(char *name)
{
	ent_t	*ep;
	int	i;

	for (i = 0, ep = sysinfov ; i < sysinfoc ; i++, ep++) {
		if (strcmp(ep->e_name, name) == 0) {
			return(ep);
		}
	}
	return((ent_t *) NULL);
}

/* grow the arrays by n elements if necessary */
static void
checksize(int n)
{
	if (sysinfoc == 0) {
		sysinfov = xalloc(n * sizeof(ent_t), 0, 1);
	} else if (sysinfoc % n == 0) {
		sysinfov = xrealloc(sysinfov, sysinfoc + (n * sizeof(ent_t)));
	}
}

/* get the value of the variable 'name' */
void *
getglobalptr(char *name)
{
	ent_t	*ep;

	if ((ep = findent(name)) == (ent_t *) NULL) {
		return((void *) NULL);
	}
	return((ep->e_isint) ? (void *) NULL : ep->e_un.u_ptr);
}

/* get the value of the variable 'name' */
int
getglobalint(char *name)
{
	ent_t	*ep;

	if ((ep = findent(name)) == (ent_t *) NULL) {
		return(0);
	}
	return((!ep->e_isint) ? 0 : ep->e_un.u_int);
}

/* set the value of the variable 'name' to 'val' */
void
setglobalptr(char *name, void *val)
{
	ent_t	*ep;

	if ((ep = findent(name)) == (ent_t *) NULL) {
		checksize(32);
		sysinfov[sysinfoc].e_name = strnsave(name, strlen(name));
		ep = &sysinfov[sysinfoc++];
	}
	ep->e_isint = 0;
	ep->e_un.u_ptr = val;
}

/* set the value of the variable 'name' to 'val' */
void
setglobalint(char *name, int val)
{
	ent_t	*ep;

	if ((ep = findent(name)) == (ent_t *) NULL) {
		checksize(32);
		sysinfov[sysinfoc].e_name = strnsave(name, strlen(name));
		ep = &sysinfov[sysinfoc++];
	}
	ep->e_isint = 1;
	ep->e_un.u_int = val;
}
