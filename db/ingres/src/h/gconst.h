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
 *	$Id: gconst.h,v 1.2 1994/06/19 05:38:22 alm Exp $
 */
#ifndef	INGRES_GCONST_H_
#define	INGRES_GCONST_H_

#define USERCODE_SIZE	2		/* max size of an Ingres usercode */

#define MAX_STRING_LEN	255		/* max size of a string */
#define MAX_FIELD_SIZE	255		/* max size of a field in a tuple */
#define MAX_LINE_SIZE	(MAX_FIELD_SIZE + 1)	/* max size of lines in files */

#define MAX_DOMAINS	50		/* max #+ 1 of doms in a relation */

#define MAXFILENAMESIZ	14		/* maximum size of a filename */

#define MAX_NAME_SIZE	12		/* max size of a name (in bytes) */
#define MAX_VARS	10		/* max # of variables */
#define	MAX_RANGES	(MAX_VARS + 1)	/* max range */
#define MAX_2ND_KEYS	6		/* max # of keys in secondary index */
#define MAX_QRY_AGGS	50		/* max number of aggs in a qry */
#define MAX_STACK_SIZE	20		/* max depth for arith. expr. stacks */

/*
**	PGSIZE is the physical size of a page.
**	MAX_TUP_SIZE is the maximum size of a tuple, assuming only one
**		tuple on the page.  This is PGSIZE-hdrsize, where
**		hdrsize is the size of the page header (12 bytes).
**	MAX_TUPS_PER_PG is the maximum number of tuples on a page
**		assuming minimum size tuples (1 byte).  This is
**		constrained by the size of the lineid field of
**		the tid.
*/
#define	PGSIZE		1024		/* size of page */
#define	MAX_TUP_SIZE	1008		/* max size of a tuple */
#define	MAX_TUPS_PER_PG	254		/* max tuples per page */

/*
 * The following are the names of the global variables.
 * These are used in util/extern.c to access the global variables.
 */
#define BTREE_FD_NAME		"Btree fd"
#define NEED_HEADER_NAME	"Need Header Printed"
#define STATUS_NAME		"Status"

#endif /* INGRES_GCONST_H_ */
