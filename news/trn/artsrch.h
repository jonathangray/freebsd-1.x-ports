/* $Id: artsrch.h,v 1.2 1993/07/26 19:12:03 nate Exp $
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

#ifndef NBRA
#include "search.h"
#endif

#ifdef ARTSEARCH

#define SRCH_ABORT 0
#define SRCH_INTR 1
#define SRCH_FOUND 2
#define SRCH_NOTFOUND 3
#define SRCH_DONE 4
#define SRCH_SUBJDONE 5
#define SRCH_ERROR 6
#endif

EXT char *lastpat INIT(nullstr);	/* last search pattern */
#ifdef ARTSEARCH
EXT COMPEX sub_compex;		/* last compiled subject search */
EXT COMPEX art_compex;		/* last compiled normal search */
#   ifdef CONDSUB
EXT COMPEX *bra_compex INIT(&(art_compex));
					/* current compex with brackets */
#   endif

#define ARTSCOPE_SUBJECT	0
#define ARTSCOPE_FROM		1
#define ARTSCOPE_ONEHDR		2
#define ARTSCOPE_HEAD		3
#define ARTSCOPE_ARTICLE	4

EXT char scopestr[] INIT("sfHha");
EXT char art_howmuch;		/* search scope */
EXT char *art_srchhdr;		/* specific header to search or NULL */
EXT bool art_doread;		/* search read articles? */
#endif

void	artsrch_init _((void));
#ifdef ARTSEARCH
int	art_search _((char*, int, int));
bool	wanted _((COMPEX*, ART_NUM, char_int));
			/* return TRUE if current article matches pattern */
#endif
