/* $Id: only.h,v 1.4 1994/02/22 01:49:35 nate Exp $
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

#ifdef ONLY
    EXT char *ngtodo[NGMAX];		/* restrictions in effect */
#   ifdef SPEED_OVER_MEM
	EXT COMPEX *compextodo[NGMAX];	/* restrictions in compiled form */
#   endif
#endif

EXT int maxngtodo INIT(0);			/*  0 => no restrictions */
					/* >0 => # of entries in ngtodo */
EXT bool emptyOnly INIT(FALSE);

void	only_init _((void));
bool	inlist _((char*));	/* return TRUE if ngname is in command line list */
			/* or if there was no list */
void	setngtodo _((char*));
#ifdef ONLY
void	end_only _((void));
#endif
