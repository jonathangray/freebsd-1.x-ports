/* $Id: last.h,v 1.4 1994/02/22 01:47:09 nate Exp $
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

EXT char *lastngname INIT(Nullch);	/* last newsgroup read, from .rnlast file */
EXT time_t lasttime INIT(0);	/* time last rn was started up */
EXT long lastactsiz INIT(0);	/* size of active file when rn last started up */
EXT time_t lastnewtime INIT(0);	/* time last new group was found */
EXT long lastnewsize INIT(0);	/* size of active.times file when rn last started up */

void	last_init _((char*));
void    writelast _((void));
