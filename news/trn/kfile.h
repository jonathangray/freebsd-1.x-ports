/* $Id: kfile.h,v 1.4 1994/02/22 01:46:59 nate Exp $
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

#define KF_GLOBAL 0
#define KF_LOCAL 1

#ifdef KILLFILES
EXT FILE *globkfp INIT(Nullfp);		/* global article killer file */
EXT FILE *localkfp INIT(Nullfp);	/* local (for this newsgroup) file */
EXT int localkf_changes;		/* do we need to write changes? */
EXT bool has_normal_kills;		/* flag when KILLs needs rereading */
EXT ART_NUM killfirst;			/* used as firstart when killing */
#endif

void	kfile_init _((void));
int	do_kfile _((FILE*,int));
void	kill_unwanted _((ART_NUM,char*,int));
int	edit_kfile _((void));
void	open_kfile _((int));
void    kf_append _((char*));
void	rewrite_kfile _((ART_NUM));
