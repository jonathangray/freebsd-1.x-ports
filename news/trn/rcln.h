/* $Id: rcln.h,v 1.3 1993/11/17 23:03:40 nate Exp $
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

EXT ART_NUM *ngmax INIT(NULL);

void    rcln_init _((void));
#ifdef CATCHUP
void	catch_up _((NG_NUM));
#endif
int	addartnum _((ART_NUM,char*));
#ifdef MCHASE
void	subartnum _((ART_NUM,char*));
#endif
void	prange _((char*,ART_NUM,ART_NUM));
void	set_toread _((NG_NUM));
void	checkexpired _((NG_NUM));
