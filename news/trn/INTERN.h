/* $Id: INTERN.h,v 1.4 1994/02/22 01:42:17 nate Exp $
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

#undef EXT
#define EXT

#undef INIT
#ifdef xenix
#define INIT(x) =x
#else
#define INIT(x) = x
#endif

#define DOINIT
