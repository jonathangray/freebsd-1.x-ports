/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#ifndef ANSI_PROTOTYPES
extern void initialise_stdio();

#else /* ANSI_PROTOTYPES */

extern void initialise_stdio(void);
#endif /* ANSI_PROTOTYPES */
