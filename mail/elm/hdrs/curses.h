
/* $Id: curses.h,v 1.3 1993/10/09 19:37:20 smace Exp $ */

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 1.3 $   $State: Exp $
 *
 * 			Copyright (c) 1988-1992 USENET Community Trust
 * 			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein, Elm Coordinator
 *	elm@DSI.COM			dsinc!elm
 *
 *******************************************************************************
 * $Log: curses.h,v $
 * Revision 1.3  1993/10/09 19:37:20  smace
 * Update to elm 2.4 pl23 release version
 *
 * Revision 5.1  1992/10/03  22:34:39  syd
 * Initial checkin as of 2.4 Release at PL0
 *
 *
 ******************************************************************************/

     /*** Include file for seperate compilation.  ***/

#define OFF		0
#define ON 		1

int  InitScreen(),      /* This must be called before anything else!! */

     ClearScreen(), 	 CleartoEOLN(),

     MoveCursor(),

     StartBold(),        EndBold(), 
     StartUnderline(),   EndUnderline(), 
     StartHalfbright(),  EndHalfbright(),
     StartInverse(),     EndInverse(),
	
     transmit_functions(),

     Raw(),              RawState(),
     ReadCh();

char *return_value_of();
