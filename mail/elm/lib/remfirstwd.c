static char rcsid[] = "@(#)$Id: remfirstwd.c,v 1.3 1993/10/09 19:38:45 smace Exp $";

/*******************************************************************************
 *  The Elm Mail System  -  $Revision: 1.3 $   $State: Exp $
 *
 *			Copyright (c) 1988-1992 USENET Community Trust
 *			Copyright (c) 1986,1987 Dave Taylor
 *******************************************************************************
 * Bug reports, patches, comments, suggestions should be sent to:
 *
 *	Syd Weinstein, Elm Coordinator
 *	elm@DSI.COM			dsinc!elm
 *
 *******************************************************************************
 * $Log: remfirstwd.c,v $
 * Revision 1.3  1993/10/09 19:38:45  smace
 * Update to elm 2.4 pl23 release version
 *
 * Revision 5.1  1992/10/03  22:41:36  syd
 * Initial checkin as of 2.4 Release at PL0
 *
 *
 ******************************************************************************/

/** 

**/

#include "headers.h"


remove_first_word(string)
char *string;
{	/** removes first word of string, ie up to first non-white space
	    following a white space! **/

	register int loc;

	for (loc = 0; string[loc] != ' ' && string[loc] != '\0'; loc++) 
	    ;

	while (string[loc] == ' ' || string[loc] == '\t')
	  loc++;
	
	move_left(string, loc);
}

remove_header_keyword(string)
char *string;
{	/** removes a RFC822 header keyword from the string.
	    i.e. removes up to (and including) the first colon,
	    plus any white-space immediately following it.  **/

	register int loc;

	for (loc = 0; string[loc] != ':' && string[loc] != '\0'; loc++) 
	    ;

	if (string[loc] == ':') {
	    loc++; /* move beyond the colon */
	    while (string[loc] == ' ' || string[loc] == '\t')
		loc++;
	}
	
	move_left(string, loc);
}
