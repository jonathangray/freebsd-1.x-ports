/* Getpass.h */

#ifndef _getpass_h_
#define _getpass_h_

/*  $RCSfile: getpass.h,v $
 *  $Revision: 1.1 $
 *  $Date: 1994/03/01 00:31:49 $
 */

#define kMaxPassLen 127

#ifdef GETPASS
extern char *getpass();	/* Use the system supplied getpass. */
#else
char *Getpass(char *prompt);
#endif

void Echo(FILE *fp, int on);

#endif	/* _getpass_h_ */

/* eof Getpass.h */
