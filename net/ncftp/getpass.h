/* Getpass.h */

#ifndef _getpass_h_
#define _getpass_h_

/*  $RCSfile: getpass.h,v $
 *  $Revision: 1.4 $
 *  $Date: 1994/06/01 22:20:19 $
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
