/* Getpass.h */

#ifndef _getpass_h_
#define _getpass_h_

/*  $RCSfile: getpass.h,v $
 *  $Revision: 1.3 $
 *  $Date: 1994/04/10 22:14:43 $
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
