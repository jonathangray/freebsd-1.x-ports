/*	Displaying messages and getting input for Lynx Browser
**	==========================================================
**
**	REPLACE THIS MODULE with a GUI version in a GUI environment!
**
** History:
**	   Jun 92 Created May 1992 By C.T. Barker
**	   Feb 93 Simplified, portablised TBL
**
*/


#include "HTAlert.h"
#include "LYStrings.h"
#include "LYUtils.h"
#include "LYSignal.h"
#include "GridText.h"
#include "LYGlobalDefs.h"

#include <ctype.h> 		/* for toupper - should be in tcp.h */


PUBLIC void HTAlert ARGS1(CONST char *, Msg)
{
    user_message("Alert!:  %s\n", (char *)Msg);
    if(user_mode == ADVANCED_MODE)
        sleep(1);
    else
        sleep(2);
}


PUBLIC void HTProgress ARGS1(CONST char *, Msg)
{
    if(!TRACE)
        statusline((char *)Msg);
    else
	fputs((char *)Msg,stderr);
}


PUBLIC BOOL HTConfirm ARGS1(CONST char *, Msg)
{
  char c;
#ifdef VMS
  extern BOOLEAN HadVMSInterrupt;
#endif /* VMS */

  user_message("WWW: %s (y/n) ", (char *) Msg);

  while(1) {
     c = LYgetch();
#ifdef VMS
     if(HadVMSInterrupt) {
         HadVMSInterrupt = FALSE;
         c = 'N';
     }
#endif /* VMS */
     if(toupper(c)=='Y')
        return(YES);
     if(toupper(c)=='N')
        return(NO);
  }
}

/*	Prompt for answer and get text back
*/
PUBLIC char * HTPrompt ARGS2(CONST char *, Msg, CONST char *, deflt)
{
    char * rep = 0;
    char Tmp[200];

    Tmp[0]='\0';

    statusline((char *)Msg);
    if (deflt) 
        strcpy(Tmp, deflt);

    LYgetstr(Tmp, VISIBLE);

    StrAllocCopy(rep, Tmp);

    return rep;
}

/*      Prompt for password without echoing the reply
*/
PUBLIC char * HTPromptPassword ARGS1(CONST char *, Msg)
{
    char *result = NULL;
    char pw[120];

    pw[0]='\0';

    statusline(Msg ? (char *)Msg : "Password: ");
    LYgetstr(pw, HIDDEN); /* hidden */

    StrAllocCopy(result, pw);
    return result;
}


/*      Prompt both username and password       HTPromptUsernameAndPassword()
**      ---------------------------------
** On entry,
**      Msg             is the prompting message.
**      *username and
**      *password       are char pointers; they are changed
**                      to point to result strings.
**
**                      If *username is not NULL, it is taken
**                      to point to  a default value.
**                      Initial value of *password is
**                      completely discarded.
**
** On exit,
**      *username and *password point to newly allocated
**      strings -- original strings pointed to by them
**      are NOT freed.
**
*/
PUBLIC void HTPromptUsernameAndPassword ARGS3(CONST char *,     Msg,
                                              char **,          username,
                                              char **,          password)
{
    if (Msg) {
        *username = HTPrompt(Msg, *username);
    } else
        *username = HTPrompt("Username: ", *username);
    *password = HTPromptPassword("Password: ");
}

