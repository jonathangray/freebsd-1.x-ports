#ifndef _ERROR_H_
#define _ERROR_H_

#include "copyright.h"

/*
 *  Dependencies on other include files:
 */

/*
 *  Constants and macros:
 */

/*
 *  Type declarations:
 */

/*
 *  Function prototypes:
 */

#if NeedFunctionPrototypes
void HandleXPMError(Display *display, int ErrorStatus, char *tag);
void ErrorMessage(char *message);
void WarningMessage(char *message);
void NormalMessage(char *message);
#else
void NormalMessage();
void WarningMessage();
void HandleXPMError();
void ErrorMessage();
#endif

#endif
