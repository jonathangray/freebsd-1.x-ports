/* -*-C-*- cuseri.c */
/*-->cuserid*/
/**********************************************************************/
/****************************** cuserid ******************************/
/**********************************************************************/

#include <stdio.h>

#ifdef PCC_20
#define KCC_20 0
#else
#define PCC_20 0
#endif

#if    PCC_20
#undef tops20				/* to avoid macro substitution */
#include <tops20.h>			/* gets jsys.h and byteptr.h; */
#define tops20 1
#endif

#if    KCC_20
#include <jsys.h>
#ifndef ac1
static int acs[5];
#define ac1 acs[1]
#define ac2 acs[2]
#define ac3 acs[3]
#define ac4 acs[4]
#endif /* ac1 */
#endif /* KCC_20 */

char*
cuserid(s)
char* s;
{
    static char username[40];

    username[0] = '\0';

#if    KCC_20
    (void)jsys(GJINF,acs);
    ac2 = ac1;				/* user number */
    ac1 = (int)(username-1);		/* Monitor wants ILDB */
					/* pointer, not LDB pointer*/
    (void)jsys(DIRST,acs);
#endif /* KCC_20 */

#if    PCC_20
    (void)jsys(JSgjinf,acs);
    ac2 = ac1;				/* user number */
    ac1 = POINT(username);
    (void)jsys(JSdirst,acs);
#endif /* PCC_20 */

    if (s != (char*)NULL)
        strcpy(s,username);
    return ((char*) username);
}
