/* -*-C-*- getlogin.c */
/*-->getlogin*/
/**********************************************************************/
/****************************** getlogin ******************************/
/**********************************************************************/

#if    KCC_20
#include <jsys.h>
static int acs[5];
#define ac1 acs[1]
#define ac2 acs[2]
#define ac3 acs[3]
#define ac4 acs[4]
#endif /* KCC_20 */


char*
getlogin()
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

    return ((char*) username);
}
