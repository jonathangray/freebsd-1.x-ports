/* tools.c 
	vi:se ts=3 sw=3:
 */

/* standard routines for use in tracker. Used to be in main.c
 */

/* $Id: tools.c,v 1.1 1994/02/19 16:03:09 ache Exp $
 * $Log: tools.c,v $
 * Revision 1.1  1994/02/19 16:03:09  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:57:52  espie
 * Minor change.
 *
 * Revision 1.3  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.2  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */
     

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
     
#include "defs.h"
#include "extern.h"
     
ID("$Id: tools.c,v 1.1 1994/02/19 16:03:09 ache Exp $")


/* v = read_env(name, default): reads the scalar value v
 * in the environment, supplies a defaults.
 */
int read_env(name, def)
char *name;
int def;
   {
   char *var;
   int value;

   var = getenv(name);
   if (!var)
      return def;
   if (sscanf(var, "%d", &value) == 1)
      return value;
   else
      return def;
   }

