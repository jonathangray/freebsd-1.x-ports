/* amiga/popen.c 
	vi:se ts=3 sw=3:
 */

/* $Id: popen.c,v 1.1 1994/02/19 16:03:11 ache Exp $
 * $Log: popen.c,v $
 * Revision 1.1  1994/02/19 16:03:11  ache
 * Initial revision
 *
 * Revision 1.6  1994/01/09  17:38:28  Espie
 * Generalized open.c.
 *
 * Revision 1.5  1994/01/07  15:08:54  Espie
 * Maybe correct code now...
 *
 * Revision 1.4  1994/01/06  22:37:26  Espie
 * Fixed up some cosmetic problems.
 *
 * Revision 1.3  1994/01/05  14:55:38  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1993/12/28  14:03:53  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  22:48:18  Espie
 * Initial revision
 *
 */


#include <proto/dos.h>
#include <proto/exec.h>
#include <exec/tasks.h>
#include <dos/dostags.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"
ID("$Id: popen.c,v 1.1 1994/02/19 16:03:11 ache Exp $")

/*
###   CSupport/popen
###
###   NAME
###      popen/pclose -- Unix-like pipes
###
###   STATUS
###      Experimental
###      does not work with csh !
###
 */
FILE *popen(char *command, char *mode)
   {
   static char pname[25];
   struct Task *me = FindTask(0);
   static count = 0;
   
   count++;
   
      /* guarantees a unique pipe name ! */
   sprintf(pname, "pipe:tr_%lx_%d", me, count);
   
   if (strcmp(mode, "r") == 0)
      /* open pipe for reading */
      {
      FILE *reader;
      BPTR writer, null;

      writer = Open(pname, MODE_NEWFILE);
      reader = fopen(pname, "r");
      null = Open("NIL:", MODE_NEWFILE);
      if (SystemTags(command, SYS_Input, null, 
         SYS_Output, writer, SYS_Asynch, TRUE, 
         NP_StackSize, me->tc_SPUpper - me->tc_SPLower,
         TAG_END) == -1)
         {
         Close(null);
         Close(writer);
         fclose(reader);
         return NULL;
         }
      else
         return reader;
      }
   else if (strcmp(mode, "w") == 0)
      /* open pipe for writing */
      {
      FILE *writer;
      BPTR reader, null;
      
      writer = fopen(pname, "w");
      reader = Open(pname, MODE_OLDFILE);
      null = Open("NIL:", MODE_NEWFILE);
      if (SystemTags(command, SYS_Input, reader, 
         SYS_Output, null, SYS_Asynch, TRUE, 
         NP_StackSize, me->tc_SPUpper - me->tc_SPLower, 
         TAG_END) == -1)
         {
         Close(null);
         Close(reader);
         fclose(writer);
         return NULL;
         }
      else
         return writer;
      }
   else
      return NULL;
   }

/* for us, pclose is just fclose.
 * But we have to insure the file is empty first
 */
void pclose(FILE *f)
   {
   while (fgetc(f) != EOF)
      ;
   fclose(f);
   }

