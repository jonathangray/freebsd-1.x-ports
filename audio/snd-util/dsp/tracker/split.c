/* split.c 
	vi:se ts=3 sw=3:
 */


/* $Id: split.c,v 1.1 1994/02/19 16:03:09 ache Exp $
 * $Log: split.c,v $
 * Revision 1.1  1994/02/19 16:03:09  ache
 * Initial revision
 *
 * Revision 4.1  1994/01/12  16:11:07  espie
 * Last minute changes.
 *
 * Revision 4.0  1994/01/11  17:56:24  espie
 * Cleaner.
 *
 * Revision 1.5  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.4  1994/01/08  03:55:43  Espie
 * run_in_fg() and create_notes_table() not needed !
 *
 * Revision 1.3  1994/01/06  22:32:42  Espie
 * Use new pref scheme.
 *
 * Revision 1.2  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 1.2  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 1.1  1993/12/02  15:45:33  espie
 * Initial revision
 *
 */
     

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#endif
     
#include "defs.h"
#include "song.h"
#include "extern.h"
     
#include "getopt.h"
     
ID("$Id: split.c,v 1.1 1994/02/19 16:03:09 ache Exp $")

/* global variable to catch various types of errors
 * and achieve the desired flow of control
 */
int error;

LOCAL struct song *do_scan_song(name, type)
char *name;
int type;
   {
   struct song *song;
   struct exfile *file;

   file = open_file(name, "r", getenv("MODPATH"));
   if (!file)
      return NULL;
   song = read_song(file, type); 
   close_file(file);
   return song;
   }

#define CHUNK_SIZE 32000

LOCAL char *suffix[] =
   {
   "lzh", "lha", "Z", "z", "shn", "zoo", 0
   };

LOCAL void truncate(name)
char *name;
   {
   int i;
   int last_point = 0;

   for (i = 0; name[i]; i++)
      {
      if (name[i] == '.')
         last_point = i + 1;
      }
   if (last_point)
      {
      for (i = 0; suffix[i]; i++)
      if (strcmp(name + last_point, suffix[i]) == 0)
         {
         name[last_point - 1] = 0;
         return;
         }
      }
   }

   
   
void split_module(name, cutpoint)
char *name;
long cutpoint;
   {
   char buffer[300];
   FILE *mod;
   FILE *samp;
   struct exfile *file;
   char *copy_buff;
   int chunk;

   file = open_file(name, "r", getenv("MODPATH"));
   truncate(name);
   sprintf(buffer, "%s.mod", name);
   mod = fopen(buffer, "w");
   if (!mod)
      exit(10);
   sprintf(buffer, "%s.samp", name);
   samp = fopen(buffer, "w");
   if (!samp)
      exit(10);
   copy_buff = malloc(CHUNK_SIZE);
   if (!copy_buff)
      exit(10);
   while(cutpoint >= CHUNK_SIZE)
      {
      fread(copy_buff, 1, CHUNK_SIZE, file_handle(file));
      fwrite(copy_buff, 1, CHUNK_SIZE, mod);
      cutpoint -= CHUNK_SIZE;
      }
   if (cutpoint > 0)
      {
      fread(copy_buff, 1, cutpoint, file_handle(file));
      fwrite(copy_buff, 1, cutpoint, mod);
      }
   fclose(mod);
   while ((chunk = fread(copy_buff, 1, CHUNK_SIZE, file_handle(file))) > 0)
      fwrite(copy_buff, 1, chunk, samp);
   fclose(samp);
   close_file(file);
   }
      
   


int main(argc, argv)
int argc;
char **argv;
   {
   struct song *song;
   int i;
   int default_type;


   default_type = BOTH;

   for (i = 1; i < argc; i++)
      {
      song = do_scan_song(argv[i], NEW);
      if (!song && error != NEXT_SONG)
         song = do_scan_song(argv[i], OLD);
      if (song)
         {
         dump_song(song); 
         split_module(argv[i], song->samples_start);
         release_song(song);
         }
      }
   }


