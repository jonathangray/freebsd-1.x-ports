/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>

#include <stdio.h>

#include "defs.h"
#include "data.h"

extern FILE    *yyin;

/******************************************************************************
 *
 * XTrekParse()
 *
 ******************************************************************************/

extern char    *xtrekFile;
extern int      yyparse();


GLOBAL FILE    *OpenLibraryFile(name)
  char           *name;
{
  FILE           *fp;
  char            buffer[512];

  if (fp = fopen(name, "r"))
    return fp;
  sprintf(buffer, "%s/%s", LIBDIR, name);	/* so try lib */
  if (fp = fopen(buffer, "r"))
    return fp;
  fprintf(stderr, "OpenLibraryFile: can't open `%s'\n", name);
  exit(1);
  return 0;				/* in GCC could just say volatile
					   exit() */
}


GLOBAL void     XTrekParse(file_name)
  char           *file_name;
{
  xtrekFile = file_name;
  yyin = OpenLibraryFile(file_name);
  fprintf(stderr, "Reading `%s'...\n", xtrekFile);

  initdefaultship();
  /* Parse XTREK file. */
  yyparse();
}


struct planet   defaultPlanet =
{
  0,					/* int pl_no; */
  0,					/* int pl_flags; */
  0,					/* int pl_owner; */
  0,					/* int pl_orig_owner; */
  0,					/* int pl_x; */
  0,					/* int pl_y; */
  "dummy",				/* char pl_name[16]; */
  6,					/* int pl_namelen; */
  30,					/* int pl_armies; */
  0,					/* int pl_info; */
  0,					/* int pl_deadtime; */
  0,					/* int pl_couptime; */
};


/****************************************************************************
 *
 * findplanet
 *
 ****************************************************************************/

GLOBAL struct planet *findplanet(planet_name)
  char           *planet_name;
{
  int             i;

  if (!planet_name)
    return &defaultPlanet;
  for (i = 0; i < numplanets; i++)
    if (!strcmp(planets[i].pl_name, planet_name))
      return &planets[i];
  return NULL;
}


/****************************************************************************
 * addplanet
 ****************************************************************************/

GLOBAL void     addplanet(planet_name, empire_name)
  char           *planet_name;
  char           *empire_name;
{
  struct planet  *planet;
  int             empire;

  if (planet = findplanet(planet_name)) {
    fprintf(stderr, "Planet %s already defined!\n", planet_name);
    return;
  }
  if ((empire = findempire(empire_name)) < 0) {
    fprintf(stderr, "Empire %s not defined!\n", empire_name);
    exit(1);
  }
  planet = &planets[numplanets];
  bcopy(&defaultPlanet, planet, sizeof (struct planet));
  strcpy(planet->pl_name, planet_name);
  planet->pl_namelen = strlen(planet_name);
  planet->pl_no = numplanets;
  planet->pl_owner = empire;
  planet->pl_orig_owner = empire;
  planet->pl_info = FLAG(empire);
  ++numplanets;
}


GLOBAL int      addempire(name, abbrev)
  char           *name;
  char           *abbrev;
{
  int             code;
  struct empire  *emp = &empires[numempires];

  code = name[0];
  emp->code = code;
  emp->namelen = strlen(name);
  strcpy(emp->name, name);
  sprintf(emp->robotname, "%c_robot", code);
  if (!abbrev)
    *emp->abbrev = 0;
  else
    strcpy(emp->abbrev, abbrev);
  return numempires++;
}


/****************************************************************************
 *
 * findempire():
 *
 *   Given an empire name, look it up in the list of empires, and
 * return the number of the empire.
 *
 ****************************************************************************/

GLOBAL int      findempire(empire)
  char           *empire;
{
  int             i;

  if (!empire)
    return -1;

  for (i = 0; i < numempires; i++)
    if (!strcmp(empire, empires[i].name) || !strcmp(empire, empires[i].abbrev))
      return i;

  return -1;
}


GLOBAL void     ConfigGlobals()
{
  DEATHTIME *= UPS;
  PFIRETIME *= UPS;
  TFIREMIN *= UPS;
  TFIREVAR *= UPS;
  MFIREMIN *= UPS;
  MFIREVAR *= UPS;
  PEXPTIME *= UPS;
  PWEAPLOCKMIN *= UPS;
  PWEAPLOCKVAR *= UPS;
  PENGLOCKMIN *= UPS;
  PENGLOCKVAR *= UPS;
  PSELFDESTTIME *= UPS;
  RGIVEUPTIME *= UPS;
}
