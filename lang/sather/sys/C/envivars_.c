/*  -*- Mode: C;  -*-
 * File: envivars_.c
 * Author: Hiroshi HARADA
 * Copyright (C) Software Research Assosiates, Inc. Tokyo Japan.
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 *
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 *          Oscar Bosman (oscar@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ** FUNCTION: putenv() and setenv() using gc_malloc().
 **           The system provided ones implicitly use malloc and 
 **           can loose with GC_ on. This is also a pot of
 **           compatibility stuff, to be moved elsewhere later.
 **
 ** RELATED PACKAGES: compiler calls this.
 **
 ** HISTORY:
 ** Last edited: Oct 24 19:23 1993 (hws)
 **  Oct 24 19:13 1993 (hws): document solaris changes integrated earlier
 **  Sep  1 15:31 1993 (oscar): replace strdup with one that uses GC
 **  May 28 02:10 1993 (hws): adapt to GC 2.6
 **  May  4 15:56 1993 (hws): ANSI C changes putenv_, setenv_
 **  Mar  2 01:17 1992 (hws): adapted to GC V1.9
 **  Feb 15 11:46 1992 (hws): merged with other compile time 
 **                           conditionalization.
 ** Created: Mon Aug 5 1991
 **~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


#include "all_.h"

#include <stdio.h>
#include <sys/param.h>

#ifdef sony_news
# include <sys/vmparam.h>
#endif

#ifdef hpux7
#  include <string.h>
#else
#  include <strings.h>
#endif

#ifdef sequent
#  include <stdio.h>
#  include <sys/ioctl.h>
#  include <pwd.h>
#  include <varargs.h>
#  include <ctype.h>
#  include <string.h>
#endif

#if defined(GC_) || defined(NeXT)
#  define ENVISETENV setenv_
#  define ENVIPUTENV putenv_
#else
#  define ENVISETENV setenv
#  define ENVIPUTENV putenv
#endif 

#if defined(__svr4__)
char *index(s,c)
     char *s;
     char c;
{
  char *res;
  char d;
  while (1) {
    d=(*res);
    if (d==c) break;
    if (d==(char)NULL) {
      res=(char *)NULL;
      break;
      };
    res++;
  };
  return(res);
}
#endif

int
putenv_(env)
     char  *env;
{
    char *name;
    char *value;

    name = strcpy((char *)malloc(strlen(env)+1),env);
    if((value = (char *)index(name,'=')) == (char *)NULL)
        return(1);     /* No Identifier */

    *value=NULL;
    value++;
    return( *value ? ENVISETENV(name,value,1) : 1 );  /* if *value is NULL return error*/
}
    
int
setenv_(name,value,ov_write)
     char  *name;
     char  *value;
     int   ov_write;
{
    char  *env;
    char  **P;
    int   env_cnt=0;
    int   name_len=0;
    char  **new_env;
    extern char **environ;

    if( !(name_len = strlen(name)) )
        return(1);

    for(P=environ; *P ; P++,env_cnt++)
    {
	if( ((int)index(*P,'=') - (int)*P)== (name_len+1) && strncmp(name,*P,name_len)==0 )
	    break;
    }

    if(*P)
    {
	if(ov_write)
	{
	    if( strlen(*P) < strlen(name)+strlen(value)+2)
	    {
	      *P = (char *)malloc(strlen(name)+strlen(value)+2);
	    }
	    strcat(strcat(strcpy(*P,name),"="),value);
	    return(0);
	}
	else
	    return(0);
    }
    new_env = (char  **)malloc(sizeof(char *) * (env_cnt+2));
    bcopy(environ,new_env,sizeof(char *) * env_cnt);
    environ = new_env;
    environ[env_cnt] = (char *)malloc(strlen(name)+strlen(value)+2);
    strcat(strcat(strcpy(environ[env_cnt],name),"="),value);
    environ[env_cnt+1] = NULL;
    return(0);
}


/* For testing above....
 * void printenv()
 *{
 *    extern char **environ;
 *    register char **P;
 *
 *    fprintf(stderr,"****** PRINTENV ******\n");
 *    for(P = environ; *P ; P++)
 *	fprintf(stderr,"            [%s]\n",*P);
 *} 
 */

/*
 * We cannot rely on availability of Fortran getcwd. getwd is the C library
 * function available everywhere. However we need to take care not to malloc
 * in the presence of the GC!
 */

int getcwd_()
{
  int err = 0;
  char *str = (char *)malloc(MAXPATHLEN);
  str[0] = 0;
#if defined(hpux) || defined(__svr4__)
  err = getcwd(str,MAXPATHLEN);  
#else
  err = getwd(str);
#endif
  if ( err == 0 ) {
    return((int)str);
  } else
    { 
      return((int)err);
    };
}

/*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
/* The rest should perhaps go into some other file as it grows.
** I have been adding more and more with ports failing (hws).
*/

/* Replace strdup with one that uses the GC */
char *strdup_(s)
char *s;
{
    char *res = 0;
    if (res = (char *)malloc(strlen(s) +1))  strcpy(res, s);
    return res;
}


#if defined(sequent) || defined(NeXT)

double fmod(x,y)
     double x, y;
{ return ((double) 0);
}

#endif


