/********************************************************************
 * lindner
 * 3.3
 * 1993/07/27 05:27:52
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/kernutils.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: kernutils.c
 * Routines to implement kernel specific stuff
 *********************************************************************
 * Revision History:
 * kernutils.c,v
 * Revision 3.3  1993/07/27  05:27:52  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.2  1993/06/11  16:52:33  lindner
 * Added VMS load code
 *
 * Revision 3.1.1.1  1993/02/11  18:02:56  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/21  20:46:12  lindner
 * Initial revision
 *
 *
 *********************************************************************/


#include "gopherd.h"
#include "Debug.h"

#define LOADSYMBOL "_avenrun"    /* should work with most Unix flavors */

#define WHICHLOAD  2             /* 0 ==  1 min average           */
                                 /* 1 ==  5 min average           */
                                 /* 2 == 15 min average           */

#if !defined(LOADRESTRICT)

double maxload = 0;

int LoadTooHigh() 
{
  return(0);
}

#else /* LOADRESTRICT */

int LoadTooHigh()
{
  int status;

  status = getload();
  Debug("getload returns %d\n",status);
  return(status);

}

#ifdef VMS
#  ifndef MAXLOAD
#    define MAXLOAD 10.0
#  endif
float maxload = MAXLOAD;
float sysload = 0.0;

int getload()
{
    /* In VMS with Multinet, we'll assign a channel to $$VMS_LOAD_AVERAGE,
	and read in 36 bytes into the structure _averages.		    */
  struct {
	    float   load_avg[3];    /*	1 min, 5 min, 15 min	*/
	    float   prio_avg[3];    /*	1 min, 5 min, 15 min	*/
	    float   queue_avg[3];   /*	1 min, 5 min, 15 min	*/
	 }  _averages;

  FILE	*lav;

  if ((lav=fopen("$$VMS_LOAD_AVERAGE","r"))==NULL) {
       Debug("Cannot access load averages...",0);
       return(0);
  }
  if (fread((char *)&_averages,sizeof(_averages),1,lav)==0) {
       Debug("Cannot access load averages...",0);
       fclose(lav);
       return(0);
  }
  if((sysload = _averages.load_avg[WHICHLOAD]) > maxload) {
       Debug("System maxload exceeded (currently %f)\n",sysload);
       fclose(lav);
       return(1);
  }
  fclose(lav);
  return(0);
}
#else
#  ifndef MAXLOAD
#    define MAXLOAD 10.0
#  endif
double atof();
double maxload = MAXLOAD;
double sysload = 0.0;
#include <nlist.h>
#include <kvm.h>
#define X_AVENRUN 0
long avenrun[3];
kvm_t * kd;
struct nlist nl[] = { {LOADSYMBOL}, {""}, };

int getload()
{
  if ((kd = kvm_open(NULL, NULL, NULL, O_RDONLY, NULL)) == NULL) 
    return(-1);
  if (kvm_nlist(kd, nl) != 0) 
    return(-1);
  if(nl[X_AVENRUN].n_type == 0) 
    return(-1);
  if(kvm_read(kd,nl[X_AVENRUN].n_value,avenrun,sizeof(avenrun)) 
     != sizeof(avenrun)) 
    return(-1);
  if((sysload = (((double) avenrun[WHICHLOAD]) / FSCALE)) > maxload) {
       Debug("System maxload exceeded (currently %f)\n",sysload);
       return(1);
  }
  return(0);
}

#endif /* VMS */
#endif /* LOADRESTRICT */     
