#include "config.h"
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
int stat();
char *path_segment();

bool
findfile(path,n,fontmag,name)
char path[STRSIZE];  /* PIXEL path */
char n[STRSIZE];     /* name of font */
long fontmag;        /* magnification */
char name[STRSIZE];  /* full name of PXL file  (returned) */
{
    char local_path[STRSIZE];
    char *pathpt;
    struct stat s;
    int rc = -1;
    int resolution, i;

    resolution = (int)(fontmag/5.0 +0.5) ;

    for(i=0; (pathpt=path_segment((bool)(i==0),path,local_path))!=NULL;i++) {
#ifdef USEPXL
       sprintf(name,"%s/dpi%d/%s.pk",pathpt,resolution,n);
       if ((rc = stat(name,&s))!=0) {
           sprintf(name,"%s/dpi%d/%s.pxl",pathpt,resolution,n);
           if ((rc = stat(name,&s))!=0) {
               sprintf(name,"%s/pxl%ld/%s.pk",pathpt,fontmag,n);
               if ((rc = stat(name,&s))!=0) {
                   sprintf(name,"%s/pxl%ld/%s.pxl",pathpt,fontmag,n);
                   if ((rc = stat(name,&s))!=0) {
#ifndef MSDOS
                      sprintf(name,"%s/%s.%dpk",pathpt,n,resolution);
                      if ((rc = stat(name,&s))!=0) {
                         sprintf(name,"%s/%s.%dpxl",pathpt,n,resolution);
                         if ((rc = stat(name,&s))!=0) {
#endif
                            sprintf(name,"%s/%s.%d",pathpt,n,resolution);
                            rc = stat(name,&s);
#ifndef MSDOS
                         }
                      }
#endif
                   }
               }
           }
       }
#else
       sprintf(name,"%s/%s.%dgf",pathpt,n,resolution);
       if ((rc = stat(name,&s))!=0) {
           sprintf(name,"%s/%s.%ldgf",pathpt,n,fontmag);
           rc = stat(name,&s);
       }
#endif
       if (rc==0) return(TRUE);
     };

#ifdef FUTURE
    for(i=0; (pathpt=path_segment((bool)(i==0),VFPATH,local_path))!=NULL;i++) {
       sprintf(name,"%s/%s.vfm",pathpt,n);
       printf("searching virtual font <%s>\n",name);
       if (stat(name,&s) == 0) return(TRUE);
    }
#endif

#ifdef USEPXL
    /* return error messaage */
    sprintf(name,"font not found: <%s>/%s.<%d;%ld>gf",
                  path,n,resolution,fontmag);
#else
    sprintf(name,"font not found: <%s>/<dpi%d;pxl%ld>/%s.<pk;pxl>",
                  path,resolution,fontmag,n);
#endif

return(FALSE);
}

char *
path_segment(first,full_path,local_path)
bool first;
char *full_path, *local_path;
{
	static char *pppt;
	char *pathpt;

	if (first) pathpt = strcpy(local_path,full_path);
	else pathpt = pppt;
	if (pathpt != NULL) {
#ifdef unix
       	        pppt = strchr(pathpt , ':' );
#else
                pppt = strchr(pathpt , ';' );
#endif
                if (pppt != NULL) {
                   *pppt = '\0';
                   pppt++;
                   }
        }
return pathpt;
}
