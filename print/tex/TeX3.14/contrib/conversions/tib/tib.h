/*   various arguments for tib and tiblist processors */

/* file names 
            need to be set at installation */

        /* common words */
#define COMFILE "CCCCC"
        /* default system index */
#define SYSINDEX "FFFFF"
        /* where macro libraries live */
#define TMACLIB "BBBBB"
        /* default style of references */
#define DEFSTYLE "BBBBB/num.tib"
        /* output of tibdex, input file for references */
#define INDXFILE "INDEX"

/* system dependent characters */

#define DIRSEP "/"          /* directory separator for path names */
                            /* use "/" for unix */
                            /* use ":" for TOPS-20/VMS */
                            /* use "\\" for MS-DOS  */
#define OPTCH "-"           /* option character for program call */
                            /* use "-" for unix */
                            /* use "/" for VMS, MS-DOS */

/* temporary work files */

        /* pass1 reference collection file */
#define TMPREFFILE  "/tmp/tibrXXXXXX"
        /* pass2 text collection file */
#define TMPTEXTFILE "/tmp/tibpXXXXXX"
        /* temp file used in tibdex */
#define INVTEMPFILE "/tmp/tibdexXXXXXX"

/* constants */

#define VERSION  "2.1"              /* version number                      */
#define RDATE "12/1/87"             /* release date                        */
#define true  1
#define false 0
#define err  -1
#define REFSIZE    1024             /* maximum size of reference string    */
#define MAXFIELD    512          /* maximum size of any field in reference */
#define HUNTSIZE    512             /* maximum size of hunt string         */
#define MAXREFS     300        /* maximum number of references in document */
#define MAXATONCE    35             /* maximum references at one location  */
#define LINELENGTH 1024
#define MAXDEFS    2200             /* maximum number of defined words     */
#define MAXCOMM    1000     /* maximum number of characters in common file */
#define MAXSTR      256            /* maximum length of string in streams.c */

/* reference citation marker generated in pass 1 */

#define CITEMARK (char) 02
#define CITEEND  (char) 03

char *malloc();

/* fix needed for systems where open [w]+ doesn't work */
#ifdef READWRITE

#define READ 1
#define WRITE 0

#endif

 /* Modification added by J. Goldberg for porting to HP-UX */
 
#ifdef HPUX
#   include "/usr/contrib/include/bsd.h"
#   include <string.h>                 /* for index --> strchr */
#endif
