/* exp_log.h */

#include "exp_printify.h"

/* special version of log for non-null-terminated strings which */
/* never need printf-style formatting. */
#define logn(buf,length)  { \
			  if (logfile) fwrite(buf,1,length,logfile); \
			  if (debugfile) fwrite(buf,1,length,debugfile); \
			  }

#define dprintify(x)	((is_debugging || debugfile)?exp_printify(x):0)
/* in circumstances where "debuglog(printify(...))" is written, call */
/* dprintify instead.  This will avoid doing any formatting that would */
/* occur before debuglog got control and decided not to do anything */
/* because (is_debugging || debugfile) was false. */

void exp_errorlog();
void exp_log();
void exp_debuglog();
void exp_nflog();
void exp_nferrorlog();

extern FILE *debugfile;
extern FILE *logfile;
extern int logfile_all;

extern int is_debugging;	/* useful to know for avoid debug calls */
