extern char *fgetfln();		/* internal interface */

/* external interfaces */
#define fgetln(fp)  fgetfln(fp, -1, (int *)NULL) /* unbounded read */
#define cfgetln(fp) csfgetln(fp, -1, 1)	/* unbounded cont.d read; toss space */

extern char *csfgetln(), *dogets(), *fgetline();
