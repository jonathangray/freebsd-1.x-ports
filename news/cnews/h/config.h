/*
 * configuration-inquiry functions
 */

extern char *artfile();		/* article pathname, may be relative */
extern char *fullartfile();	/* article full pathname */
extern char *ctlfile();		/* control-file name */
extern char *binfile();		/* program pathname */
extern char *newspath();	/* PATH */
extern int newsumask();		/* umask */
extern char *newsmaster();	/* place to mail complaints to */

extern void cd();		/* chdir() with errunlock() on failure */

extern void unprivileged();	/* user-supplied privilege dropper */
