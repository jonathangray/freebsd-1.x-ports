/* newsgroup matching */
#define NGPAT struct ngpatnode

extern boolean ngmatch();	/* old, slow interface */

extern NGPAT *ngparse();	/* parse newsgroup pattern into a tree */
extern ngprint();		/* print a pattern tree */
extern boolean ngpatmat();	/* match a pattern against a group list */
extern int ngpatscore();	/* as ngpatmat, but return the score */
extern ngfree();		/* destroy a tree */
extern char *ngerr;		/* error string, if non-NULL */
