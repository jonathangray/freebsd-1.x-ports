#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#define	strchr	index
#define	strrchr	rindex
#endif

/*
 * converters program names
 */
#define FIG2DEV	"fig2dev"
#define PIC2FIG "pic2fig"
#define APG2FIG "apgto f"

/*
 * filename defaults
 */
#define MK "Makefile"
#define TX "transfig.tex"

enum language  {box, epic, eepic, eepicemu, latex,
	pictex, postscript, psfig, pstex, textyl, tpic};
#define MAXLANG tpic

enum input {apg, fig, pic, ps};
#define MAXINPUT xps

typedef struct argument{
	char *name, *interm, *f, *s, *m, *tofig, *topic, *tops;
	enum language tolang;
	enum input type;
	struct argument *next;
} argument ;

extern enum language str2lang();
extern char *lname[];
extern char *iname[];

extern char *sysls(), *mksuff();
extern argument *arglist;
extern char *txfile, *mkfile;

extern char *optarg;
extern int optind;

