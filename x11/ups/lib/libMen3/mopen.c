/* mopen.c - code to read a menu definition from a file */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mopen_c_sccsid[] = "@(#)mopen.c	1.12 26/4/92 (UKC)";

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/file.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

#ifdef __STDC__
void message(const char *fmt, ...);
#endif

#define MAXNODES 1000	/* Max number of nodes for compiling menus */

#define ANY_CHAR (-2)
#define LASTNODE (-2)

static MENU *Nodevec[MAXNODES];	/* Table of node addresses */

static struct merrst merr;	/* Error information */

/*  Magic number for old binary perq menu files.
 */
#define MENUMAGIC	0xcd00ce00

/*  Structures and constants  used in a menu tree structure
 */
struct omnode {
	unsigned short ome_flags;
	short ome_pos;	/*  The position at which the field is divided */
	char *ome_cap;	/*  Caption to be displayed in the field */
	short ome_rv; 		/* result to be returned */
	short ome_xstart;	/* The window area of this node */
	short ome_ystart;
	short ome_xend;
	short ome_yend;
	short ome_colour;
	char *ome_save;			/* For saving popup backgrounds */
	struct omnode * ome_topleft; /* pointer to top or left sub-menu */
	struct omnode * ome_botrite; /* pointer to b. or r. sub_menu */
	struct omnode * ome_parent;  /* pointer to parent */
	struct omenst *ome_omen;     /* pointer to open menu structure */
};

static MENU *mread MPROTO((int fildes, MENU *parent));
MENU *getmenu MPROTO((const char *fname, char *mname));
static MENU *getsrcmenu MPROTO((int fd, char *mname));
static void fix_parent_links MPROTO((MENU *mp, MENU *parent));
static int skip_header MPROTO((FILE *fp));
static int getmnode MPROTO((FILE *fp, MENU *mnode, char *mname));
static int skipnull MPROTO((FILE *fp, int expected));
static int getstring MPROTO((FILE *fp, const char **p_cptr, int expected));
static int backslash MPROTO((FILE *fp));
static int do_struct_header MPROTO((FILE *fp, char *mname));
static int skipwhite MPROTO((FILE *fp, int expected));
static int getword MPROTO((FILE *fp, char *buf));
static int checkword MPROTO((FILE *fp, const char *word));
static int getnodenum MPROTO((FILE *fp, MENU **p_mnode, int expected));
static int getnum MPROTO((FILE *fp, short *p_res, int followch));
static int mygetc MPROTO((FILE *fp));
static void myungetc MPROTO((int c, FILE *fp));

/*  BUG: this shouldn't be here, but where should it be?  Not in ansi/stdio.h.
 */
FILE *fdopen MPROTO((int fd, const char *mode));

/*  Read in the menu from file 'name'.  A menu identifier is returned which
 *  is used to identify the menu in all subsequent function calls.
 *  -1 is returned if unsuccessful.
 */
int
Mopen(name)
const char * name;
{
	register int i;

	for (i = 0; i < MAXMEN; i++)
		if (_menu_[i].om_root == NULL)
			break;
	if (i == MAXMEN) {
		menerr = MTABFL;
		return(-1);
	}

	if ((_menu_[i].om_root = getmenu(name,(char *)NULL)) == NULL)
		return(-1);

	_menu_[i].om_md = i;
	_menu_[i].om_last = NULL;
	_menu_[i].om_font[0] =
	_menu_[i].om_font[1] =
	_menu_[i].om_font[2] =
	_menu_[i].om_font[3] = Mstdfont();
	_menu_[i].om_fback[0] = MH_GREY;
	_menu_[i].om_fback[1] = MH_BLACK;
	_menu_[i].om_fback[2] = MH_GREY;
	_menu_[i].om_wantbox = 1;
	momen(_menu_[i].om_root,&_menu_[i]);
	menerr = MENOK;
	return(i);
}

/*  Recusively read an old binary menu menu from file opened with file
 *  descriptor fildes and convert it.
 *  When first called the argument 'parent' should be NULL. If the call is
 *  unsuccessful NULL will be returned. Room for the menu is obtained using
 *  malloc.
 */
static MENU *
mread(fildes,parent)
int fildes;
MENU * parent;
{
	MENU * menu;
	struct omnode om;
	int n;
	char *newcap;

	menu = (MENU *)malloc(sizeof(MENU));
	if (read(fildes,(char *)&om,sizeof(om)) != sizeof(om)) {
		free((char *)menu);
		return(NULL);
	}
	menu->me_flags = om.ome_flags;
	menu->me_pos = om.ome_pos;
	menu->me_cap = om.ome_cap;
	menu->me_rv = om.ome_rv;
	menu->me_xstart = om.ome_xstart;
	menu->me_ystart = om.ome_ystart;
	menu->me_xend = om.ome_xend;
	menu->me_yend = om.ome_yend;
	menu->me_colour = om.ome_colour;
	menu->me_topleft = (MENU *)om.ome_topleft;
	menu->me_botrite = (MENU *)om.ome_botrite;

	menu->me_parent = parent;
	if ((n = (int)menu->me_cap) != 0) {
		if ((n < 0) || (n > 2000))
			return(NULL);
		newcap = malloc((size_t)n);
		if (read(fildes,newcap,(size_t)n) != n) {
			free(newcap);
			return(NULL);
		}
		menu->me_cap = newcap;
	}
	if (menu->me_topleft != NULL)
		if ((menu->me_topleft = mread(fildes,menu)) == NULL) {
			free((char *)menu);
			return(NULL);
		}
	if (menu->me_botrite != NULL)
		if ((menu->me_botrite = mread(fildes,menu)) == NULL) {
			free((char *)menu);
			return(NULL);
		}
	menu->me_flags |= (ME_FREC | ME_FREN);
	return(menu);
}

/*  Read a menu from file fname and return a pointer to it. In case of failure
 *  NULL is returned. If the menu is in the old format it is automatically
 *  converted. If the menu is in the new format and mname is non NULL then
 *  it is used as a destination for the menu variable name.
 */
MENU *
getmenu(fname,mname)
const char *fname;
char *mname;
{
	int fd, magic;
	MENU *menu;

	if ((fd = open(fname,0)) < 0) {
		menerr = MNOFIL;
		return(NULL);
	}
	read(fd,(char *)&magic,sizeof(magic));
	if (magic == MENUMAGIC) {
		/* an old binary menu. read it in and convert it.
		 */
		menu = mread(fd,(MENU *)NULL);
		close(fd);
		if (menu == NULL) {
			menerr = MBADFIL;
		}
		return(menu);
	}
	lseek(fd,(long)0,0);
	merr.mr_line = 1;
	menu = getsrcmenu(fd,mname);
	close(fd);
	return(menu);
}

/*  The following routines, which compile a C source code menu, are due
 *  to Mark Russell.
 */

/*  Read in a source code menu from the file open with file descriptor fd.
 *  If mname is not NULL it will be used as a destination for the menu
 *  name.
 */
static MENU *
getsrcmenu(fd,mname)
int fd;
char *mname;
{
	FILE *fp;
	MENU *mnode;
	int nodenum;
	
	if ((fp = fdopen(fd,"r")) == NULL) {
		return(NULL);
	}
	if (skip_header(fp) == -1) {
		menerr = MBADFIL;
		fclose(fp);
		return(NULL);
	}
	for (;;) {
		if ((mnode = (MENU *)malloc(sizeof(MENU))) == NULL)
			abort();
		if ((nodenum = getmnode(fp,mnode,mname)) == LASTNODE)
			break;
		if (nodenum >= MAXNODES) {
			menerr = MBADFIL;
			nodenum = -1;
		}
		if (nodenum == -1) {
			for (nodenum = 0; nodenum < MAXNODES; nodenum++) {
				if (Nodevec[nodenum] != NULL)
					free((char *)Nodevec[nodenum]);
				Nodevec[nodenum] = NULL;
			}
			return(NULL);
		}
		Nodevec[nodenum] = mnode;
	}
	fix_parent_links(mnode,(MENU *)NULL);
	for (nodenum = 0; nodenum < MAXNODES; nodenum++)
		Nodevec[nodenum] = NULL;
	return(mnode);
}

static void
fix_parent_links(mp,parent)
MENU *mp, *parent;
{
	if (mp == NULL)
		return;
	fix_parent_links(mp->me_topleft,mp);
	fix_parent_links(mp->me_botrite,mp);
	mp->me_parent = parent;
}

/*  skip header of menu file. We depend on the first real line
 *  starting with the 's' of 'static'
 */
static int
skip_header(fp)
FILE *fp;
{
	int ch, lastch;
	
	lastch = '\n';
	while ((ch = mygetc(fp)) != EOF) {
		if ((ch == 's' || ch == 'M') && lastch == '\n') {
			myungetc(ch,fp);
			return(0);
		}
		lastch = ch;
	}
	return(-1);
}

static int
getmnode(fp,mnode,mname)
FILE *fp;
MENU *mnode;
char *mname;
{
	short flags, nodenum;
	
	if ((nodenum = do_struct_header(fp,mname)) == -1||
	    getnum(fp, &flags,',')			||
	    getnum(fp, &mnode->me_pos,',')		||
	    getstring(fp, &mnode->me_cap,',')		||
	    getnum(fp, &mnode->me_rv,',')		||
	    getnum(fp, &mnode->me_xstart,',')		||
	    getnum(fp, &mnode->me_ystart,',')		||
	    getnum(fp, &mnode->me_xend,',')		||
	    getnum(fp, &mnode->me_yend,',')		||
	    getnum(fp, &mnode->me_xcurs,',')		||
	    getnum(fp, &mnode->me_ycurs,',')		||
	    getnum(fp, &mnode->me_colour,',')		||
	    skipnull(fp,',')				||
	    getnodenum(fp, &mnode->me_topleft,',')	||
	    getnodenum(fp, &mnode->me_botrite,',')	||
	    skipnull(fp,',')				||
	    skipnull(fp,',')				||
	    skipwhite(fp,'}')				||
	    skipwhite(fp,';'))
		return(-1);
	mnode->me_flags = flags;
	mnode->me_save = NULL;
	mnode->me_parent = NULL;
	mnode->me_omen = NULL;
	return(nodenum);
}

static int
skipnull(fp,expected)
FILE *fp;
int expected;
{
	return(checkword(fp,"NULL") != 0 ? -1 : skipwhite(fp,','));
}

static int
getstring(fp,p_cptr,expected)
FILE *fp;
const char **p_cptr;
int expected;
{
	char buf[256], *cptr, *new;
	int ch;
	
	if ((ch = skipwhite(fp,ANY_CHAR)) == 'N') {
		*p_cptr = NULL;
		myungetc('N',fp);
		return(skipnull(fp,expected));
	}
	else  if (ch == '0') {
		*p_cptr = NULL;
		return(skipwhite(fp,expected));
	}
	else if (ch != '"') {
		menerr = MBADFIL;
		return(-1);
	}
	cptr = buf;
	while ((ch = mygetc(fp)) != EOF && ch != '"') {
		if (ch < 32 || ch > 127) {
			/*  illegal character
			 */
			menerr = MBADFIL;
			return(-1);
		}
		if (cptr >= buf + sizeof(buf) - 1) {
			menerr = MBADFIL;
			return(-1);
		}
		if (ch == '\\')
			if ((ch = backslash(fp)) == -1)
				return(-1);
		*cptr++ = ch;
	}
	if (ch == EOF) {
		menerr = MBADFIL;
		return(-1);
	}
	*cptr = '\0';
	if ((new = malloc((size_t)strlen(buf) + 1)) == NULL)
		abort();
	*p_cptr = strcpy(new, buf);
	return(skipwhite(fp,expected));
}

/*  process C escape sequence
 */
static int
backslash(fp)
FILE *fp;
{
	char *pos;
	int ch, res, ndigs;
	
	if ((ch = mygetc(fp)) == EOF) {
		menerr = MBADFIL;
		return(-1);
	}
	if ((pos = strchr("n\nt\tb\br\r",ch)) != NULL)
		return(pos[1]);
	else if (ch < '0' || ch > '7')
		return(ch);
	res = ndigs = 0;
	while (ndigs++ < 3 && (ch = mygetc(fp)) != EOF && ch>='0' && ch<='7')
		res = res * 8 + ch - '0';
	if (ch == EOF) {
		menerr = MBADFIL;
		return(-1);
	}
	return(res);
}

static int
do_struct_header(fp,mname)
FILE *fp;
char *mname;
{
	char wbuf[100];
	short nodenum;
	
	if (getword(fp,wbuf) != 0)
		return(-1);
	if (strcmp(wbuf,"static") == 0) {
		if (checkword(fp,"MENU") != 0 ||
		    checkword(fp,"MM") != 0 ||
		    getnum(fp,&nodenum,'=') == -1 ||
		    skipwhite(fp,'{') != 0)
			return(-1);
		return(nodenum);
	}
	else {
		if (strcmp(wbuf,"MENU") != 0 ||
		    getword(fp,mname) != 0 ||
		    skipwhite(fp,'=') != 0 ||
		    skipwhite(fp,'{') != 0)
			return(-1);
		return(LASTNODE);
	}
}

static int
skipwhite(fp,expected)
FILE *fp;
int expected;
{
	int ch;

	while ((ch = mygetc(fp)) == ' ' || ch == '\t' || ch == '\n')
		;
	if (ch == EOF) {
		menerr = MBADFIL;
		return(-1);
	}
	if (expected != ANY_CHAR && ch != expected) {
		menerr = MBADFIL;
		return(-1);
	}
	return((expected == ANY_CHAR) ? ch : 0);
}

static int
getword(fp,buf)
FILE *fp;
char *buf;
{
	int ch;
	
	if ((ch = skipwhite(fp,ANY_CHAR)) == -1 || !(isascii(ch)&&isalpha(ch)))
		return(-1);
	while (isascii(ch) && (isalpha(ch) || isdigit(ch) || ch == '_')) {
		if (buf != NULL)
			*buf++ = ch;
		ch = mygetc(fp);
	}
	if (buf != NULL)
		*buf = '\0';
	return(0);
}

static int
checkword(fp,word)
FILE *fp;
const char *word;
{
	int ch1, ch2;
	
	if (skipwhite(fp,*word++) != 0)
		return(-1);
	for (;;) {
		if ((ch1 = *word++) == '\0')
			return(0);
		if ((ch2 = mygetc(fp)) == EOF) {
			menerr = MBADFIL;
			break;
		}
		if (ch1 != ch2) {
			menerr = MBADFIL;
			break;
		}
	}
	return(-1);
}

static int 
getnodenum(fp,p_mnode,expected)
FILE *fp;
MENU **p_mnode;
int expected;
{
	int ch;
	short nodenum;
	
	if ((ch = skipwhite(fp,ANY_CHAR)) == '&') {
		if (skipwhite(fp,'M') != 0 ||
		    mygetc(fp) != 'M' ||
		    getnum(fp,&nodenum,expected) == -1)
			return(NULL);
		if (nodenum < 0 || nodenum >= MAXNODES ||
					Nodevec[nodenum] == NULL){
			menerr = MBADFIL;
			return(-1);
		}
		*p_mnode = Nodevec[nodenum];
		return(0);
	}
	else if (ch == 'N') {
		if (checkword(fp,"ULL") != 0)
			return(-1);
	}
	else if (ch != '0') {
		menerr = MBADFIL;
		return(-1);
	}
	*p_mnode = NULL;
	return(skipwhite(fp,expected));
}

static int
getnum(fp, p_res, followch)
FILE *fp;
short *p_res;
int followch;
{
	int ch, base, res, minus;
	
	if ((ch = skipwhite(fp,ANY_CHAR)) == -1)
		return(-1);
	minus = 0;
	if (ch == '0') {
		if ((ch = mygetc(fp)) == 'x')
			base = 16;
		else {
			base = 8;
			myungetc(ch,fp);
		}
	}
	else if (isdigit(ch) || ch == '-') {
		base = 10;
		if (ch == '-')
			minus = 1;
		else
			myungetc(ch,fp);
	}
	else {
		menerr = MBADFIL;
		return(-1);
	}
	res = 0;
	for (;;) {
		if ((ch = mygetc(fp)) == EOF) {
			menerr = MBADFIL;
			return(-1);
		}
		if (isascii(ch) && islower(ch))
			ch = toupper(ch);
		if (isdigit(ch) || (base == 16 && ch >= 'A' && ch <= 'F'))
			res = res * base + ch - ((ch > '9') ? 'A'-10 : '0');
		else {
			*p_res = minus ? -res : res;
			myungetc(ch,fp);
			return(skipwhite(fp,followch));
		}
	}
}

/*  Code for reporting back on errors in the menu file.
 *
 *  Need different versions for old-style and ANSI C.
 *
 *  BUG: this uses a fixed size buffer.  If sprintf overflows it we
 *  get a core dump or worse.  The fix for this is vsnprintf(), which
 *  unfortunately doesn't exist.
 */
#ifdef __STDC__
void
message(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	(void) vsprintf(merr.mr_message, fmt, args);
	va_end(args);
}
#else
void
message(va_alist)
va_dcl
{
	va_list args;
	const char *fmt;

	va_start(args);
	fmt = va_arg(args, char *);
	(void) vsprintf(merr.mr_message, fmt, args);
	va_end(args);
}
#endif /* !__STDC__ */

struct merrst *
Mopenerr()
{
	return(&merr);
}

static int
mygetc(fp)
FILE *fp;
{
	int c;

	if ((c = getc(fp)) == '\n')
		merr.mr_line++;
	return(c);
}

static void
myungetc(c,fp)
int c;
FILE *fp;
{
	ungetc(c,fp);
	if (c == '\n')
		merr.mr_line--;
}
