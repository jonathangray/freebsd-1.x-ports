/* marea.c - message area routines */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_marea_c_sccsid[] = "@(#)marea.c	1.7 26/4/92 (UKC)";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"	/* for Box_t */
#include "wdbyte.h"

#define DEL 0177
#define OOPS 025

/*  message area structure.
 */
typedef struct msg_st {
	short msg_x;	/* TLH corner of the message area */
	short msg_y;
	short msg_width;
	short msg_depth;
	int msg_wfd;	/* window file descriptor */
	struct DBCtl msg_dba;
} MAREA;

static int mgets MPROTO((MAREA *ma, char *string));
static int mgetc MPROTO((MAREA *ma));
static int mputs MPROTO((MAREA *ma, const char *string));
static int mputc MPROTO((int c, MAREA *ma));
static void scroll MPROTO((MAREA *ma));
static void bline MPROTO((MAREA *ma));
static void erase MPROTO((MAREA *ma, int n));
static MAREA *msopen MPROTO((int wn, int x, int y, int width, int depth));

static MAREA * _marea_[MAXMEN] = {
	NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL
};

/* Width and depth of a character in the system font */
static int Char_width = -1, Char_height;

/*  Open a message area height pixels high and width pixels wide with top
 *  left corner at x, y. wd should be the file descriptor of an open window.
 */
int
Msopen(wd,x,y,width,height)
int wd, x, y, width, height;
{
	register i;

	for (i = 0; i < MAXMEN; i++)
		if (_marea_[i] == NULL)
			break;

	if (i == MAXMEN) {
		menerr = MAERROR;
		return(-1);
	}

	if ((_marea_[i] = msopen(wd,x,y,width,height)) == NULL)
		return(-1);
	return(i);
}

/*  Close the numbered message area, freeing the slot. The screen is not
 *  changed. The procedure becomes a no-op if 'mad' does not represent a
 *  valid message area.
 */
int
Msclose(mad)
int mad;
{
	if ((mad < 0) || (mad >= MAXMEN))
		return(0);
	if (_marea_[mad] == NULL)
		return(0);
	free ((char *)_marea_[mad]);
	_marea_[mad] = NULL;
	return(0);
}

/*  display the string in the message area opened with descriptor mad.
 *  at most u - l + 1 characters are displayed. All control characters
 *  except '\n', '\r' and '\0' are ignored. Wrapping is done automatically.
 */
int
Mputs(mad, str)
int mad;
const char *str;
{
	if ((mad < 0) || (mad >= MAXMEN)) {
		menerr = MAERROR;
		return(-1);
	}
	if (_marea_[mad] == NULL) {
		menerr = MAERROR;
		return(-1);
	}

	if (mputs(_marea_[mad],str) < 0) {
		menerr = MAERROR;
		return(-1);
	}
	return(0);
}

/*  Read in text from the keyboard while echoing it in the message area.
 *  A single line is input terminated by '\n' or '\r'. DEL and OOPS work
 *  as usual and wrapping is done automatically. Other control characters
 *  are echoed as BELL and then ignored. While input is expected a black
 *  rectangle is displayed as a text cursor. The string is returned in str.
 */
int
Mgets(mad,str)
int mad;
char *str;
{
	if ((mad < 0) || (mad >= MAXMEN)) {
		menerr = MAERROR;
		return(-1);
	}
	if (_marea_[mad] == NULL) {
		menerr = MAERROR;
		return(-1);
	}

	if (mgets(_marea_[mad], str) < 0) {
		menerr = MAERROR;
		return(-1);
	} else
		return(0);
}

/*  Print the character c at the current position in the message area 
 *  represented by mad. '\n' and '\r' have the usual effects, other control
 *  characters are ignored. Wrapping is done automatically.
 */
int
Mputc(mad,c)
int mad;
char c;
{

	if ((mad < 0) || (mad >= MAXMEN)) {
		menerr = MAERROR;
		return(-1);
	}
	if (_marea_[mad] == NULL) {
		menerr = MAERROR;
		return(-1);
	}

	c &= 0177;
	if (c >= ' ') {
		if (mputc(c,_marea_[mad]) < 0) {
			menerr = MAERROR;
			return(-1);
		}
	} else if (c == '\n')
		scroll(_marea_[mad]);
	else if (c == '\r')
		bline(_marea_[mad]);
	return(0);
}

/*  Erase a maximum of n characters from before the current printing position
 *  in the message area represented by mad. Erasing does not continue beyond
 *  the start of the current line. If n == 0 the current line is erased.
 */
int
Merase(mad,n)
int mad,n;
{
	if ((mad < 0) || (mad >= MAXMEN)) {
		menerr = MAERROR;
		return(-1);
	}
	if (_marea_[mad] == NULL) {
		menerr = MAERROR;
		return(-1);
	}

	if (n == 0)
		bline(_marea_[mad]);
	else
		erase(_marea_[mad],n);
	return(0);
}

/*  Open a message area at location x,y with dimensions width and depth pixels.
 *  wn should be the file descriptor of an already open window. A box is
 *  drawn round the area and the current printing position is set to the start
 *  of the bottom line. In case of failure NULL is returned.
 */
static MAREA * 
msopen(wn,x,y,width,depth)
int wn;
int x, y, width, depth;
{
	MAREA * m;
	struct DBCtl * db;
	Pos_t p1, p2;
	Box_t box;
	font_t *sysfont;

	if (Char_width == -1) {
		sysfont = wn_get_sysfont();
		Char_width = sysfont->ft_width;
		Char_height = sysfont->ft_depth;
	}
	m = (MAREA *)malloc(sizeof(MAREA));
	box.pos.x = m->msg_x = x;
	box.pos.y = m->msg_y = y;
	box.size.w = m->msg_width = width;
	box.size.h = m->msg_depth = depth;
	m->msg_wfd = wn;
	db = &(m->msg_dba);
	db->DBFont = wn_get_sysfont();
	db->DBX = x + Char_width / 2;
	db->DBY = y + depth - db->DBFont->ft_height + db->DBFont->ft_baseline - 1;
	db->DBFunc = R_RPL;
	db->DBMaxX = x + width;
	db->DBScreen = 0;

	/* draw a box round the message area */

	p1.x = x - 1;
	p2.x = x + width + 1;
	p1.y = p2.y = y + depth;
	wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
	if (y > 0) {
		p1.y = p2.y = y-1;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
	}
	p1.y = y - 1;
	p2.y = y + depth;
	p1.x = p2.x = x + width;
	wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
	if (x > 0) {
		p1.x = p2.x = x-1;
		wn_draw_line(wn,p1.x,p1.y,p2.x,p2.y,BLACK);
	}
	/* blank the interior
	 */
	wblank(wn,&box);
	return(m);
}
	
/*  Output the string to the message area starting at the cuirrent printing
 *  position. '\n' causes the area to scroll up one line. '\r' blanks out
 *  the current line. Both '\n' and '\r' set the current printing position
 *  to the start of the current (presumably last) line.
 *  '\0' is treated as an 'end of line' and should be used to terminate
 *  the string. Lines too long to fit in the message area are automatically
 *  wrapped. Control characters except '\n', '\r' and '\0' are ignored.
 */
static int
mputs(ma,string)
MAREA * ma;
const char *string;
{
	struct DBCtl * db;
	int w;

	Mpushsu(ma->msg_wfd);
#ifdef DEBUG
	dbprintf("mputs(%x,",(int)ma);
	dbputsv(string);
#endif
	db = &(ma->msg_dba);
	db->DBByteOffset = 0;
	db->DBMaxByte = strlen(string);
	db->DBSrcString = string;
	while ((w = wn_wdbyte(ma->msg_wfd,db,(Box_t *)0)) > 0) {
#ifdef DEBUG
		dbprintf("w:%s ByteO:%d MaxB:%d SrcS:",dec(w),
				db->DBByteOffset,db->DBMaxByte);
		dbputsv(db->DBSrcString);
		dbputc('\n');
#endif
		if (w == 1) {  /* R.H. margin reached */
			scroll(ma);
			continue;
		}
		if (string[db->DBByteOffset] == '\n') {
			scroll(ma);
		}
		if (string[db->DBByteOffset] == '\r') {
			bline(ma);
		}
		if (w == 2)
			db->DBByteOffset++;
	}
	Mpopsu(ma->msg_wfd);
	return(w);
}

/*  Blank the current line and set the current printing position to the start
 *  of the line.
 */
static void
bline(ma)
MAREA * ma;
{
	wn_set_area(ma->msg_wfd,ma->msg_x,ma->msg_dba.DBY - Char_height,
			ma->msg_width,Char_height+2,WHITE);
	ma->msg_dba.DBX = ma->msg_x + Char_width/2;
}

/*  Scroll up the message area one line and set the current printing position
 *  to the start of the line.
 */
static void
scroll(ma)
MAREA * ma;
{
	Mpushsu(ma->msg_wfd);
	if (ma->msg_depth - Char_height >= Char_height)
		/*  don't scroll right out of the area
		 */
		wn_mono_rop(ma->msg_wfd,
				ma->msg_x,
				ma->msg_y + Char_height + 1,
				ma->msg_width,
				ma->msg_depth - Char_height,
				ma->msg_x,
				ma->msg_y + 1,
				R_RPL);
	bline(ma);
	Mpopsu(ma->msg_wfd);
}


/*  Get a message typed to the window. The message is echoed in the message
 *  area starting at the current position. While input is expected a text
 *  cursor is displayed at the current position.'\r' terminates the
 *  input message but is not included in the returned string. DEL and
 *  OOPS have the usual effect. Long lines are wrapped automatically but
 *  DEL and OOPS don't echo properly past the wrap.
 */
static int
mgets(ma,string)
MAREA * ma;
char *string;
{
	short c;
	short i = 0;

	for (;;) {
		if ((c = mgetc(ma)) == EOF)
			return(-1);
		if ((c == '\n') || (c == '\r')) {
			mputc(' ',ma);
			break;
		}
		if (c == DEL) {
			if (i > 0) {
				mputc(' ',ma);
				erase(ma,2);
				i--;
			} else
				wn_bell(WN_STDWIN);
		}
		else if (c == OOPS) {
			if (i == 0)
				wn_bell(WN_STDWIN);
			else {
				mputc(' ',ma);  /* remove text cursor */ 
				erase(ma,1);
				while (i > 0) {
					erase(ma,1);
					i--;
				}
			}
		}
		else if (c < ' ')
			wn_bell(WN_STDWIN);
		else {
			mputc(c,ma);
			string[i++] = c;
		}
	}
	string[i] = 0;
	return(0);
}

/*  Output c to the message area at the current printing position. The printing
 *  position is moved on one character. '\n' and '\r' don't have any effect
 *  but wrapping is done if appropriate.
 */
static int
mputc(c,ma)
int c;
MAREA * ma;
{
	struct DBCtl * db;
	char s[2];
	int w;		

	c &= 0177;
	db = &(ma->msg_dba);
	db->DBByteOffset = 0;
	db->DBMaxByte = 1;
	s[0] = c;
	db->DBSrcString = s;
	while ((w = wn_wdbyte(ma->msg_wfd,db,(Box_t *)0)) == 1)
		scroll(ma);
	return(w);
}

/*  Erase characters from before the printing position in the current line
 *  and back up the printing position. n characters will be erased if possible,
 *  but erasure does not extend beyond the start of the line. If n is zero
 *  then erasure is to the start of the line.
 */
static void
erase(ma,n)
MAREA * ma;
int n;
{
	struct DBCtl * db;
	short nchars;

	db = &(ma->msg_dba);
	/* first determine how many characters are displayed */
	nchars = (db->DBX - ma->msg_x) / Char_width;
	n = ((n > nchars) || (n == 0)) ? nchars : n;
	while (n--) {
		db->DBByteOffset = 0;
		db->DBX -= Char_width;
		db->DBMaxByte = 1;
		db->DBSrcString = " ";
		wn_wdbyte(ma->msg_wfd,db,(Box_t *)0);
		db->DBX -= Char_width;
	}
}

/*  Read a character typed at the keyboard. The text cursor is displayed at
 *  the current position before the character is read.
 */
static int
mgetc(ma)
MAREA *ma;
{
	char c[2];
	int junk, wn;

	mputc(DEL,ma);
	wn = ma->msg_wfd;
	ma->msg_dba.DBX -= Char_width;
	while (!wn_getc(wn,c))
		(void) wn_getpuck(wn,&junk,&junk);
	*c &= 0177;
	return(*c);
}
