/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include "xtrekfont.h"
#include "config.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define MAXPLAYER 16
#define MAXPLANETS 40
#define MAXTORP 30
#define MAXMINE 15

/* do not change this unless you know what you are doing! */
#define XTREK_PORT 5701

/* These are configuration definitions */

/* Timing */
#define UPDATE 200000		/* Update time is 200000 us */
#define UPS (1000000/UPDATE)
#define WARP1(X) ((16*X) / UPS)		/* warp 1 moves 160 spaces per sec */

/* Space */
#define GWIDTH 100000			/* galaxy is 100000 spaces on a side */
#define SCALE 40			/* Window will be one pixel for this
					   many spaces */
/* These are memory sections */
#define PLAYER 1
#define MAXMSG 50

#define rosette(x)	((((x) + 256/VIEWS/2) / (256/VIEWS)) % VIEWS)

/* various defs */

#define TEMPSCALE(x)  ((x)*16)		/* scale temperature (use power of 2 ) */
#define SPEEDSCALE(x)  ((x)*10)		/* scale speed */
#define MAXTEMP 100			/* maximum temperature */

/* These are the empires */
/* UNA is unassigned */

#define UNA (-1)
#define FED 0
#define ROM 1
#define KLI 2
#define ORI 3
#define FLAG(x) ( 1 << (x) )
#define ALLEMPIRE (~0)			/* all mask bits are 1 */
#define MAXEMPIRES 8
#define DEFEMPIRES 4

/* These are random configuration variables */
#define VICTORY 16			/* Number of planets needed to conquer
					   the galaxy */
#define MINARMIES 10			/* Can't bomb a planet when armies <=
					   this */
#define WARNTIME 30			/* Number of updates to have a warning
					   on the screen */
#define MESSTIME 30			/* Number of updates to have a message
					   on the screen */

#define COUPARMIES 4			/* Max number of armies for coup */

#define TARG_PLAYER	(1 << 0)	/* Flags for gettarget */
#define TARG_PLANET	(1 << 1)
#define TARG_CLOAK	(1 << 2)	/* include cloaked ships in search */
#define	TARG_MYSELF	(1 << 3)	/* Allow myself */

#define NUM_SLIDERS 6

/* extra factor of 4 below to avoid integer wrap around in alert code. rr2b */
#define YRANGE ((GWIDTH)/16)
#define RRANGE ((GWIDTH)/32)


/* scale damage by this avoid bogus subshield/subdamage */

#define DAMSCALEVAL 128
#define DAMSCALE(x) ((x) * DAMSCALEVAL)

#include "struct.h"

#define FAILED(p, s) ((p)->p_sysdamage[(int) s] >= (p)->ship.fail[(int) s])
#define COST(p, s)   ((p)->ship.cost[(int) s])
#define HEAT(p, s)   ((p)->ship.heat[(int) s])
#define RETWARN(x, y) { warning((x),(y)); return; }

#define BETWEEN(l, x, h) (((l) <= (x)) && ((x) <= (h)))
#define RAND(x) (random() % (x))

#define ABS(a)			(((a) < 0) ? -(a) : (a))
#define myTorp(t)		(p->p_no == (t)->t_owner)
#define friendlyTorp(t)		((!(p->p_mask & (t)->t_war)) || (myTorp(t)))
#define myPhaser(x)		(&phasers[p->p_no] == (x))
#define friendlyPhaser(x)	(p->p_empire == players[(x) - phasers].p_empire)
#define myPlanet(x)		(p->p_empire == (x)->pl_owner)
#define friendlyPlayer(x)	((!(p->p_mask & \
				    ((x)->p_swar | (x)->p_hostile))) && \
				    !((x)->p_mask) & \
				    (p->p_swar | p->p_hostile))))
#define isAlive(x)		((x)->p_status == PAlive)
#define hostilePlanet(x,p)   (FLAG((x)->pl_owner) & ((p)->p_swar | (p)->p_hostile))
#define friendlyPlanet(x)	((x)->pl_info & p->p_mask && \
				     !hostilePlanet(x,p))

#define torpColor(t)		\
	(myTorp(t) ? p->myColor : p->shipCol[players[(t)->t_owner].p_empire])
#define mineColor(m)		\
	(p->shipCol[players[(m)->m_owner].p_empire])
#define phaserColor(x)		\
	(myPhaser(x) ? p->myColor : p->shipCol[players[(x) - phasers].p_empire])
#define playerColor(x)		\
	((p == (x)) ? p->myColor : p->shipCol[(x)->p_empire])
#define planetColor(x)		\
	(((x)->pl_info & p->p_mask) ? p->shipCol[(x)->pl_owner] : p->unColor)
#define mplanetGlyph(x)		\
	((((x)->pl_info & p->p_mask) ? \
	( p->mono ? (x)->pl_owner+1 : 0 ) : 0)+MPLANET_GLYPHS)
#define planetGlyph(x)		\
	((((x)->pl_info & p->p_mask) ? \
	( p->mono ? (x)->pl_owner+1 : 0 ) : 0)+PLANET_GLYPHS)

#define planetFont(x)		\
	(myPlanet(x) ? p->bfont : friendlyPlanet(x) ? p->ifont : p->dfont)
#define shipFont(x)		\
	((p == (x)) ? p->bfont : friendlyPlayer(x) ? p->ifont : p->dfont)

/* window sizes */

#define WINSIDE    500
#define BOXSIDE    (WINSIDE / 5)
#define BORDER    4
#define MSGSIZE  20
#define STATSIZE  (MSGSIZE * 2 + BORDER)
#define YOFF    100

/* Macros for X11 (nurk) */
#define fontWidth(f)	((f)->max_bounds.width)
#define fontHeight(f)	((f)->max_bounds.ascent + (f)->max_bounds.descent)

#define	TRIGSCALE	13		/* trig values are multiplied by 8K */

/* fill in ship characteristics */

#define set_course(p, dir) ((p)->p_desdir = (dir))

/* fuses */
/*
 * These specify how often special actions will take place in UPDATE units
 * (0.20 seconds, currently.)
 */
#define EMPIREFUSE      3
#define PLFIGHTFUSE     2
#define BEAMFUSE        5
#define PLANETFUSE      8001

#define SIZEOF(a)  (sizeof (a) / sizeof (*(a)))

#define ISPLOCK(x) ((x)->p_flags & PFPLOCK)
#define SETPLOCK(x) ((x)->p_flags |= PFPLOCK)
#define ISPLLOCK(x) ((x)->p_flags & PFPLLOCK)
#define SETPLLOCK(x) ((x)->p_flags |= PFPLLOCK)
#define ISCOPILOT(x) ((x)->p_flags & PFCOPILOT)
#define SETCOPILOT(x) ((x)->p_flags |= PFCOPILOT)
#define FLIPCOPILOT(x) ((x)->p_flags ^= PFCOPILOT)
#define ISRHOSTILE(x) ((x)->p_flags & PFRHOSTILE)
#define SETRHOSTILE(x) ((x)->p_flags |= PFRHOSTILE)
#define ISRHARD(x) ((x)->p_flags & PFRHARD)
#define SETRHARD(x) ((x)->p_flags |= PFRHARD)
#define ISRSTICKY(x) ((x)->p_flags & PFRSTICKY)
#define SETRSTICKY(x) ((x)->p_flags |= PFRSTICKY)
#define ISSHOWSTATS(x) ((x)->p_flags & PFSHOWSTATS)
#define SETSHOWSTATS(x) ((x)->p_flags |= PFSHOWSTATS)
#define ISWAR(x) ((x)->p_flags & PFWAR)
#define SETWAR(x) ((x)->p_flags |= PFWAR)
#define ISENTER(x) ((x)->p_flags & PFENTER)
#define SETENTER(x) ((x)->p_flags |= PFENTER)
#define ISFOLLOW(x) ((x)->p_flags & PFFOLLOW)
#define SETFOLLOW(x) ((x)->p_flags |= PFFOLLOW)
#define ISWEPREL(x) ((x)->p_flags & PFWEPREL)
#define SETWEPREL(x) ((x)->p_flags |= PFWEPREL)
#define ISPATROL(x) ((x)->p_flags & PFPATROL)
#define SETPATROL(x) ((x)->p_flags |= PFPATROL)
#define ISASSIST(x) ((x)->p_flags & PFASSIST)
#define SETASSIST(x) ((x)->p_flags |= PFASSIST)
#define ISCALLEDHELP(x) ((x)->p_flags & PFCALLEDHELP)
#define SETCALLEDHELP(x) ((x)->p_flags |= PFCALLEDHELP)
#define ISKILLER(x) ((x)->p_flags & PFKILLER)
#define SETKILLER(x) ((x)->p_flags |= PFKILLER)
#define ISSHIELD(x) ((x)->p_flags & PFSHIELD)
#define SETSHIELD(x) ((x)->p_flags |= PFSHIELD)
#define ISREPAIR(x) ((x)->p_flags & PFREPAIR)
#define SETREPAIR(x) ((x)->p_flags |= PFREPAIR)
#define ISBOMB(x) ((x)->p_flags & PFBOMB)
#define SETBOMB(x) ((x)->p_flags |= PFBOMB)
#define ISORBIT(x) ((x)->p_flags & PFORBIT)
#define SETORBIT(x) ((x)->p_flags |= PFORBIT)
#define ISCLOAK(x) ((x)->p_flags & PFCLOAK)
#define SETCLOAK(x) ((x)->p_flags |= PFCLOAK)
#define ISWEP(x) ((x)->p_flags & PFWEP)
#define SETWEP(x) ((x)->p_flags |= PFWEP)
#define ISENG(x) ((x)->p_flags & PFENG)
#define SETENG(x) ((x)->p_flags |= PFENG)
#define ISROBOT(x) ((x)->p_flags & PFROBOT)
#define SETROBOT(x) ((x)->p_flags |= PFROBOT)
#define ISBEAMUP(x) ((x)->p_flags & PFBEAMUP)
#define SETBEAMUP(x) ((x)->p_flags |= PFBEAMUP)
#define ISBEAMDOWN(x) ((x)->p_flags & PFBEAMDOWN)
#define SETBEAMDOWN(x) ((x)->p_flags |= PFBEAMDOWN)
#define ISSELFDEST(x) ((x)->p_flags & PFSELFDEST)
#define SETSELFDEST(x) ((x)->p_flags |= PFSELFDEST)
#define ISTURBO(x) ((x)->p_flags & PFTURBO)
#define SETTURBO(x) ((x)->p_flags |= PFTURBO)
#define ISSNEAKY(x) ((x)->p_flags & PFSNEAKY)
#define SETSNEAKY(x) ((x)->p_flags |= PFSNEAKY)

#define CLRPLOCK(x) ((x)->p_flags &= ~PFPLOCK)
#define CLRPLLOCK(x) ((x)->p_flags &= ~PFPLLOCK)
#define CLRCOPILOT(x) ((x)->p_flags &= ~PFCOPILOT)
#define CLRRHOSTILE(x) ((x)->p_flags &= ~PFRHOSTILE)
#define CLRRHARD(x) ((x)->p_flags &= ~PFRHARD)
#define CLRRSTICKY(x) ((x)->p_flags &= ~PFRSTICKY)
#define CLRSHOWSTATS(x) ((x)->p_flags &= ~PFSHOWSTATS)
#define CLRWAR(x) ((x)->p_flags &= ~PFWAR)
#define CLRENTER(x) ((x)->p_flags &= ~PFENTER)
#define CLRFOLLOW(x) ((x)->p_flags &= ~PFFOLLOW)
#define CLRWEPREL(x) ((x)->p_flags &= ~PFWEPREL)
#define CLRPATROL(x) ((x)->p_flags &= ~PFPATROL)
#define CLRASSIST(x) ((x)->p_flags &= ~PFASSIST)
#define CLRCALLEDHELP(x) ((x)->p_flags &= ~PFCALLEDHELP)
#define CLRKILLER(x) ((x)->p_flags &= ~PFKILLER)
#define CLRSHIELD(x) ((x)->p_flags &= ~PFSHIELD)
#define CLRREPAIR(x) ((x)->p_flags &= ~PFREPAIR)
#define CLRBOMB(x) ((x)->p_flags &= ~PFBOMB)
#define CLRORBIT(x) ((x)->p_flags &= ~PFORBIT)
#define CLRCLOAK(x) ((x)->p_flags &= ~PFCLOAK)
#define CLRWEP(x) ((x)->p_flags &= ~PFWEP)
#define CLRENG(x) ((x)->p_flags &= ~PFENG)
#define CLRROBOT(x) ((x)->p_flags &= ~PFROBOT)
#define CLRBEAMUP(x) ((x)->p_flags &= ~PFBEAMUP)
#define CLRBEAMDOWN(x) ((x)->p_flags &= ~PFBEAMDOWN)
#define CLRSELFDEST(x) ((x)->p_flags &= ~PFSELFDEST)
#define CLRTURBO(x) ((x)->p_flags &= ~PFTURBO)
#define CLRSNEAKY(x) ((x)->p_flags &= ~PFSNEAKY)

/* planet definitions */

#define ISFUELP(p) ((*p).pl_flags & PLFUEL)

/* syntactic sugar */

#define LOCAL static
#define GLOBAL
#define SQR(x) ((double)(x) * (x))

typedef int     (*PFUNC) ();

#define ToggleWindow(p, win)  \
  if (ismapped(p, p->win)) \
    XUnmapWindow(p->display, p->win); \
  else \
    XMapWindow(p->display, p->win)

/*  ((*MapFunc[ismapped(p, p->win) ? 1 : 0])(p->display, p->win)) */

/* global declarations */

#define OUTRANGE(x,y,d) \
  ((x) > (d) || (x) < -(d) || (y) > (d) || (y) < -(d) \
  || (double)(SQR(x) + SQR(y) > (double)SQR(d)))

#define INRANGE(x,y,d) (!OUTRANGE(x,y,d))

#define CHECKSPEC(x,mask) (((x) & (mask)) == (mask))


/* function declarations to keep gcc and lint happy */

#include <X11/Xos.h>
#include <math.h>

#ifndef FILE
#include <stdio.h>
#endif

extern double   hypot();		/* gcc math.h doesn't include */
extern void     bcopy(), bzero();
extern int      bcmp();
extern long     random();
extern void     exit();
extern int      ioctl();
extern int      read(), write(), ioctl();
extern void    *malloc(), *calloc();
extern void     free();
extern time_t   time();
extern int      select(), socket(), bind(), listen(), setsockopt();
extern int      setitimer(), accept();
extern unsigned char asintable[];
#include "decl.h"
