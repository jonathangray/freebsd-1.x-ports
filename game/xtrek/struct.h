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

#include <X11/Xlib.h>

struct status {
  int             active;
};

typedef
enum {
  PFree, POutfit, PAlive, PExplode, PDead, PSetup
}               PlayerStatus;

/* Return values from newwin(): */
#define	OK		1
#define	NODISPLAY	2
#define	NOFONTS		3


#define PFSHIELD	(1 <<  0)
#define PFREPAIR	(1 <<  1)
#define PFBOMB		(1 <<  2)
#define PFORBIT		(1 <<  3)
#define PFCLOAK		(1 <<  4)
#define PFWEP		(1 <<  5)
#define PFENG		(1 <<  6)
#define PFROBOT		(1 <<  7)
#define PFBEAMUP	(1 <<  8)
#define PFBEAMDOWN	(1 <<  9)
#define PFSELFDEST	(1 << 10)
#define PFPLOCK		(1 << 11)	/* Locked on a player */
#define PFPLLOCK	(1 << 12)	/* Locked on a planet */
#define PFCOPILOT	(1 << 13)	/* Allow copilots */
#define PFRHOSTILE	(1 << 14)	/* Hostile robot */
#define PFRHARD		(1 << 15)	/* hard robot */
#define	PFRSTICKY	(1 << 16)	/* sticky robot */
#define	PFSHOWSTATS	(1 << 17)	/* show player statistics */
#define	PFWAR		(1 << 18)	/* computer reprogramming for war */
#define	PFENTER		(1 << 19)	/* entry windows showing */
#define PFFOLLOW	(1 << 20)	/* follow a player */
#define PFWEPREL	(1 << 21)	/* weapons release */
#define PFPATROL	(1 << 22)	/* on patrol */
#define PFASSIST	(1 << 23)	/* assisting */
#define PFCALLEDHELP	(1 << 24)	/* called for help */
#define PFKILLER	(1 << 25)	/* killer robot */
#define PFTURBO 	(1 << 26)	/* turbo speed on */
#define PFSNEAKY	(1 << 27)	/* sneaky robot */
#ifdef KILLERROBOTS
#undef PFRHARD
#define PFRHARD		((1 << 15)|PFKILLER)	/* hard robot */
#endif
typedef
enum {
  KQuit, KTorp, KPhaser, KPlanet, KShip, KDaemon, KWinner
}               DeathReason;


#define	UMSGADDRLEN	10		/* user message address length */
#define	UMSGLEN		80		/* user message length */

typedef
enum {
  Green, Yellow, Red
}               AlertLevel;


/* subsystem numbers */

typedef enum {
  SRS, Cooling, Shield, Warp, Torp, Lock, LRS, Phaser, Detonate,
  Cloak, Teleport, Mine, Trans, Sentinel
}               System;

#define NSUBSYSTEMS ((int) Sentinel)
#define STAT_FAILED_POS 64
#define STATLINESIZE (STAT_FAILED_POS+NSUBSYSTEMS)


struct stats {
  int             st_id;		/* identifier for this stats record */
  int             st_time;		/* real time in game */
  int             st_cpu;		/* cpu time in game */
  double          st_kills;		/* how many kills */
  int             st_losses;		/* times killed */
  double          st_maxkills;		/* times killed */
  int             st_entries;		/* times in game */
  int             st_conqs;		/* times galaxy taken over */
  int             st_coups;		/* retaken home planet */
  int             st_torps;		/* torps launched */
  int             st_phasers;		/* phasers fired */
  int             st_armsbomb;		/* armies bombed */
  int             st_armsship;		/* ship board armies killed */
  int             st_planets;		/* planets conquered */
  int             st_genocides;		/* races genocided */
};


typedef struct ship {
  int             turns;
  int             accint;
  int             decint;
  int             phaserdamage;

  int             ph_pulses;
  int             phaserdist;
  int             torpdamage;
  int             minedamage;
  int             torpdrange;
  int             minedrange;
  int             torpspeed;
  int             repair;
  int             maxdamage;
  int             maxshields;
  int             reload;
  int             burst;

  int             recharge;
  int             wcool;
  int             ecool;

  int             maxspeed;
  int             maxarmies;
  int             maxfuel;
  int             cruise;
  int             hscruise;
  int             battle;
  int             bturn;
  int             flee;
  int             engage;
  int             disengage;
  int             shotdamage;
  int             circledist;
  int             cloaked;
  int             refresh;
  int             mincloak;
  int             maxcloak;
  int             sneaky;

  int             telrange;
  int             turbospeed;		/* Turbo-warp speed. */
  int             turbotime;		/* Clicks at turbo-speed. */
  int             fail[NSUBSYSTEMS];
  int             cost[NSUBSYSTEMS];
  int             heat[NSUBSYSTEMS];
}               aShip;



typedef struct player {
  int             p_no;
  int             p_updates;		/* Number of updates ship has survived */
  PlayerStatus    p_status;		/* Player status */
  u_long           p_flags;		/* Player flags */
  char            p_name[16];
  char            p_login[16];
  char            p_monitor[40];	/* Monitor being played on */
  char            p_mapchars[2];	/* Cache for map window image */
  int             p_lv;			/* last visible in clock ticks */
  int             p_x;
  int             p_lvx;		/* last visible x */
  int             p_y;
  int             p_lvy;		/* last visible y */
  int             p_desx;		/* destination x */
  int             p_desy;		/* destination y */
  int             p_despl;		/* destination planet */
  float           p_kills;		/* Enemies killed */
  u_char           p_dir;		/* Real direction */
  u_char           p_lvdir;		/* last visible dir */
  u_char           p_desdir;		/* desired direction */
  u_long           p_hostile;		/* Who my torps will hurt */
  u_long           p_swar;		/* Who am I at sticky war with */
  u_short          p_subdir;		/* fraction direction change */
  short           p_speed;		/* Real speed */
  short           p_lvspeed;		/* last visible speed */
  short           p_desspeed;		/* Desired speed */
  short           p_subspeed;		/* Fractional speed */
  short           p_empire;		/* Team I'm on */
  u_long           p_mask;		/* Team bit mask */
  int              p_damage;		/* Current damage */
  int             p_shield;		/* Current shield power */
  short           p_ntorp;		/* Number of torps flying */
  short           p_nmine;		/* Numbers of mines droped */
  short           p_planet;		/* Planet orbiting or locked onto */
  short           p_playerl;		/* Player locked onto */
  int             p_newdamage;
  short           p_armies;
  short           p_fuel;
  short           p_explode;		/* Keeps track of final explosion */
  short           p_etemp;
  short           p_etime;
  short           p_wtemp;
  short           p_wtime;
  short           p_timer;		/* General purpose timer */
  short           p_rmode;		/* Robot attack mode */
  DeathReason     p_whydead;		/* Tells you why you died */
  short           p_whodead;		/* Tells you who killed you */
  short           p_selfdest;		/* How long until I die. */
  short           p_sysdamage[NSUBSYSTEMS];
  short           p_reload;		/* time to load next torp */
  AlertLevel      p_alert;		/* alert level, R, G, Y */
  AlertLevel      oldalert;
  aShip           ship;
  int             turboclicks;		/* # time clicks left in turbo-speed. */
  int             mapmode;
  int             namemode;
  int             messpend;
  int             lastcount;
  int             mdisplayed;
  int             redrawall;
  int             watch;
  int             selfdest;
  int             statmode;
  int             showShields;
  int             showStats;
  int             infomapped;		/* NOTE: Only use one of these? */
  int             infow_on;		/* NOTE: Only use one of these? */
  int             ts_offset;
  int             lastm;
  int             warntimer;
  int             warncount;
  int             newhostile;
  int             reprogram;
  int             delay;
  struct stats    p_stats;		/* player statistics */
  struct usermsg {			/* User message */
    char            m_pending;
    int             m_addr;
    char            m_addrmsg[UMSGADDRLEN];
    char            m_buf[UMSGLEN];
    int             m_lcount;
  }               p_umsg;
  int             copilot;
  long            start_time;
  char            last_msg[80];
  char		  UsingX;

  /* X11 stuff: */
  Display        *display;
  int             depth;		/* DefaultDepth */
  int             screen;
  int             xcn;			/* XConnectionNumber */
  int             mono;
  XFontStruct    *dfont,		/* normal font */
                 *bfont,		/* bold font */
                 *ifont,		/* italic font */
                 *xfont,		/* xtrek font */
                 *bigFont;		/* big font in choose */
  Window          w,			/* big window */
                  mapw, statwin, baseWin, messagew, infow, iconWin, tstatw, war, warnw, helpWin, planetw, playerw, qwin, waremp[MAXEMPIRES + 2],	/* wargo, warno hanging off */
                  win[MAXEMPIRES];
  GC              gc,			/* normal gc */
                  bmgc,			/* base window background gc */
                  cleargc, dfgc,	/* default font gc */
                  bfgc,			/* bold font gc */
                  ifgc,			/* italic font gc */
                  bFgc,			/* big font gc */
                  xfgc,			/* xtrek font gc */
                  monogc;		/* mono ship gc */

  u_long           borderColor, backColor, textColor, myColor, warningColor, shipCol[MAXEMPIRES], aColor[3], unColor;
  XRectangle      clearzone[(MAXTORP + 1) * MAXPLAYER + MAXPLANETS];
  int             clearcount;
  int             clearline[4][MAXPLAYER];
  int             clearlcount;
  XRectangle      mclearzone[MAXPLAYER + MAXPLANETS * 2];
  int             mclearcount;
  int             statX;
  int             statY;
  int             uspec;
  GC              sgc;
  Pixmap          foreTile, backTile, aTile[3], stippleTile;
  Pixmap          Fibm;			/* Icon bitmap */
  Pixmap          tbm;
  Pixmap          shippics[MAXEMPIRES][VIEWS];	/* ship pictures */
  Window          clockw;
  int             once;
  int             oldtime;
  long            startTime;
  short           mustexit;
  short           do_bell;		/* bellmode boolean */
  short           fracwarp;		/* next digit is fractional warp
					   desired */
  short           p_oldshield;		/* crap for the sliding scale display */
  short           p_olddamage;		/* crap for the sliding scale display */
  short           p_oldfuel;		/* crap for the sliding scale display */
  short           p_oldspeed;		/* crap for the sliding scale display */
  short           p_oldwtemp;		/* crap for the sliding scale display */
  short           p_oldetemp;		/* crap for the sliding scale display */
}               aPlayer;

/* Torpedo states */

typedef enum {
  TFree, TMove, TExplode, TDet, TOff, TMine
}               TorpState;


#define NORMTorp 0
#define MINETorp 1

typedef struct torp {
  TorpState       t_status;		/* State information */
  short           t_damage;		/* damage for direct hit */
  int		  t_x,t_dx;		/* torp location and delta */
  int             t_y,t_dy;
  short           t_range;		/* minimum deton range */
  short           t_owner;
  short           t_speed;		/* Moving speed */
  short           t_fuse;		/* Life left in current state */
  u_char           t_dir;		/* direction */
  u_long           t_war;		/* enemies */
  u_long           t_mask;		/* launching empire */
  char            t_type;		/* type of torp it is, mine, etc. */
}               aTorp;


typedef enum {
  PhFree, PhHit, PhMiss, PhNew, PhCool
}               PhaserState;

struct phaser {
  PhaserState     ph_status;		/* What it's up to */
  short           ph_pulses;		/* # of pulses left */
  short           ph_target;		/* Who's being hit (for drawing) */
  u_char           ph_dir;		/* direction */
  char            ph_cool;		/* cool off cycle */
};


/*
 * An important note concerning planets:  The game assumes that the planets
 * are in a 'known' order.  Ten planets per empire, the first being the home
 * planet.
 */

/* the lower bits represent the original owning empire */
/* this may not be true anymore */

#define PLREPAIR (1 << (0+4))
#define PLFUEL   (1 << (1+4))
#define PLREDRAW (1 << (2+4))		/* Player close for redraw */
#define PLHOME   (1 << (3+4))		/* home planet for a given empire */
#define PLCOUP   (1 << (4+4))		/* Coup has occured */

typedef struct planet {
  int             pl_no;
  u_long           pl_flags;		/* State information */
  int             pl_owner;		/* Current owner of planet. */
  int             pl_orig_owner;	/* Original owner of planet (for
					   coup). */
  int             pl_x;
  int             pl_y;
  char            pl_name[16];
  int             pl_namelen;		/* Cuts back on strlen's */
  int             pl_armies;
  int             pl_info;		/* Teams which have info on planets */
  int             pl_deadtime;		/* Time before planet will support
					   life */
  int             pl_couptime;		/* Time before coup may take place */
}               aPlanet;

#define MVALID 		(1 << 0)
#define MINDIV 		(1 << 1)
#define MEMPIRE  	(1 << 2)
#define MALL  		(1 << 3)

typedef struct message {
  int             m_no;
  u_long          m_flags;
  int             m_time;
  int             m_recpt;
  char            m_from[3];
  char            m_to[3];
  char            m_data[80];
}               aMessage;

/* message control structure */

struct mctl {
  int             mc_current;
};

/* This is a structure used for objects returned by mouse pointing */

#define PLANETTYPE 0x1
#define PLAYERTYPE 0x2

struct obtype {
  int             o_type;
  int             o_num;
};

typedef struct slider {
  char           *label;
  short           min, max;
  short           low_red, high_red;
  short           label_length;
  short           diff;
  short		  scale;
}               aSlider;

typedef struct empirestats {
  double          kills;
  double          maxkills;
  int             losses;
  int             torps;
  int             phasers;
  int             planets;
  int             armsbomb;
}               aEmpireStats;

typedef struct empire {
  char            name[50];
  char            abbrev[16];
  char            robotname[40];
  char            code;
  int             namelen;
  int             centerx;
  int             centery;
  char            iconname[100];
  aEmpireStats    stats;
}               aEmpire;
