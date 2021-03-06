/*
 * $Id: ct.h,v 1.1 1993/08/17 09:42:28 alm Exp $
 */
/*
 * ct.h - header file for calentool
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Original suntool source Copyright (C) 1987, Sun Microsystems, Inc.
 * 	All Rights Reserved
 * Permission is hereby granted to use and modify this program in source
 * or binary form as long as it is not sold for profit and this copyright
 * notice remains intact.
 * Original author: Philip Heller, Sun Microsystems, Inc.
 * 
 * All additional software, enhancements and modifications are
 * Copyright 1988, 1989, 1991 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 

/*
 * If calentool is too big and you want a stripped-down version
 * define some or all of these here or in the Makefile. Combined,
 * they save ~100K bytes for a statically linked object.
 */
/*#define NO_PRINTER		/* exclude printer support code */
/*#define NO_HOLIDAYS		/* exclude holiday checking code */
/*#define NO_SUN_MOON		/* exclude Sun/Moon data frames */

/* ignore several things for calencheck program */
#ifdef CALENCHECK
#	define NO_PRINTER
#	define NO_HOLIDAYS
#	define NO_SUN_MOON
#	define NOTOOL
#endif

/* directory for date/event files */
#ifndef DATELIB_DIR
#	define DATELIB_DIR	"/usr/openwin/lib/calentool"
#endif

#ifndef NO_PRINTER
/* command string for sending a file to the Postscript printer */
#	ifndef PRINT_CMD
#		define PRINT_CMD	"lpr -Plw"
#	endif

/*#	define RASTER_ONLY	/* define this if no PostScript printer available */
#	ifndef PS_NOTE_FONT
#		define PS_NOTE_FONT     "Helvetica-Narrow"
#	endif
/*#	define LANGUAGE		/* for natural language support */
				/* see the file pcaldw.c for details */
#else
#	define RASTER_ONLY	/* do not change this line */
#endif /* NO_PRINTER */

#ifndef MAILPROG
#	define MAILPROG		"/usr/ucb/mail"
				/* assumes -s option is available */
#endif

/* define NR_WEEKDAYS for desired week display */
/* NR_WEEKDAYS		display   */
/*	5		Mon-Fri   */
/*	6		Mon-Sat	  */
/*	7		Sun-Sat	or Mon-Sun  */
/**/
#ifndef NR_WEEKDAYS
#	define NR_WEEKDAYS	5
#endif
#ifndef MON_FIRST
#	define MON_FIRST	0	/* 0=Sun-Sat, 1=Mon-Sun */
#endif

#ifndef START_HOUR
#	define START_HOUR	8	/* 8am */
#endif					/*  to */
#ifndef END_HOUR
#	define END_HOUR		18	/* 6pm */
#endif
#ifndef HOUR_24
#	define HOUR_24		0	/* 0=12hr time, 1=24hr time */
#endif
#ifndef DAY_FIRST
#	define DAY_FIRST	0	/* 0=M/D/Y, 1=D/M/Y */
#endif

#ifndef START_YEAR
#	define START_YEAR	90
#endif
#ifndef NR_YEARS
#	define NR_YEARS		5	/* number of years in menu */
#endif

#ifndef N_NOTESLOTS
#	define N_NOTESLOTS	10	/* number of slots for notes */
#endif

#ifndef UPDATE_RATE
#	define UPDATE_RATE	"second"	/* update time */
#endif 					/* options are "second" & "minute" */
#define TIME_OUT	2		/* check appts every 2 minutes */

/*
 * APPT_CHECK_LIMIT is typically either "n_tslots"
 * or "n_slots" depending on whether we include the
 * notes section when indicating that we still have
 * appts today.
 */
#ifndef APPT_CHECK_LIMIT
#	define APPT_CHECK_LIMIT	n_tslots
#endif

/*
 * Natural language support for month and day names
 */
/*#define FRENCH */

/*********************************************************************/
/* Nothing below here should need to be changed (typically)          */
/*********************************************************************/

#ifndef TRUE
#	define TRUE	1
#endif
#ifndef FALSE
#	define FALSE	0
#endif

#define MAX_FUTURE_ENTRIES	32	/* number of appts displayed in popup window */

/* Dimensions of 30-minute week slot.
 * Message size determines width - everything else keyed
 * off font size and message size
 */
#define WEEK_MESSAGE_SIZE	12

#define MAX_INCLUDE_NESTING	4	/* number of allowed include files */

#define DISPLAYING_DAY          1       /* Defs for state of main */
#define DISPLAYING_WEEK         2	/* subwindow (mainsw_state) */
#define DISPLAYING_MONTH        3
#define DISPLAYING_YEAR         4

#define BACKSPACE               8	/* editing chars */
#define CTRL_R                  18
#define CTRL_U                  21
#define CTRL_W                  23
#define DEL                     127

#define NONE			0
#define DAY			1	/* Defs for "selected_type" (type of */
#define WEEK			2	/* object selected by month click). */
#define MONTH			3

#define JUSTIFY_LEFT	0		/* text justification in day slot */
#define JUSTIFY_RIGHT	1
#define JUSTIFY_INDEX	2

#define FOUND_SLOT	1		/* event tags for day display */
#define FOUND_MORE	2
#define FOUND_LARROW	3
#define FOUND_RARROW	4

#define MMODIFY		1		/* order of entries in day menu */
#define MCUT		2
#define MPASTE		3
#define MCOPY		4
#define MDELETE		5
#define MUNDELETE	6

#define PR_DEFAULT	0		/* print options */
#define PR_ASCII	1
#define PR_POSTSCRIPT	2
#define PR_RASTER	3

#define SUN		0		/* days of the week */
#define MON		1
#define TUE		2
#define WED		3
#define THU		4
#define FRI		5
#define SAT		6

#define JAN		0		/* selected months */
#define FEB		1
#define DEC		11

#define ALL_YEARS	0x1		/* flags for special case appts */
#define ALL_MONTHS	0x2
#define ALL_DAYS	0x4
#define REPEAT		0x8
	/* check for any repeating type appt */
#	define Repeating(d)	((d)&(ALL_YEARS|ALL_MONTHS|ALL_DAYS|REPEAT|EVERY_MON_FRI))
#define A_COMMENT	0x10
#define EVERY_SUN	0x20
#define EVERY_MON	0x40
#define EVERY_TUE	0x60
#define EVERY_WED	0x80
#define EVERY_THU	0xa0
#define EVERY_FRI	0xc0
#define EVERY_SAT	0xe0
#define	EVERY_SOMEDAY	0xe0
	/* convert flag value to day-of-week number */
#	define Pickday(d)	((((d)&EVERY_SOMEDAY)>>5)-1)	
	/* convert day of week to flag value */
#	define Setday(d)	(((d)+1)<<5)
#define LOOKAHEAD	0x100
#define READONLY	0x200
#define A_NOTE		0x400
#define MARKED		0x800	/* don't show in month/year display */
#define MARKED_NOTE	0xc00
#define DELETED		0x1000	/* don't show the appt that matches this */
#define RUN		0x2000
#define EVERY_MON_FRI	0x4000

/* format of repeat field for every_someday type appts */
#define WEEK1		0x1
#define WEEK2		0x2
#define WEEK3		0x4
#define WEEK4		0x8
#define WEEK5		0x10
#define ALL_WEEKS	0x1f
#define LAST_WEEK	0x20
#define WEEK_LIMIT	0x3f

/* error reporting flags */
#define NON_FATAL	0
#define FATAL		1

/* icon identifiers */
#define STD_ICON	0		/* flags for icon currently displayed */
#define REV_ICON	1
#define NA_ICON		2

/* arguments to print_apts() */
#define PRI_DAY			1
#define PRI_WEEK		2
#define PRI_MONTH		3
#define PRI_XNOTES		4
#define PRI_DAY_XNOTES		(PRI_DAY|PRI_XNOTES)
#define PRI_WEEK_XNOTES		(PRI_WEEK|PRI_XNOTES)
#define PRI_MONTH_XNOTES 	(PRI_MONTH|PRI_XNOTES)
#define DST_STDOUT		1
#define DST_MAIL		2

/* return codes from get_day_appts() */
#define NO_ENTRIES	0
#define SOME_APPTS	1
#define SOME_NOTES	2
#define SOME_MKNOTES	4
#define SOME_FUTURES	8

/* header line in appts file implies one-based entries and 99 memo flag */
#define HEADER		"# CalenTool V2.2 - DO NOT REMOVE THIS LINE\n"
#define OHEADER		"# CalenTool V2 - DO NOT REMOVE THIS LINE\n"

#define MAX_STRLEN	256	/* max size of strings */
struct appt_entry {
	/* describes an entry in the appointments file */
	int year, month, day, hour, minute, arrows;
	int repeat, lookahead, flags, sindex;
	int runlength, warn;
	char str[MAX_STRLEN];
	struct appt_entry *next;	/* ptr to next appt in list */
};					/* NULL if last entry */

struct rect_pos {
	/* location of a given rect in the canvas */
	int top, bottom, left, right;
};

struct dayslot {
	/* struct for storing relevant info for 30 min day slot */
	struct rect_pos slot_pos; /* slot position in day display */
	struct rect_pos larrow_pos; /* position of left scroll arrow */
	struct rect_pos rarrow_pos; /* position of right scroll arrow */
	struct rect_pos moreb_pos; /* position of "more" button */
	int active;	/* number of appts that originate here */
	int count;	/* number of appts in this slot */
	int arrow_pos;	/* bitmap of arrow locations on screen */
	struct appt_entry *cur_appt;	/* ptr to current appt in list */
	struct appt_entry *first;	/* ptr to first appt in list */
};					/* NULL if no entries */

struct weekrect {
	/* info for week display */
	struct rect_pos wday_pos; /* screen position of this day */
	struct rect_pos moreb_pos; /* position of "more" button */
	int more;
	struct dayslot *weekslots; /* array of slots for this day */
};

struct rect_limits {
	int lowx;
	int lowy;
	int highx;
	int highy;
};

struct week_arrow {
	int top;
	int bottom;
	int left;
	int right;
	int active;
};
