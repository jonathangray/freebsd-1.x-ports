/*
 * Copyright (C) 1990 Regents of the University of California.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of the University of
 * California not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  the University of California makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 */

# include <X11/Intrinsic.h>
# include <cdaudio.h>
# include <audio.h>

# define NOTITLESTR	"No Title"
# define NODISCSTR	"No Disc"

# define VAL2PCT(a)	(double)(1.0 - 1.0 * (exp((-7.0/255.0) * a)))
# define PCT2VAL(a)	(int)((-255.0/7.0) * log(1.0 * (1.0 - a)))

# define bit(n)			(1 << (n))

/* bits for cdrom_state */
# define CDROM_STATE_PLAY	bit(0)
# define CDROM_STATE_PAUSE	bit(1)
# define CDROM_STATE_STOP	bit(2)
# define CDROM_STATE_EJECTED	bit(3)
# define CDROM_STATE_CYCLE	bit(4)
# define CDROM_STATE_SHUFFLE	bit(5)
# define CDROM_STATE_PROGRAM	bit(6)

/* return codes from cdrom_status() */
# define CDROM_INVALID		CD_STILL
# define CDROM_PLAYING		CD_PLAYING
# define CDROM_PAUSED		CD_PAUSED
# define CDROM_COMPLETED	CD_READY
# define CDROM_ERROR		CD_ERROR
# define CDROM_NO_STATUS	CD_NODISC
# define CD_ABORT		0xfe

#define CDROM_LEADOUT   0xAA
#define CDROM_MSF	0x02
#define	STILL_MOUNTED	1
#define	UNMOUNTED	0
#define SAMPLES_PER_FRAME (CDDA_DATASIZE/2)

struct cdrom_tocentry {
	unsigned char cdte_track;
	unsigned char cdte_adr  :4;
	unsigned char cdte_ctrl :4;
	unsigned char cdte_format;
	union {
		struct {
			int minute;
			int second;
			int frame;
		} msf;
		int lba;
	} cdte_addr;
	unsigned char cdte_datamode;
};

struct msf {
	int   minute;
	int   second;
	int   frame;
};

struct prognode {
	unsigned char	track;
	Widget		button;
	struct prognode *next;
	struct prognode *prev;
};

typedef struct _cdrom_info {
	unsigned char	curtrack;	/* current track playing	*/
	unsigned char	mintrack;	/* first audio track		*/
	unsigned char	maxtrack;	/* last audio track		*/
	unsigned char	ntracks;	/* size of random track list	*/
	int		duration;	/* seconds played so far	*/
	int		state;		/* state of cd-rom drive	*/
	short		currand;	/* index into random track list */
	short		lastprog;	/* number of selections in prog.*/
	unsigned short	*times;		/* duration of each track	*/
	struct msf 	*addrs;		/* starting minute/second/frames*/
	struct prognode	*selection;	/* currently selected prog. trk	*/
	struct prognode *program;	/* list of programmed tracks	*/
	int		scsi_audio;	/* true if scsi audio is avail. */
} cdrom_info;

/* The below is a repeat of the above for SGIs using scsi_audio (the	*/
/* CD* calls don't return the info anymore). 				*/
typedef struct _cdrom_audio_info {
	unsigned char	curtrack;
	int		duration;
	int		state;
	int		start_track;	/* Start track for play		*/
	int		end_track;	/* End track for play		*/
	int		child_pid;	/* PID of child process		*/
} cdrom_audio_info;

struct _cdrom_shmem {
	cdrom_audio_info	cdrom_audio_cdi;
	struct msf		cdrom_audio_msf;
};

extern int		cdrom_open();
extern void		cdrom_close();
extern int		cdrom_start();
extern int		cdrom_stop();
extern int		cdrom_eject();
extern int		cdrom_pause();
extern int		cdrom_resume();
extern int		cdrom_volume();
extern int		cdrom_get_times();
extern int		cdrom_get_curtrack();
extern int		cdrom_get_msf();
extern int		cdrom_get_curmsf();
extern int		cdrom_play_track();
extern int		cdrom_play_msf();
extern int		cdrom_read_tocentry();
extern int		cdrom_read_tochdr();
extern int		cdrom_status();

extern cdrom_info	cdi;
extern char		*disc_title;
extern char		program_str[];
extern char		*cdInfoDir;

