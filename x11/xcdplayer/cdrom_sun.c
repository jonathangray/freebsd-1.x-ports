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

static int c;

# if defined(sun)
# include <stdio.h>

# include <sys/file.h>
# include <sys/types.h>
# include <sys/param.h>
# include <sys/stat.h>

# include <sun/dkio.h>

# include <mntent.h>
# include <string.h>

# include <sys/buf.h>
# ifdef sun4c
# include <scsi/targets/srdef.h>
# else
# include <sundev/srreg.h>
# endif

# include <sys/time.h>

# include "debug.h"
# include "cdrom_sun.h"

extern char	*device;
# if defined(notdef)
extern void	cdrom_print_toc();
# endif /* defined(notdef) */

static char	cdrom[] =	"/dev/rsr0";

cdrom_info	cdi;
char		info_filename[256];
FILE		*disc_info = NULL;

static int	cdrom_fd = -1;

get_stored_info()
{
	int i,n;
	char	line[100];
	char	*title;

        if ( cdi.maxtrack == 0) {
                return(0);
        }
	for (i = 0, n = 0; i < cdi.maxtrack; i++)
	    n = n + ((i+1) * cdi.times[i]);
	n = n / cdi.maxtrack;

        disc_title = NULL;
	if (cdInfoDir != NULL)
	    sprintf(info_filename, "%s/cd.%d", cdInfoDir, n);
	else
	    sprintf(info_filename, "cd.%d", n);

	if ((disc_info = fopen(info_filename, "r")) != NULL)
	{
	    fgets(line, 100, disc_info);
	    title = strchr(line, ':');
	    if (title != NULL)
	    {
	    	*(strchr(title, '\n')) = '\0';
	        disc_title = strdup(title + 1);
	    }
	    fgets(line, 100, disc_info);
	    sscanf(line, "Program: %s", program_str);
	}
        if (disc_title == NULL) {
                disc_title = NOTITLESTR;
        }
}

int
cdrom_open() {
	int	n;
	extern void update_title();

	if (cdrom_fd != -1)
		return(cdi.curtrack);

	if (device != NULL) {
		if ((cdrom_fd = open(device, O_RDONLY)) == -1) {
			debug-printf(stderr, "open: ");
			return(-1);
		}
	} else {
		if ((cdrom_fd = open(cdrom, O_RDONLY)) == -1) {
			debug-printf(stderr, "open: ");
			return(-1);
		}
	}

	if (cdrom_get_times() == -1) {
		cdrom_close();
		return(-1);
	}

	if ((n = cdrom_get_curtrack()) == -1)
		return(-1);

	get_stored_info();

	update_title();

	if (cdi.state & CDROM_STATE_PLAY)
		cdi.curtrack = n;

	if (cdi.state & CDROM_STATE_SHUFFLE)
		shuffle_setup();

	return(cdi.curtrack);
}

void
cdrom_close() {
	if (cdrom_fd == -1)
		return;

	if (cdi.times != NULL) {
		free((char *) cdi.times);
		free((char *) cdi.addrs);
		cdi.times = NULL;
		cdi.addrs = NULL;
	}

	(void) close(cdrom_fd);
	cdrom_fd = -1;
}


int
cdrom_start() {
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMSTART) == -1) {
		perror("ioctl(cdromstart)");
		return(-1);
	}

	return(0);
}

int
cdrom_stop() {
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMSTOP) == -1) {
		perror("ioctl(cdromstop)");
		return(-1);
	}

	return(0);
}

int
cdrom_eject() {
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMEJECT) == -1) {
		perror("ioctl(cdromeject)");
		return(-1);
	}

	return(0);
}

int
cdrom_pause() {
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMPAUSE) == -1) {
		perror("ioctl(cdrompause)");
		return(-1);
	}

	return(0);
}

int
cdrom_resume() {
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMRESUME) == -1) {
		perror("ioctl(cdromresume)");
		return(-1);
	}

	return(0);
}

int
cdrom_volume(left_vol, right_vol)
	int			left_vol;
	int			right_vol;
{
	struct cdrom_volctrl	vol;

	if (cdrom_fd == -1)
		return(-1);

	vol.channel0 = left_vol;
	vol.channel1 = right_vol;

	if (ioctl(cdrom_fd, CDROMVOLCTRL, &vol) == -1) {
		perror("ioctl(cdromvolctrl)");
		return(-1);
	}

	return(0);
}

int
cdrom_get_times() {
	struct cdrom_tochdr	tochdr;
	extern unsigned short	*ushort_malloc();
	extern struct msf	*msf_malloc();
	unsigned long		trk, trk_total, otime;
	struct msf		msf;

	if (cdrom_read_tochdr(&tochdr) == -1)
		return(-1);

	cdi.mintrack = tochdr.cdth_trk0;
	cdi.maxtrack = tochdr.cdth_trk1;

	if (cdi.times != NULL)
	{
		free((char *) cdi.times);
		free((char *) cdi.addrs);
		cdi.times = NULL;
		cdi.addrs = NULL;
	}

	cdi.times = ushort_malloc(cdi.maxtrack - cdi.mintrack + 1);
	cdi.addrs = msf_malloc(cdi.maxtrack - cdi.mintrack + 2);

	otime = 0;

	for (trk = cdi.mintrack; trk <= cdi.maxtrack; trk++) {
		if (cdrom_get_msf(trk, &msf, &trk_total) == -1)
			return(-1);

		/* record start address for each track (track 1 starts at 0)*/
		cdi.addrs[trk - cdi.mintrack] = msf;

		trk_total -= otime;

		/* use start time of next track as length of previous */
		if (otime != 0) 
		{
			cdi.times[trk - cdi.mintrack - 1] = trk_total;
		}

		otime += trk_total;

	}

	/* find start of  leadout to get length of last track */
	if (cdrom_get_msf(CDROM_LEADOUT, &msf, &trk_total) == -1)
		return(-1);

	/* recode leadout start address */
	cdi.addrs[trk - cdi.mintrack] = msf;
	trk_total -= otime;
	otime += trk_total;

	cdi.times[trk - cdi.mintrack - 1] = trk_total;

	return(0);
}

# if defined(notdef)
static void
cdrom_print_toc() {
	unsigned long		trk, trk_total;

	for (trk = cdi.mintrack; trk <= cdi.maxtrack; trk++) {
		trk_total = cdi.times[trk - cdi.mintrack];
		debug_printf(1, "%02u:%02u\n", trk_total/60, trk_total%60);
	}
}
# endif /* defined(notdef) */

int
cdrom_get_curtrack() {
	struct cdrom_subchnl	subchnl;

	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);
		return(-1);
	}

	switch (subchnl.cdsc_audiostatus) {
		case CDROM_AUDIO_INVALID:
		return(-1);

		/* playing track subchnl.cdsc_trk */
		case CDROM_AUDIO_PLAY:
		return((int) subchnl.cdsc_trk);

		/* paused on track subchnl.cdsc_trk */
		case CDROM_AUDIO_PAUSED:
		return((int) subchnl.cdsc_trk);

		/* punt */
		case CDROM_AUDIO_COMPLETED:
		return(0);

		case CDROM_AUDIO_ERROR:
		return(-1);

		/* punt */
		case CDROM_AUDIO_NO_STATUS:
		debug_printf(1, "cdrom_get_curtrack: no status\n");
		return(0);
	}

	/* bad value in cdsc_audiostatus */
	return(-1);
}

int
cdrom_get_msf(track, msf, length)
	unsigned long		track;
	struct msf		*msf;
	unsigned long		*length;
{
	struct cdrom_tocentry	tocentry;

	if (cdrom_read_tocentry(track, &tocentry) == -1)
		return(-1);

	msf->minute = tocentry.cdte_addr.msf.minute;
	msf->second = tocentry.cdte_addr.msf.second;
	msf->frame = tocentry.cdte_addr.msf.frame;

	*length = ((int) tocentry.cdte_addr.msf.minute * 60) +
		(int) tocentry.cdte_addr.msf.second;

	return(0);
}

int
cdrom_get_curmsf(msf)
	struct msf *msf;
{
	struct cdrom_subchnl	subchnl;

	if (cdrom_fd == -1)
		return(-1);

	subchnl.cdsc_format = CDROM_MSF;

	if (ioctl(cdrom_fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		perror("ioctl(cdromsubchnl)");
		return(-1);
	}

	msf->minute = subchnl.cdsc_absaddr.msf.minute;
	msf->second = subchnl.cdsc_absaddr.msf.second;
	msf->frame = subchnl.cdsc_absaddr.msf.frame;

	return (0);

}

int
cdrom_play_track(start_track, end_track)
	unsigned char		start_track;
	unsigned char		end_track;
{
	struct	cdrom_ti	ti;

	if (cdrom_fd == -1)
		return(-1);

	ti.cdti_trk0 = start_track;
	ti.cdti_ind0 = 1;
	ti.cdti_trk1 = end_track;
	ti.cdti_ind1 = 1;

	if (ioctl(cdrom_fd, CDROMPLAYTRKIND, &ti) == -1) {
		perror("ioctl(cdromplaytrkind)");
		return(-1);
	}

	return(0);
}

int
cdrom_play_msf(start_msf, end_msf)
	struct msf	*start_msf;
	struct msf	*end_msf;
{
	struct	cdrom_msf	play_addr;

	if (cdrom_fd == -1)
		return(-1);

	play_addr.cdmsf_min0 = start_msf->minute;
	play_addr.cdmsf_sec0 = start_msf->second;
	play_addr.cdmsf_frame0 = start_msf->frame;
	play_addr.cdmsf_min1 = end_msf->minute;
	play_addr.cdmsf_sec1 = end_msf->second;
	play_addr.cdmsf_frame1 = end_msf->frame;

	if (ioctl(cdrom_fd, CDROMPLAYMSF, &play_addr) == -1) {
		perror("ioctl(cdromplaymsf)");
		return(-1);
	}

	return(0);
}
int
cdrom_read_tocentry(track, tocentry)
	unsigned int		track;
	struct cdrom_tocentry	*tocentry;
{
	if (cdrom_fd == -1)
		return(-1);

	tocentry->cdte_track = track;
	tocentry->cdte_format = CDROM_MSF;

	if (ioctl(cdrom_fd, CDROMREADTOCENTRY, (char *) tocentry) == -1) {
		perror("ioctl(cdromreadtocentry)");
		return(-1);
	}

	return(0);
}

int
cdrom_read_tochdr(tochdr)
	struct cdrom_tochdr	*tochdr;
{
	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMREADTOCHDR, (char *) tochdr) == -1) {
		debug-printf(stderr,"ioctl(cdromreadtochdr): ");
		/* perror("ioctl(cdromreadtochdr)"); */
		return(-1);
	}

	return(0);
}

int
cdrom_status() {
	struct cdrom_subchnl	subchnl;

	if (cdrom_fd == -1)
		return(-1);

	if (ioctl(cdrom_fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);
		exit(1);
	}

	switch (subchnl.cdsc_audiostatus) {
		case CDROM_AUDIO_INVALID:
		return(CDROM_INVALID);

		case CDROM_AUDIO_PLAY:
		return(CDROM_PLAYING);

		case CDROM_AUDIO_PAUSED:
		return(CDROM_PAUSED);

		case CDROM_AUDIO_COMPLETED:
		return(CDROM_COMPLETED);

		case CDROM_AUDIO_ERROR:
		return(CDROM_ERROR);

		case CDROM_AUDIO_NO_STATUS:
		return(CDROM_NO_STATUS);
	}

	return(-1);
}

# if defined(notused)
int
cdrom_playing(track)
	int			*track;
{
	struct cdrom_subchnl	sc;

	if (cdrom_fd == -1)
		return(-1);

	sc.cdsc_format = CDROM_MSF;
	if (ioctl(cdrom_fd, CDROMSUBCHNL, &sc) == -1) {
		perror("ioctl(cdromsubchnl)");
		return(-1);
	}

	*track = sc.cdsc_trk;

	if (sc.cdsc_audiostatus == CDROM_AUDIO_PLAY)
		return(1);

	return(0);
}
# endif /* defined(notused) */

# if defined(notused)
int
cdrom_paused(track)
	int			*track;
{
	struct cdrom_subchnl	sc;

	if (cdrom_fd == -1)
		return(-1);

	sc.cdsc_format = CDROM_MSF;
	if (ioctl(cdrom_fd, CDROMSUBCHNL, &sc) == -1) {
		perror("ioctl(cdromsubchnl)");
		return(-1);
	}

	*track = sc.cdsc_trk;

	if (sc.cdsc_audiostatus == CDROM_AUDIO_PAUSED)
		return(1);

	return(0);
}
# endif /* defined(notused) */

# if defined(notused)
int
mounted(name)
	char		*name;
{
	char		buf[MAXPATHLEN], *cp;
	struct stat	st;
	dev_t		bdevno;
	FILE		*fp;
	struct mntent	*mnt;

	/*
	 * Get the block device corresponding to the raw device opened,
	 * and find its device number.
	 */
	if (stat(name, &st) != 0) {
		(void) fprintf(stderr, "stat: ");
		perror(name);
		return(UNMOUNTED);
	}

	/*
	 * If this is a raw device, we have to build the block device name.
	 */
	if ((st.st_mode & S_IFMT) == S_IFCHR) {
		if ((cp = strchr(name, 'r')) != NULL)
			cp++;

		(void) sprintf(buf, "/dev/%s", cp);
		if (stat(buf, &st) != 0) {
			(void) fprintf(stderr, "stat: ");
			perror(buf);
			return(UNMOUNTED);
		}
	}

	if ((st.st_mode & S_IFMT) != S_IFBLK)
		return(UNMOUNTED);

	bdevno = st.st_rdev & (dev_t)(~0x07);	/* Mask out partition. */

	/*
	 * Now go through the mtab, looking at all hsfs filesystems.
	 * Compare the device mounted with our bdevno.
	 */
	if ((fp = setmntent(MOUNTED, "r")) == NULL) {
		(void) fprintf(stderr, "couldn't open %s\n", MOUNTED);
		return(UNMOUNTED);
	}

	while ((mnt = getmntent(fp)) != NULL) {
		/* avoid obvious excess stat(2)'s */
		if (strcmp(mnt->mnt_type, "hsfs") != 0)
			continue;

		if (stat(mnt->mnt_fsname, &st) != 0)
			continue;

		if (((st.st_mode & S_IFMT) == S_IFBLK) &&
		    ((st.st_rdev & (dev_t)(~0x07)) == bdevno)) {
			(void) endmntent(fp);
			return(STILL_MOUNTED);
		}
	}

	(void) endmntent(fp);

	return(UNMOUNTED);
}
# endif /* defined(notused) */

unsigned short *
ushort_malloc(n)
	int		n;
{
	extern char	*calloc();
	unsigned short	*ptr;

	ptr = (unsigned short *) calloc(n, sizeof(unsigned short));
	if (ptr == NULL) {
		perror("calloc");
		exit(1);
	}

	return(ptr);
}

struct msf *
msf_malloc(n)
	int		n;
{
	extern char	*calloc();
	struct msf	*ptr;

	ptr = (struct msf *) calloc(n, sizeof(struct msf));
	if (ptr == NULL) {
		perror("calloc");
		exit(1);
	}

	return(ptr);
}

int
cdrom_disp_cdi() {
	int trk;

	fprintf(stderr,"CDI structure:\n");
	fprintf(stderr,"\tcurtrack: %d\n",cdi.curtrack);
	fprintf(stderr,"\tmin: %d  max: %d  total: %d\n",
		cdi.mintrack, cdi.maxtrack, cdi.ntracks);
	fprintf(stderr,"\tdur: %d  state: %2x\n",cdi.duration, cdi.state);
	fprintf(stderr,"\tcurrand: %d  lastprog: %d\n",
		cdi.currand, cdi.lastprog);
	fprintf(stderr,"\n\tTracklist:\n");
	if (cdi.maxtrack != cdi.mintrack) {
		for (trk=cdi.mintrack; trk<=cdi.maxtrack; trk++) {
			fprintf(stderr,"\t%3d: %d %02d:%02d %d\n",trk,cdi.times[trk],
				cdi.addrs[trk].minute,cdi.addrs[trk].second,
				cdi.addrs[trk].frame);
		}
	}
}

# endif /* defined(sun) */
