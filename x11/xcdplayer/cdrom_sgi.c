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

# if defined(sgi)
# include <stdio.h>

# include <sys/file.h>
# include <sys/types.h>
# include <sys/param.h>
# include <sys/stat.h>

# include <mntent.h>
# include <string.h>
# include <signal.h>

# include <sys/buf.h>

# include <sys/time.h>

/* We need shared memory for the counter display if using scsi_audio */
#include <sys/ipc.h>
#include <sys/shm.h>
int		shmid;
key_t		shmkey;


# include "debug.h"
# include "cdrom_sgi.h"

extern char	*device;
# if defined(notdef)
extern void	cdrom_print_toc();
# endif /* defined(notdef) */

void		cdrom_audio_close();
void		cdrom_callback();
void		cdrom_kill_child();
int		cdrom_child_setup();
void		cdrom_child_pause();
void		cdrom_child_died();
void		cdrom_child_quit();

#define INIT_READ	200	/* Maximum number we can play */
#define ONE_SECOND	75	/* One second of data */

cdrom_info		cdi;
struct _cdrom_shmem	*cdrom_shmem;
char			info_filename[256];
FILE			*disc_info = NULL;

CDPLAYER	*cdrom_fd = NULL;
ALport		audio_fd = NULL;
long		audio_param[6];
CDFRAME		*cd_audio_buff = NULL;
CDPARSER	*cd_parser = NULL;
int		cdrom_audio_opened = 0;

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
	int		n;
	CDSTATUS	cdrom_stat;
	extern void update_title();

	if (cdrom_fd != NULL) {
		return(cdi.curtrack);
	}

	if (device != NULL) {
		fprintf(stderr,"Device: %s\n",device);
		if ((cdrom_fd = CDopen(device, "r")) == NULL) {
			fprintf(stderr, "open cdrom: %s",device);
			perror("CDopen");
			return(-1);
		}
	} else {
		if ((cdrom_fd = CDopen(0, "r")) == NULL) {
			fprintf(stderr, "open cdrom: ");
			perror("CDopen");
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

	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_stat): ");
		perror("CDgetstatus");
		return(-1);
	}
	if ((cdrom_stat.state == CD_PLAYING) ||
	    (cdrom_stat.state == CD_PAUSED)) {
		cdi.scsi_audio = 0;
	} else {
		cdi.scsi_audio = cdrom_audio_avail();
	}

	if (cdi.scsi_audio) {
		if (cd_audio_buff == (CDFRAME *) NULL) {
			if ((cd_audio_buff =
			     (CDFRAME *) malloc(INIT_READ * CDDA_BLOCKSIZE)) == NULL) {
				fprintf(stderr,"cdrom_open: cannot allocate cd digital data buffer!\n");
				perror("cdrom_open");
				cdi.scsi_audio = 0;
			}
		}
		if (cd_parser == NULL) {
			if ((cd_parser = CDcreateparser()) == NULL) {
				fprintf(stderr,"cdrom_open: CDcreateparser failed!\n");
				perror("cdrom_open");
				cdi.scsi_audio = 0;
			}
			CDresetparser(cd_parser);
			CDsetcallback(cd_parser, cd_audio, cdrom_callback, 0);
			CDsetcallback(cd_parser, cd_pnum, cdrom_callback, 0);
			CDsetcallback(cd_parser, cd_atime, cdrom_callback, 0);
			CDsetcallback(cd_parser, cd_catalog, cdrom_callback, 0);
		}
#if 0
		/* Get the catalog number, if one exists */
		CDseektrack(cdrom_fd,1);
		cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
		cdrom_play_frames(1);
#endif
	}

	if (cdi.state & CDROM_STATE_PLAY)
		cdi.curtrack = n;

	if (cdi.state & CDROM_STATE_SHUFFLE)
		shuffle_setup();

	return(cdi.curtrack);
}

void
cdrom_close() {
	if (cdrom_fd == NULL)
		return;

	if (cdi.scsi_audio) {
		cdrom_kill_child();
		cdrom_audio_close();
	}
	if (cdi.times != NULL) {
		free((char *) cdi.times);
		free((char *) cdi.addrs);
		cdi.times = NULL;
		cdi.addrs = NULL;
	}

	(void) CDclose(cdrom_fd);
	cdrom_fd = NULL;
}

int
cdrom_audio_open() {
	ALconfig	aconfig;
	
	aconfig = ALnewconfig();

	/* Attempt to create the largest buffer we can */
	ALsetqueuesize (aconfig, SAMPLES_PER_FRAME * INIT_READ);

	/* Make sure we are 16bit stereo */
	ALsetwidth(aconfig, AL_SAMPLE_16);
	ALsetchannels(aconfig, AL_STEREO);

	if ((audio_fd = ALopenport("XCplayer", "w", aconfig)) == NULL) {
		ALfreeconfig(aconfig);
		return(-1);	/* No error, just return -1 */
	}
	
	ALfreeconfig(aconfig);

	audio_param[0] = AL_OUTPUT_RATE;
	audio_param[1] = AL_RATE_44100;
	ALsetparams(AL_DEFAULT_DEVICE, audio_param, 2);

	shmkey = IPC_PRIVATE;
	if ((shmid = shmget(shmkey, sizeof(struct _cdrom_shmem), 
			    (SHM_R | SHM_W | IPC_CREAT | IPC_EXCL) )) < 0) {
		perror("shmget");
		return(-1);
	}
	cdrom_shmem = shmat(shmid, NULL, SHM_RND);
}

void
cdrom_audio_close() {
	if (audio_fd == NULL)
		return;

	(void) ALcloseport(audio_fd);
	shmdt(cdrom_shmem);
	shmctl(shmid,IPC_RMID);
	audio_fd = NULL;
	cdrom_audio_opened = 0;
}

void
cdrom_callback(arg, type, data)
	int		arg;
	CDDATATYPES	type;
	void		*data;
{
	struct cdtimecode	*timecode_info;
	CDPROGNUM		*pnum_info;
	char *			catalog_num;
	int			otime, time;
	int			i;

	switch (type) {
	case cd_audio:
		ALwritesamps(audio_fd, data, SAMPLES_PER_FRAME);
		break;
	case cd_pnum:
		pnum_info = data;

		if (pnum_info->value <= 
		    cdrom_shmem->cdrom_audio_cdi.end_track) {
			cdrom_shmem->cdrom_audio_cdi.curtrack = pnum_info->value;
		} else {
			cdrom_shmem->cdrom_audio_cdi.curtrack = 0;
			cdrom_shmem->cdrom_audio_cdi.state = CD_READY;
		}
		break;
	case cd_atime:
		timecode_info = data;

		otime = (cdrom_shmem->cdrom_audio_msf.minute * 60) +
			cdrom_shmem->cdrom_audio_msf.second;
		time = (((timecode_info->mhi * 10) + 
			 timecode_info->mlo) * 60) +
				 (timecode_info->shi * 10) +
					 timecode_info->slo;

		if ((otime > (time + 2)) || (otime < time - 2)) {
			if (CDseek(cdrom_fd, 
				   cdrom_shmem->cdrom_audio_msf.minute,
				   cdrom_shmem->cdrom_audio_msf.second, 
				   cdrom_shmem->cdrom_audio_msf.frame) < 0) {
				perror("CDseek");
			}
			cdrom_shmem->cdrom_audio_cdi.state = CD_ABORT;
			return;
		}

		cdrom_shmem->cdrom_audio_msf.minute = 
			(timecode_info->mhi * 10) + timecode_info->mlo;
		cdrom_shmem->cdrom_audio_msf.second = 
			(timecode_info->shi * 10) + timecode_info->slo;
		cdrom_shmem->cdrom_audio_msf.frame = 
			(timecode_info->fhi * 10) + timecode_info->flo;
		
		break;
	case cd_catalog:
		catalog_num = data;
#if 0
		fprintf(stderr,"Catalog #: ");
		for (i=0; i<13; i++) {
			fprintf(stderr,"%d",*(catalog_num+i));
		}
		fprintf(stderr,"\n");
#endif
	default:
		return;
	}
}

int
cdrom_start() {
	return(0);
}

int
cdrom_stop() {
	if (cdi.scsi_audio && cdrom_audio_opened) {
		cdrom_kill_child();
		cdrom_shmem->cdrom_audio_cdi.curtrack = cdi.curtrack;
		cdrom_shmem->cdrom_audio_cdi.state = CD_READY;
	}
	if (! CDstop(cdrom_fd)) {
		perror("CDstop");
		return(-1);
	}

	return(0);
}

int
cdrom_eject() {
	if (cdi.scsi_audio && cdrom_audio_opened) {
		cdrom_kill_child();
		cdrom_shmem->cdrom_audio_cdi.state = CD_NODISC;
	}
	if (! CDeject(cdrom_fd)) {
		perror("ioctl(cdromeject)");
		return(-1);
	}
	cdrom_close();

	return(0);
}

int
cdrom_pause() {
	int track;

	if (cdi.scsi_audio && cdrom_audio_opened) {
		cdrom_shmem->cdrom_audio_cdi.state = CD_PAUSED;
		return(0);
	}
	if (! cdrom_paused(&track)) {
		if (! CDtogglepause(cdrom_fd)) {
			perror("CDtogglepause");
			return(-1);
		}
	}
	
	return(0);
}

int
cdrom_resume() {
	int track;

	if (cdi.scsi_audio && cdrom_audio_opened) {
		cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
		return(0);
	}
	if (! cdrom_playing(&track)) {
		if (! CDtogglepause(cdrom_fd)) {
			perror("CDtogglepause");
			return(-1);
		}
	}

	return(0);
}

int
cdrom_toggle_audio()
{
	if (cdi.scsi_audio) {
		cdrom_kill_child();
		cdi.scsi_audio = 0;
		return;
	}
	cdi.scsi_audio = 1;
}

int
cdrom_volume(left_vol, right_vol)
	int			left_vol;
	int			right_vol;
{
	CDVOLUME	cd_vol;

	if (cdi.scsi_audio) {
		audio_param[0] = AL_LEFT_SPEAKER_GAIN;
		audio_param[1] = left_vol;
		audio_param[2] = AL_RIGHT_SPEAKER_GAIN;
		audio_param[3] = right_vol;

		ALsetparams (AL_DEFAULT_DEVICE, audio_param, 4);
	} else {
#if 0
		/* I would love to use this, but it isn't documented */
		cd_vol.chan0 = (unsigned char)left_vol;
		cd_vol.chan1 = (unsigned char)right_vol;
		CDsetvolume(cdrom_fd, &cd_vol);
#endif
		return(-1);
	}

	return(0);
}

int
cdrom_get_volume()
{
	int vol;
	CDVOLUME	cd_vol;

	audio_param[0] = AL_LEFT_SPEAKER_GAIN;
	audio_param[2] = AL_RIGHT_SPEAKER_GAIN;
	if (cdi.scsi_audio) {
		ALgetparams (AL_DEFAULT_DEVICE, audio_param, 4);
	
		/* Return the average of left and right channels */
		vol=(audio_param[1] + audio_param[3]) / 2;
	} else {
#if 0
		/* I would love to use this, but it isn't documented */
		CDgetvolume(cdrom_fd, &cd_vol);
		fprintf(stderr,"%d %d %d %d\n",cd_vol.chan0,cd_vol.chan1,
			cd_vol.chan2,cd_vol.chan3);
		vol=(cd_vol.chan0 + cd_vol.chan1) / 2;
#endif
		vol = -1;
	}

	return(vol);
}

int
cdrom_get_times() {
	CDSTATUS	        tochdr;
	extern unsigned short	*ushort_malloc();
	extern struct msf	*msf_malloc();
	unsigned long		trk, trk_total, otime;
	struct msf		msf;

	if (cdrom_read_tochdr(&tochdr) == -1) {
		cdi.mintrack = 0;
		cdi.maxtrack = 0;
		return(-1);
#if 0
		return(0);
#endif
	}

	cdi.mintrack = tochdr.first;
	cdi.maxtrack = tochdr.last;

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
	CDSTATUS cdrom_stat;
  
	if (cdi.scsi_audio && cdrom_audio_opened &&
	    ((cdrom_shmem->cdrom_audio_cdi.state == CD_PLAYING) ||
	     (cdrom_shmem->cdrom_audio_cdi.state == CD_PAUSED))) {
		if (((cdrom_shmem->cdrom_audio_msf.minute * 60) +
		     cdrom_shmem->cdrom_audio_msf.second) > 
		    ((cdi.addrs[cdrom_shmem->cdrom_audio_cdi.curtrack].minute * 60) +
		     cdi.addrs[cdrom_shmem->cdrom_audio_cdi.curtrack].second)) {
			cdrom_shmem->cdrom_audio_cdi.curtrack++;
		}
		if (((cdrom_shmem->cdrom_audio_msf.minute * 60) +
		     cdrom_shmem->cdrom_audio_msf.second) < 
		    ((cdi.addrs[cdrom_shmem->cdrom_audio_cdi.curtrack-1].minute * 60) +
		     cdi.addrs[cdrom_shmem->cdrom_audio_cdi.curtrack-1].second)) {
			cdrom_shmem->cdrom_audio_cdi.curtrack--;
		}
		return(cdrom_shmem->cdrom_audio_cdi.curtrack);
	}
	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_stat): ");
		perror("CDgetstatus");
		return(-1);
	}

	switch (cdrom_stat.state) {
	case CD_ERROR:
		return(-1);
		
	case CD_NODISC:
		debug_printf(1, "cdrom_get_curtrack: no disc\n");
		return(0);
		
		/* playing track cdrom_stat.track */
	case CD_PLAYING:
		return((int) cdrom_stat.track);
		
		/* paused on track cdrom_stat.track */
	case CD_PAUSED:
		return((int) cdrom_stat.track);
  
		/* punt */
	case CD_READY:
		return(0);

		/* punt */
	case CD_STILL:
		return(0);

#if 0
	case CD_NOSTATUS:
		debug_printf(1, "cdrom_get_curtrack: no status\n");
		return(0);
#endif
	}

	/* bad value in cdrom_stat */
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
	int 		num_frames;
	CDSTATUS 	cdrom_stat;

	if (cdi.scsi_audio && cdrom_audio_opened) {
		msf->minute = cdrom_shmem->cdrom_audio_msf.minute;
		msf->second = cdrom_shmem->cdrom_audio_msf.second;
		msf->frame = cdrom_shmem->cdrom_audio_msf.frame;
		if (msf->minute < 0) {
			msf->minute = 0;
			msf->second = 0;
			msf->frame = 0;
		}
		return(0);
	}
	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_stat): ");
		perror("CDgetstatus");
		return(-1);
	}
	msf->minute = cdrom_stat.abs_min;
	msf->second = cdrom_stat.abs_sec;
	msf->frame = cdrom_stat.abs_frame;

	return (0);
}

int
cdrom_play_track(start_track, end_track)
	unsigned char		start_track;
	unsigned char		end_track;
{
	int 		track;
	struct msf 	msf;

	track = start_track;
	
	if ( cdi.scsi_audio && cdrom_audio_opened ) {
		cdrom_shmem->cdrom_audio_cdi.start_track = start_track;
		cdrom_shmem->cdrom_audio_cdi.end_track = end_track;

		cdrom_shmem->cdrom_audio_cdi.state = CD_ABORT;
		if (CDseektrack(cdrom_fd, track) < 0) {
			perror("CDseektrack");
			return(-1);
		}
		msf = cdi.addrs[track - cdi.mintrack];
		cdrom_shmem->cdrom_audio_msf.minute = msf.minute;
		cdrom_shmem->cdrom_audio_msf.second = msf.second;
		cdrom_shmem->cdrom_audio_msf.frame = msf.frame;
		cdi.curtrack = track;
		cdrom_shmem->cdrom_audio_cdi.curtrack = track;
		cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
		if (cdrom_shmem->cdrom_audio_cdi.child_pid != 0) {
			return(0);
		}
		signal(SIGCLD, cdrom_child_died);
		if ((cdrom_shmem->cdrom_audio_cdi.child_pid = fork()) == 0) {
			cdrom_child_setup();
			while (1) {
				cdrom_play_frames(ONE_SECOND);
			}
		}
	} else {
		if (! CDplaytrack(cdrom_fd, track, 1)) {
			perror("CDplaytrack");
			return(-1);
		}
	}	  

	return(0);
}

int
cdrom_play_msf(start_msf, end_msf)
	struct msf	*start_msf;
	struct msf	*end_msf;
{
	int num_frames;
	int i;

	/* Fix negative seconds */
	if (start_msf->second < 0) {
		start_msf->second += 60;
		start_msf->minute--;
	}
	
	if ( cdi.scsi_audio && cdrom_audio_opened ) {
		cdrom_shmem->cdrom_audio_msf.minute = start_msf->minute;
		cdrom_shmem->cdrom_audio_msf.second = start_msf->second;
		cdrom_shmem->cdrom_audio_msf.frame = start_msf->frame;
		if (CDseek(cdrom_fd, start_msf->minute,
				start_msf->second, 
				start_msf->frame) < 0) {
			perror("CDseek");
			return(-1);
		}
		cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
		cdrom_shmem->cdrom_audio_cdi.curtrack = cdi.curtrack;
		if (cdrom_shmem->cdrom_audio_cdi.child_pid != 0) {
			return(0);
		}
		signal(SIGCLD, cdrom_child_died);
		if ((cdrom_shmem->cdrom_audio_cdi.child_pid = fork()) == 0) {
			cdrom_child_setup();
			while (1) {
				cdrom_play_frames(ONE_SECOND);
			}
		}

	} else {
		if (! CDplayabs(cdrom_fd, start_msf->minute, 
				start_msf->second, 
				start_msf->frame, 1)) {
			perror("CDplayabs");
			return(-1);
		}

		return(0);
	}
}

int
cdrom_read_tocentry(track, tocentry)
	unsigned int		track;
	struct cdrom_tocentry 	*tocentry;
{
	CDSTATUS cdrom_stat;
	CDTRACKINFO cdrom_track;

	tocentry->cdte_track = track;
	tocentry->cdte_format = CDROM_MSF;

	if (cdrom_status() == CD_NODISC) {
		tocentry->cdte_addr.msf.minute = 0;
		tocentry->cdte_addr.msf.second = 0;
		tocentry->cdte_addr.msf.frame = 0;
		return(0);
	}

	if (track == CDROM_LEADOUT) {
		if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
			fprintf(stderr, "CDgetstatus(cdrom_read_tocentry): ");
			perror("CDgetstatus");
			return(-1);
		}
		tocentry->cdte_addr.msf.minute = cdrom_stat.total_min;
		tocentry->cdte_addr.msf.second = cdrom_stat.total_sec;
		tocentry->cdte_addr.msf.frame = cdrom_stat.total_frame;
	} else {
		if (! CDgettrackinfo(cdrom_fd, track, 
				     (CDTRACKINFO *) &cdrom_track)) {
			fprintf(stderr, "CDgettrackinfo(cdrom_read_tocentry): ");
			perror("CDgetstatus");
			return(-1);
		}
		tocentry->cdte_addr.msf.minute = cdrom_track.start_min;
		tocentry->cdte_addr.msf.second = cdrom_track.start_sec;
		tocentry->cdte_addr.msf.frame = cdrom_track.start_frame;
	}
	return(0);
}

int
cdrom_read_tochdr(tochdr)
	CDSTATUS *tochdr;
{
	if (cdrom_status() == CD_NODISC) 
		return(-1);

	if (! CDgetstatus(cdrom_fd, tochdr)) {
		fprintf(stderr, "CDgetstatus(cdrom_read_tochdr): ");
		perror("CDgetstatus");
		return(-1);
	}

	return(0);
}

int
cdrom_status() {
	CDSTATUS cdrom_stat;

	if (cdrom_fd == NULL) {
		return(-1);
	}

	if (cdi.scsi_audio && cdrom_audio_opened &&
	    (cdrom_shmem->cdrom_audio_cdi.child_pid != 0)) { 
		return(cdrom_shmem->cdrom_audio_cdi.state);
	}
	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_stat): ");
		perror("CDgetstatus");
		return(-1);
	}

	switch (cdrom_stat.state) {
	case CD_ERROR:
		return(CD_ERROR);
	case CD_NODISC:
		return(CD_NODISC);
	case CD_READY:
		return(CD_READY);
	case CD_PLAYING:
		return(CD_PLAYING);
	case CD_PAUSED:
		return(CD_PAUSED);
	case CD_STILL:
		return(CD_STILL);
	}

	return(-1);
}

int
cdrom_playing(track)
	int			*track;
{
	CDSTATUS cdrom_stat;

	if (cdi.scsi_audio && cdrom_audio_opened &&
	    ((cdrom_shmem->cdrom_audio_cdi.state == CD_PLAYING) ||
	     (cdrom_shmem->cdrom_audio_cdi.state == CD_PAUSED))) {
		return(cdrom_shmem->cdrom_audio_cdi.state);
	}

	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_playing): ");
		perror("CDstatus");
		return(-1);
	}

	*track = cdrom_stat.track;
	if (cdrom_stat.state == CD_PLAYING) {
		return(1);
	}

	return(0);
}

int
cdrom_paused(track)
	int			*track;
{
	CDSTATUS cdrom_stat;

	if (cdi.scsi_audio && cdrom_audio_opened &&
	    ((cdrom_shmem->cdrom_audio_cdi.state == CD_PLAYING) ||
	     (cdrom_shmem->cdrom_audio_cdi.state == CD_PAUSED))) {
		return(cdrom_shmem->cdrom_audio_cdi.state);
	}

	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_paused): ");
		perror("CDgetstatus");
		return(-1);
	}

	*track = cdrom_stat.track;
	if (cdrom_status() == CD_PAUSED) {
		return(1);
	}

	return(0);
}

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

int
cdrom_play_frames(number)
	int	number;
{
	int	frames_to_read = number;
	int	num_frames;
	int	frame_ctr;
	struct msf msf;
	CDSTATUS cdrom_stat;

	if (cdrom_audio_opened && 
	    cdrom_shmem->cdrom_audio_cdi.state != CD_PLAYING) {
		sleep(1);
		return(-1);
	}

	if (frames_to_read > INIT_READ) {
		frames_to_read = INIT_READ;
	}
	if ((num_frames = 
	     CDreadda(cdrom_fd, &cd_audio_buff[0], frames_to_read)) == -1) {
		perror("CDreadda");
		return(-1);
	}
	if (num_frames == 0) {
		msf = cdi.addrs[cdi.maxtrack];
		if ((cdrom_shmem->cdrom_audio_msf.minute >= (msf.minute-1)) &&
		    (cdrom_shmem->cdrom_audio_msf.second >= (msf.second-1)) ) {
			cdrom_shmem->cdrom_audio_cdi.curtrack = 0;
			cdrom_shmem->cdrom_audio_cdi.state = CD_READY;
		}
		return(0);
	}
	for (frame_ctr=0;
	     frame_ctr<num_frames && 
	     cdrom_shmem->cdrom_audio_cdi.state == CD_PLAYING;
	     frame_ctr++) {
		CDparseframe(cd_parser, &cd_audio_buff[frame_ctr]);
	}
	if (cdrom_shmem->cdrom_audio_cdi.state == CD_ABORT) {
		cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
	}
	return(num_frames);
}

int
cdrom_child_setup() {
	cdrom_shmem = shmat(shmid, NULL, SHM_RND);
	cdrom_shmem->cdrom_audio_cdi.state = CD_PLAYING;
	signal(SIGHUP,cdrom_child_quit);
}

void
cdrom_child_died() {
	cdrom_stop();
	wait(NULL);
	cdrom_shmem->cdrom_audio_cdi.child_pid = 0;
	cdrom_shmem->cdrom_audio_cdi.state = CD_READY;
	cdrom_shmem->cdrom_audio_cdi.curtrack = 0;
	cdi.curtrack = 0;
	cdrom_shmem->cdrom_audio_msf.minute = 0;
	cdrom_shmem->cdrom_audio_msf.second = 0;
	cdrom_shmem->cdrom_audio_msf.frame = 0;
	signal(SIGCLD,cdrom_child_died);
}

void
cdrom_child_quit() {
	shmdt(cdrom_shmem);
	exit(1);
}

void
cdrom_kill_child()
{
	if (!(cdrom_audio_opened && cdrom_shmem->cdrom_audio_cdi.child_pid)) {
		return;
	}
	kill(cdrom_shmem->cdrom_audio_cdi.child_pid, SIGHUP);
	wait(NULL);
	cdrom_shmem->cdrom_audio_cdi.state = CD_READY;
	cdrom_shmem->cdrom_audio_cdi.child_pid = 0;
}

int
cdrom_audio_avail() {
	CDSTATUS	cdrom_stat;

	if (cdrom_audio_opened && (! audio_button_state())) {
		return(0);
	}
	cdrom_audio_opened = 1;

	if (! CDgetstatus(cdrom_fd, (CDSTATUS *) &cdrom_stat)) {
		fprintf(stderr, "CDgetstatus(cdrom_stat): ");
		perror("CDgetstatus");
		return(-1);
	}

	if ( cdrom_stat.scsi_audio && (cdrom_audio_open() >= 0) ) {
		return(1);
	} else {
		return(0);
	}
}

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

	if (cdi.scsi_audio) {
		fprintf(stderr,"AUDIO_CDI structure:\n");
		fprintf(stderr,"\tcurtrack: %d\n",cdrom_shmem->cdrom_audio_cdi.curtrack);
		fprintf(stderr,"\tdur: %d  state: %d\n",cdrom_shmem->cdrom_audio_cdi.duration, cdrom_shmem->cdrom_audio_cdi.state);
	}
}

# endif /* defined(sgi) */
