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

# include <sys/types.h>
# include <sys/buf.h>

# include <sun/dkio.h>

# include <scsi/targets/srdef.h>
# include <scsi/impl/uscsi.h>

/*# include <sundev/srreg.h>*/

# include <stdio.h>

char cdrom[] =	"/dev/rsr0";

extern void		cdrom_open();
extern char		*cdrom_status_string();
extern unsigned int	cdrom_get_track();
extern unsigned long	*cdrom_get_times();
extern unsigned long	*ulong_malloc();

static int		cdrom_fd;

unsigned long		cdrom_mintrack;
unsigned long		cdrom_maxtrack;
unsigned long		*cdrom_times;

main() {
	cdrom_open();

	printf("status: %s\n", cdrom_status_string(cdrom_get_status()));

	cdrom_times = cdrom_get_times();
	cdrom_print_toc();
}

void
cdrom_open() {
	if ((cdrom_fd = open(cdrom, 0)) == -1) {
		fprintf(stderr, "open: ");
		perror(cdrom);

		exit(1);
	}
}

unsigned long *
cdrom_get_times() {
	struct cdrom_tochdr	tochdr;
	struct cdrom_tocentry	tocentry;
	extern unsigned long	*ulong_malloc();
	unsigned long		trk, trk_total, otime;
	unsigned long		*cdrom_times;

	if (cdrom_read_tochdr(&tochdr) == -1)
		return(NULL);

	cdrom_mintrack = tochdr.cdth_trk0;
	cdrom_maxtrack = tochdr.cdth_trk1;

	cdrom_times = ulong_malloc(cdrom_maxtrack - cdrom_mintrack + 1);

	otime = 0;

	for (trk = cdrom_mintrack; trk <= cdrom_maxtrack; trk++) {
		if (cdrom_read_tocentry(trk, &tocentry) == -1)
			return(NULL);

		trk_total = ((int) tocentry.cdte_addr.msf.minute * 60) +
			(int) tocentry.cdte_addr.msf.second;

		trk_total -= otime;
		otime += trk_total;

		if (trk != cdrom_mintrack)
			cdrom_times[trk - cdrom_mintrack - 1] = trk_total;
	}

	if (cdrom_read_tocentry(CDROM_LEADOUT, &tocentry) == -1)
		return(NULL);

	trk_total = ((int) tocentry.cdte_addr.msf.minute * 60) +
		(int) tocentry.cdte_addr.msf.second;

	trk_total -= otime;

	cdrom_times[trk - cdrom_mintrack - 1] = trk_total;

	return(cdrom_times);
}

unsigned long *
ulong_malloc(n) {
	extern char	*malloc();
	unsigned long	*ptr;

	ptr = (unsigned long *) calloc(n, sizeof(unsigned long));
	if (ptr == NULL) {
		perror("malloc");
		exit(1);
	}

	return(ptr);
}

cdrom_print_toc() {
	unsigned long		trk, trk_total;

	for (trk = cdrom_mintrack; trk <= cdrom_maxtrack; trk++) {
		trk_total = cdrom_times[trk - cdrom_mintrack];
		printf("%02u:%02u\n", trk_total/60, trk_total%60);
	}
}

cdrom_read_tocentry(trk, tocentry)
	int			trk;
	struct cdrom_tocentry	*tocentry;
{
	tocentry->cdte_track = trk;
	tocentry->cdte_format = CDROM_MSF;

	if (ioctl(cdrom_fd, CDROMREADTOCENTRY, (char *) tocentry) == -1) {
		fprintf(stderr, "ioctl(cdromreadtocentry): ");
		perror(cdrom);

		return(-1);
	}

	return(0);
}

int
cdrom_read_tochdr(tochdr)
	struct cdrom_tochdr	*tochdr;
{
	if (ioctl(cdrom_fd, CDROMREADTOCHDR, (char *) tochdr) == -1) {
		fprintf(stderr, "ioctl(cdromreadtochdr): ");
		perror(cdrom);

		return(-1);
	}

	return(0);
}

int
cdrom_get_status() {
	struct cdrom_subchnl	subchnl;

	if (ioctl(cdrom_fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);

		return(-1);
	}

	return(subchnl.cdsc_audiostatus);
}

unsigned int
cdrom_get_track() {
	struct cdrom_subchnl	subchnl;

	if (ioctl(cdrom_fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);

		return(-1);
	}

	return(subchnl.cdsc_trk);
}

char *
cdrom_status_string(status) {
	static char		buf[512];
	typedef struct _status_table {
		unsigned int	st_val;
		char		*st_str;
	} status_table;
	static status_table	st_table[] = {
		{ CDROM_AUDIO_INVALID,	"audio status not supported" },
		{ CDROM_AUDIO_PLAY,	"audio play operation in progress" },
		{ CDROM_AUDIO_PAUSED,	"audio play operation paused" },
		{ CDROM_AUDIO_COMPLETED,"audio play successfully completed" },
		{ CDROM_AUDIO_ERROR,	"audio play stopped due to error" },
		{ CDROM_AUDIO_NO_STATUS,"no current audio status to return" },
		{ -1,			NULL }
	};
	status_table *st;

	switch (status) {
		case CDROM_AUDIO_PLAY:
		sprintf(buf, "playing track %d",
			(unsigned int) cdrom_get_track());
		return(buf);

		case CDROM_AUDIO_PAUSED:
		sprintf(buf, "paused on track %d",
			(unsigned int) cdrom_get_track());
		return(buf);
	}

	for (st = &st_table[0]; st->st_str != NULL; st++) {
		if (status == st->st_val)
			return(st->st_str);
	}

	return("invalid status value");
}
