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

# include <stdio.h>

char cdrom[] =	"/dev/rsr0";

extern char *	cdrom_status();

main() {
	int			fd;

	if ((fd = open(cdrom, 0)) == -1) {
		fprintf(stderr, "open: ");
		perror(cdrom);
		exit(1);
	}

	printf("status: %s\n", cdrom_status(fd));

	cdrom_toc(fd, CDROM_MSF);

	cdrom_subchnl(fd, CDROM_MSF);
}

cdrom_toc(fd, fmt) {
	struct cdrom_tochdr	tochdr;
	struct cdrom_tocentry	tocentry;
	int			trk;
	int			otime, trk_total;

	if (cdrom_read_tochdr(fd, &tochdr) == -1)
		return;

	printf("TRACK\tTIME\tFRAME\tTYPE\n");

	otime = 0;

	for (trk = tochdr.cdth_trk0; trk <= tochdr.cdth_trk1; trk++) {
		if (cdrom_read_tocentry(fd, trk, fmt, &tocentry) == -1)
			return;

		trk_total = ((int) tocentry.cdte_addr.msf.minute * 60) +
			(int) tocentry.cdte_addr.msf.second;

		trk_total -= otime;

		if (otime != 0) {
			printf("%d\t%02d:%02d", trk-1,
			       trk_total / 60,
			       trk_total % 60);

			printf("\t%d", (int) tocentry.cdte_addr.msf.frame);

			if (tocentry.cdte_ctrl & CDROM_DATA_TRACK)
				printf("\tdata");
			else
				printf("\taudio");

			printf("\n");
		}

		otime += trk_total;
	}

	if (cdrom_read_tocentry(fd, CDROM_LEADOUT, fmt, &tocentry) == -1)
		return;

	trk_total = ((int) tocentry.cdte_addr.msf.minute * 60) +
		(int) tocentry.cdte_addr.msf.second;

	trk_total -= otime;

	printf("%d\t%02d:%02d", trk-1,
	       trk_total / 60,
	       trk_total % 60);

	printf("\t%d", (int) tocentry.cdte_addr.msf.frame);

	if (tocentry.cdte_ctrl & CDROM_DATA_TRACK)
		printf("\tdata");
	else
		printf("\taudio");

	printf("\n");
}

cdrom_read_tocentry(fd, trk, fmt, tocentry)
	int			fd;
	int			trk, fmt;
	struct cdrom_tocentry	*tocentry;
{
	tocentry->cdte_track = trk;
	tocentry->cdte_format = fmt;

	if (ioctl(fd, CDROMREADTOCENTRY, (char *) tocentry) == -1) {
		fprintf(stderr, "ioctl(cdromreadtocentry): ");
		perror(cdrom);

		return(-1);
	}

	return(0);
}

cdrom_read_tochdr(fd, tochdr)
	int			fd;
	struct cdrom_tochdr	*tochdr;
{
	if (ioctl(fd, CDROMREADTOCHDR, (char *) tochdr) == -1) {
		fprintf(stderr, "ioctl(cdromreadtochdr): ");
		perror(cdrom);

		return(-1);
	}

	return(0);
}

char *
cdrom_status(fd) {
	static char		buf[512];
	struct cdrom_subchnl	subchnl;

	if (ioctl(fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);
		exit(1);
	}

	switch (subchnl.cdsc_audiostatus) {
		case CDROM_AUDIO_INVALID:
		return("invalid");
		break;

		case CDROM_AUDIO_PLAY:
		sprintf(buf, "playing track %d", (int) subchnl.cdsc_trk);
		return(buf);
		break;

		case CDROM_AUDIO_PAUSED:
		sprintf(buf, "paused on track %d", (int) subchnl.cdsc_trk);
		return(buf);
		break;

		case CDROM_AUDIO_COMPLETED:
		return("completed");
		break;

		case CDROM_AUDIO_ERROR:
		return("error");
		break;

		case CDROM_AUDIO_NO_STATUS:
		return("no status");
		break;
	}

	return("bad value in cdsc_audiostatus");
}

cdrom_subchnl(fd, fmt) {
	struct cdrom_subchnl	subchnl;

	if (ioctl(fd, CDROMSUBCHNL, (char *) &subchnl) == -1) {
		fprintf(stderr, "ioctl(cdromsubchnl): ");
		perror(cdrom);
		exit(1);
	}

	printf("subchnl:\n");

	printf("format=0x%x\n", (unsigned int) subchnl.cdsc_format);
	printf("adr=%u\n", (unsigned int) subchnl.cdsc_adr);
	printf("ctrl=%u\n", (unsigned int) subchnl.cdsc_ctrl);
	printf("trk=%u\n", (unsigned int) subchnl.cdsc_trk);
	printf("ind=%u\n", (unsigned int) subchnl.cdsc_ind);

	printf("absaddr=%02u:%02u %u\n",
	       (unsigned int) subchnl.cdsc_absaddr.msf.minute,
	       (unsigned int) subchnl.cdsc_absaddr.msf.second,
	       (unsigned int) subchnl.cdsc_absaddr.msf.frame);

	printf("reladdr=%02u:%02u %u\n",
	       (unsigned int) subchnl.cdsc_reladdr.msf.minute,
	       (unsigned int) subchnl.cdsc_reladdr.msf.second,
	       (unsigned int) subchnl.cdsc_reladdr.msf.frame);
}
