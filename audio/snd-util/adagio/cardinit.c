#include <stdio.h>
#include <fcntl.h>
#include "adagio.h"
#include "allphase.h"

int loadvoice(int, int, int, int, int);

void card_init(char *devname)
{
    int i, omode;

    sb_dev = gus_dev = ext_dev = -1;

    if (devname == NULL) {
	devname = "/dev/sequencer";
    }
    if (!strcmp(devname, "/dev/sequencer"))
	omode = O_NONBLOCK|O_WRONLY;
    else omode = O_RDWR;

    if ((seq_fd = open(devname, omode, 0)) == -1) {
	perror(devname);
	exit(-1);
    }
    ioctl(seq_fd, SNDCTL_SEQ_GETOUTCOUNT, &seq_max_queue);

    if (ioctl(seq_fd, SNDCTL_SEQ_NRSYNTHS, &nrsynths) == -1) {
	fprintf(stderr, "there is no soundcard\n");
	exit(1);
    }
    for (i = 0; i < nrsynths; i++) {
	card_info[i].device = i;
	if (ioctl(seq_fd, SNDCTL_SYNTH_INFO, &card_info[i]) == -1) {
	    fprintf(stderr, "cannot get info on soundcard\n");
	    perror(devname);
	    exit(-1);
	}
	card_info[i].device = i;
	if (card_info[i].synth_type == SYNTH_TYPE_SAMPLE
	    && card_info[i].synth_subtype == SAMPLE_TYPE_GUS)
	    gus_dev = i;
	else if (card_info[i].synth_type == SYNTH_TYPE_FM) {
	    sb_dev = i;
	}
    }

    if (gus_dev >= 0) {
	if (ioctl(seq_fd, SNDCTL_SEQ_RESETSAMPLES, &gus_dev) == -1) {
	    perror("Sample reset");
	    exit(1);
	}
    }

    if (ioctl(seq_fd, SNDCTL_SEQ_NRMIDIS, &nrmidis) == -1) {
	fprintf(stderr, "can't get info about midi ports\n");
	exit(1);
    }
    if (nrmidis > 0) {
#ifdef GUS_MIDI_INTERFACE
	ext_dev = 0;
#else
	ext_dev = nrmidis - 1;
#endif
	ext_index = nrsynths;
	if (ext_index > MAXCARDS-1) {
	    fprintf(stderr, "too many synths: increase MAXCARDS, recompile\n");
	    exit(1);
	}
    }

    if (exclude_gus)
	gus_dev = -1;
    if (exclude_fm)
	sb_dev = -1;
    if (!extflag)
	ext_dev = -1;

    (void)loadvoice(-1, -1, -1, 0, 0);
}
