/* set 128 SoundBlaster voices from patches in a Glib library;
   e.g.:
	setsb default.sb
				--gl
*/
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#ifdef __386BSD__
#include <machine/soundcard.h>
#else
#include <sys/soundcard.h>
#endif
#include <fcntl.h>
#include "adagio.h"
#include "sblast.h"

/*#define VERBOSE*/

int
main(int argc, char *argv[])
{	int sb, f, i, n, v, bank_size, voice_size, data_size;
	int first_voice = 0, num_progs = 128, num_drums = 0;
	int nrsynths;
	int gus_dev = -1, sb_dev = -1;
	struct sbi_instrument instr;
	struct synth_info fm_info;
	char *libname;
	char defname[80];
	char buf[O3VOICESIZE];

	if ((sb=open("/dev/sequencer", O_WRONLY, 0)) == -1) {
		fprintf(stderr,"can't open sequencer device\n");
		exit(1);
	}

	if (ioctl (sb, SNDCTL_SEQ_NRSYNTHS, &nrsynths) == -1) {
		fprintf(stderr,"there is no soundcard\n");
		exit (1);
	}

	for (i = 0; i < nrsynths; i++) {
		fm_info.device = i;
		if (ioctl (sb, SNDCTL_SYNTH_INFO, &fm_info) == -1) {
			fprintf(stderr,"cannot get info on soundcard\n");
			perror ("/dev/sequencer");
			exit (-1);
		}

		if (fm_info.synth_type == SYNTH_TYPE_SAMPLE
			&& fm_info.synth_subtype == SAMPLE_TYPE_GUS)
				gus_dev = i;
		else if (fm_info.synth_type == SYNTH_TYPE_FM) {
				sb_dev = i;
				break;
		}
	}

	if (sb_dev < 0) {
		printf("no sb device\n");
		exit(0);
	}
	instr.device = sb_dev;

	bank_size = fm_info.instr_bank_size;
	num_drums = 47;

	strcpy(defname, SBDIR);
	strcat(defname, "/std.o3");
	voice_size = O3VOICESIZE;

	if (argc < 2) libname = defname;
	else {
		libname = argv[1];
		if (!strcmp(".sb", libname + strlen(libname) - 3)) {
			voice_size = SBVOICESIZE;
		}
	}

	if ((f = open(libname, O_RDONLY, 0)) == -1) {
		if (argc < 2) {
			strcpy(defname, SBDIR);
			strcat(defname, "/std.sb");
			voice_size = SBVOICESIZE;
			f = open(libname, O_RDONLY, 0);
		}
		if (f == -1) {
			fprintf(stderr,"can't find library file %s\n", libname);
			exit(1);
		}
	}

	for (v = 0; v < num_progs; v++) {
		if (read(f, buf, voice_size) != voice_size) {
			fprintf(stderr,"short library file\n");
			exit(1);
		}
		instr.channel = v + first_voice;

		if (voice_size == SBVOICESIZE) {
			instr.key = FM_PATCH;
			data_size = 11;
		}
		else if ((buf[49]&0x3f) == 0x3f && (buf[50]&0x3f) == 0x3f ) {
			instr.key = FM_PATCH;
			data_size = 11;
		}
		else {
			instr.key = OPL3_PATCH;
			data_size = 22;
		}

		for (n = 0; n < 32; n++)
			instr.operators[n] = (n < data_size)? buf[SBOFFSET+n] : 0;

		if (write(sb, (char*)&instr, sizeof(instr)) == -1) {
			fprintf(stderr,"can't load instrument %d\n", v);
			perror("/dev/sequencer");
			exit(1);
		}
	}

	strcpy(defname, SBDIR);
	strcat(defname, "/drums.o3");
	voice_size = O3VOICESIZE;
	if (argc < 3) libname = defname;
	else {
		libname = argv[2];
		if (!strcmp(".sb", libname + strlen(libname) - 3))
			voice_size = SBVOICESIZE;
	}

	if ((f = open(libname, O_RDONLY, 0)) == -1) {
		if (argc < 3) {
			strcpy(defname, SBDIR);
			strcat(defname, "/drums.sb");
			voice_size = SBVOICESIZE;
			f = open(libname, O_RDONLY, 0);
		}
		if (f == -1) {
			fprintf(stderr,"can't find library file %s\n", libname);
			exit(1);
		}
	}
	first_voice = 128;
	num_progs = 47;

	for (v = 0; v < num_progs; v++) {
		if (read(f, buf, voice_size) != voice_size) {
			fprintf(stderr,"short library file\n");
			exit(1);
		}
		instr.channel = v + first_voice;

		if (voice_size == SBVOICESIZE) {
			instr.key = FM_PATCH;
			data_size = 11;
		}
		else if ((buf[49]&0x3f) == 0x3f && (buf[50]&0x3f) == 0x3f ) {
			instr.key = FM_PATCH;
			data_size = 11;
		}
		else {
			instr.key = OPL3_PATCH;
			data_size = 22;
		}

		for (n = 0; n < 32; n++)
			instr.operators[n] = (n < data_size)? buf[SBOFFSET+n] : 0;

		if (write(sb, (char*)&instr, sizeof(instr)) == -1) {
			fprintf(stderr,"can't load instrument %d\n", v);
			exit(1);
		}
	}
	exit(0);
}

