/* Heavily modified by Rob Hooft (hooft@chem.ruu.nl) */

#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#ifndef __386BSD__
#include <getopt.h>
#endif

struct sbi_instrument instr;
		
#define USAGE { fprintf(stderr,"usage: %s [-v] sbi_file...\n",argv[0]);\
		exit(2); }

int main(int argc, char *argv[])
{
	char buf[100];
	int opt,offset;
	extern int optind;
	int verbose=0;
	int dev, nrdevs;
	struct synth_info info;

	int l,i, fm, op, instrf;

	if ((fm=open("/dev/sequencer", O_WRONLY, 0))==-1) 
	{
		perror("/dev/sequencer");
		exit(-1);
	}

	if (ioctl(fm, SNDCTL_SEQ_NRSYNTHS, &nrdevs) == -1)
	{
		perror("/dev/sequencer");
		exit(-1);
	}

	dev = -1;

	for (i=0;i<nrdevs && dev==-1;i++)
	{
		info.device = i;
		if (ioctl(fm, SNDCTL_SYNTH_INFO, &info)==-1)
		{
			perror("info: /dev/sequencer");
			exit(-1);
		}

		if (info.synth_type == SYNTH_TYPE_FM) dev = i;
	}

	if (dev == -1)
	{
		fprintf(stderr, "%s: FM synthesizer not detected\n", argv[0]);
		exit(-1);
	}

	while ((opt = getopt(argc,argv,"v")) != -1) {
		switch (opt) {
		case 'v': verbose = 1; break;
		default : USAGE;
		}
	}
	if (argc == optind) USAGE;
	offset=optind;
	if (argc-offset>128) {
		argc=128+offset;
		fprintf(stderr,"Warning: Excess arguments ignored\n");
	}
	for (op=offset;op < argc;op++) {
		if ((instrf=open(argv[op], O_RDONLY, 0))==-1) {
			perror(argv[op]);
			offset++;
		} else if ((l=read(instrf, buf, 100))==-1) {
			perror(argv[op]);
			offset++;
		} else if (buf[0]!='S' || buf[1]!='B' || buf[2]!='I') {
			fprintf(stderr,"%s: Not SBI file\n",argv[op]);
			offset++;
		} else if (l<51) {
			fprintf(stderr,"%s: Short file\n",argv[op]);
			offset++;
		} else {	
			instr.channel = op-offset;
			instr.key = FM_PATCH;
			instr.device = dev;
		
			for (i=0;i<16;i++) {
				instr.operators[i]=buf[i+0x24];
			}
	
			if (write(fm, &instr, sizeof(instr))==-1) perror("/dev/sequencer");

			if (verbose) fprintf(stderr,"Loaded %d with %s\n",op-offset,argv[op]);
		}
		close(instrf);
	}
	if (verbose) 
		fprintf(stderr,"Initialised %d FM-instruments.\n",argc-offset);

	if (offset == optind) exit(0); else exit(1);
}
