/* gusvoice.c, adapted (slightly) from gusload.c by H. Savolainen, --gl */

#include <stdio.h>
#ifdef __386BSD__
#include <machine/ultrasound.h>
#else
#include <sys/ultrasound.h>
#endif
#include <sys/ioctl.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <fcntl.h>
#include "adagio.h"
#include "allphase.h"
#include "gusvoice.h"
#include "sblast.h"

#define K1SINGLESIZE (87+10)
/* in extvoice.c */
int loadext(int pgm, char *data);

struct lib_pat_header {
      unsigned char magic[12];
      unsigned char version[10];
      unsigned char copyright[60];
      unsigned char fill[3];
      unsigned char nr_samples;
};

struct pat_header {
    unsigned char            magic[12];
    unsigned char            version[10];
    unsigned char            description[60];
    unsigned char   instruments;
    unsigned char            voices;
    unsigned char            channels;
    unsigned short  nr_waveforms;
    unsigned short  master_volume;
    unsigned long   data_size;
};

struct lib_sample_header {
    unsigned short  base_freq;

    unsigned char   envelope_rate[6];
    unsigned char   envelope_offset[6];

    unsigned char   tremolo_sweep;
    unsigned char   tremolo_rate;
    unsigned char   tremolo_depth;

    unsigned char   vibrato_sweep;
    unsigned char   vibrato_rate;
    unsigned char   vibrato_depth;

    char	    modes;

    short	    scale_frequency;
    unsigned short  scale_factor;
};




struct sample_header {
    char            name[7];
    unsigned char   fractions;
    long            len;
    long            loop_start;
    long            loop_end;
    unsigned short  base_freq;
    long            low_note;
    long            high_note;
    long            base_note;
    short           detune;
    unsigned char   panning;

    unsigned char   envelope_rate[6];
    unsigned char   envelope_offset[6];

    unsigned char   tremolo_sweep;
    unsigned char   tremolo_rate;
    unsigned char   tremolo_depth;

    unsigned char   vibrato_sweep;
    unsigned char   vibrato_rate;
    unsigned char   vibrato_depth;

    unsigned char	    modes;

    int		scale_frequency;
    unsigned int	scale_factor;		/* from 0 to 2048 or 0 to 2 */
	
    int		volume;
    int		spare[4];
    char data[0];	/* The waveform data starts here */
};


struct patch_info *patch;

static int patch_mem_used = 0;
static int patch_mem_avail = 0;
static int total_patch_mem_avail = 0;

static char *vdata = NULL;
static char cfg_read_flag = 0;
/*static*/ int next_wave_prog = 0;
static int next_fm_prog = 0;
static int want_4op_mode = 0;

#ifndef SBDIR
#define SBDIR "/etc"
#endif

#ifndef GUSPATDIR
#define GUSPATDIR "/dos/ultrasnd/midi"
#endif
#ifndef GUSPATDIR2
#define GUSPATDIR2 "/dos/ultrasnd/midi2"
#endif

#define GUSPATSLDIR GUSPATDIR##"/"
#define GUSPATSLDIR2 GUSPATDIR2##"/"
#define SBSTDLIB SBDIR##"/"##"std.gus"
#define SBDRUMLIB SBDIR##"/"##"drums.gus"

#ifndef PDIR
#define PDIR "/usr/midi/Plib"
#endif
#define SBISLDIR PDIR##"/"

/*
 * Read fm patch libraries.
 */
#define SBDIRO3DRUMS SBDIR##"/drums.o3"
#define SBDIRO3STD SBDIR##"/std.o3"
#define SBDIRSBDRUMS SBDIR##"/drums.sb"
#define SBDIRSBSTD SBDIR##"/std.sb"

#define OWN_FM_VOL
#ifdef OWN_FM_VOL
static char            fm_volume_table[128] =
{-63, -48, -40, -35, -32, -29, -27, -26,	/* 0 -   7 */
 -24, -23, -21, -20, -19, -18, -18, -17,	/* 8 -  15 */
 -16, -15, -15, -14, -13, -13, -12, -12,	/* 16 -  23 */
 -11, -11, -10, -10, -10, -9, -9, -8,	/* 24 -  31 */
 -8, -8, -7, -7, -7, -6, -6, -6,/* 32 -  39 */
 -5, -5, -5, -5, -4, -4, -4, -4,/* 40 -  47 */
 -3, -3, -3, -3, -2, -2, -2, -2,/* 48 -  55 */
 -2, -1, -1, -1, -1, 0, 0, 0,	/* 56 -  63 */
 0, 0, 0, 1, 1, 1, 1, 1,	/* 64 -  71 */
 1, 2, 2, 2, 2, 2, 2, 2,	/* 72 -  79 */
 3, 3, 3, 3, 3, 3, 3, 4,	/* 80 -  87 */
 4, 4, 4, 4, 4, 4, 4, 5,	/* 88 -  95 */
 5, 5, 5, 5, 5, 5, 5, 5,	/* 96 - 103 */
 6, 6, 6, 6, 6, 6, 6, 6,	/* 104 - 111 */
 6, 7, 7, 7, 7, 7, 7, 7,	/* 112 - 119 */
 7, 7, 7, 8, 8, 8, 8, 8};	/* 120 - 127 */

static int
new_fm_vol(int volbyte, int mainvol)
{
    int oldvol, newvol, n;

    oldvol = 0x3f - (volbyte & 0x3f);
    newvol = fm_volume_table[mainvol] + oldvol;

    if (newvol > 0x3f) newvol = 0x3f;
    else if (newvol < 0) newvol = 0;
    n = 0x3f - (newvol & 0x3f);
    return( (volbyte & 0xc0) | (n & 0x3f) );
}

#endif


static int
loadfm(int voice_size, int tpgm, int mainvol, int panning, int reverb)
{
    int n, fm_side, data_size;
    struct sbi_instrument instr;


    if (fm_voice[tpgm].loaded || fm_voice[tpgm].volume == -2)
	return(1);

    if (card_info[sb_dev].instr_bank_size < next_fm_prog + 1) {
	fprintf(stderr, "not enough instruments in banksize of %d\n",
		card_info[sb_dev].instr_bank_size);
	return(0);
    }
/*
 * check that vdata begins with SBI, 2OP, or 4OP and has name
 */
    if (!vdata[0]) return(0);
    /*if (!vdata[SBNAMEOFFSET]) return(0);*/
    if (voice_size == O3VOICESIZE &&
	strncmp(vdata, "4OP", 3) &&
	strncmp(vdata, "2OP", 3)
		) return(0);
    if (voice_size == SBVOICESIZE &&
	strncmp(vdata, "2OP", 3) &&
	strncmp(vdata, "SBI", 3)
		) return(0);

    if (panning < 0) fm_side = 0x30;
    else if (panning < 54) fm_side = 0x10;
    else if (panning > 74) fm_side = 0x20;
    else fm_side = 0x30;

    instr.device = sb_dev;
    instr.channel = next_fm_prog;

    if (voice_size == SBVOICESIZE) {
	    instr.key = FM_PATCH;
	    data_size = 11;
    } else if ((vdata[49] & 0x3f) == 0x3f && (vdata[50] & 0x3f) == 0x3f) {
	    instr.key = FM_PATCH;
	    data_size = 11;
    } else {
	    need_4op_mode = true;
	    instr.key = OPL3_PATCH;
	    data_size = 22;
    }

	/* Dynamic voice allocation routine new_cell() needs to
	 * which voices are 4op, because they can only use the first
	 * 6 cells, so save this information.
	 */
    fm_voice[tpgm].mem_req = instr.key;

#ifdef OWN_FM_VOL
	if (instr.key == FM_PATCH) {
	    vdata[39] = new_fm_vol(vdata[39], mainvol);
	    if (vdata[46] & 1) vdata[38] = new_fm_vol(vdata[38], mainvol);
	    vdata[46] = (vdata[46] & 0xcf) | fm_side;
	    if (reverb > REVERB_THRESHHOLD) {
		unsigned val;
		val = vdata[43] & 0x0f;
		if (val > 0) val--;
		vdata[43] = (vdata[43]&0xf0) | val;
	    }
	}
	else {
	    int mode;
	    if (vdata[46]&1) mode = 2; else mode = 0;
	    if (vdata[57]&1) mode++;
	    vdata[50] = new_fm_vol(vdata[50], mainvol);
	    if (mode == 3) vdata[49] = new_fm_vol(vdata[49], mainvol);
	    if (mode == 1) vdata[39] = new_fm_vol(vdata[39], mainvol);
	    if (mode == 2 || mode == 3) vdata[38] = new_fm_vol(vdata[38], mainvol);
	    vdata[46] = (vdata[46] & 0xcf) | fm_side;
	    vdata[57] = (vdata[57] & 0xcf) | fm_side;
	    if (mode == 1 && reverb > REVERB_THRESHHOLD) {
		unsigned val;
		val = vdata[43] & 0x0f;
		if (val > 0) val--;
		vdata[43] = (vdata[43]&0xf0) | val;
		val = vdata[54] & 0x0f;
		if (val > 0) val--;
		vdata[54] = (vdata[54]&0xf0) | val;
	    }

	}
#endif

	/* Load the patches down to the driver. */
	for (n = 0; n < 32; n++)
	    instr.operators[n] = (n < data_size) ? vdata[SBOFFSET + n] : 0;

	if (write(seq_fd, (char *) &instr, sizeof(instr)) == -1) {
	    if (verbose)
		fprintf(stderr, "can't load fm instrument %d\n", tpgm);
	    return(0);
	}
	/* Save "meta" information about the patch. */
	fm_voice[tpgm].echo_delay = vdata[29];
	fm_voice[tpgm].echo_atten = vdata[30];
	fm_voice[tpgm].chorus_spread = vdata[31];
	fm_voice[tpgm].trnsps = vdata[32];
	fm_voice[tpgm].fix_dur = vdata[33];
#ifdef XVOICE
	fm_voice[tpgm].modes = vdata[34];
#endif
	fm_voice[tpgm].fix_key = vdata[35];
	fm_voice[tpgm].volume = 0;
	return(1);
}

int
find_tone_bank(int dev, int pgm, int bank)
{
	int tpgm = pgm;
	int b, bp = 0, big_bank = 0;
	struct voice_type *voice = NULL;

	if (dev == sb_dev) voice = fm_voice;
	else if (dev == gus_dev) voice = gus_voice;
	else if (dev == ext_index) voice = ext_voice;

	if (voice == NULL) return(pgm);

	while ( (b=voice[tpgm].bank) != bank && voice[tpgm].next) {
	    if (b > big_bank && b < bank) {
#ifdef FTONE_DEBUG
		printf("b=%d (tpgm %d) is better than %d for pgm %d, bank %d\n",
			b, tpgm, big_bank, pgm, bank);
#endif
		big_bank = b;
		bp = tpgm;
	    }
#ifdef FTONE_DEBUG
	    printf("follow from %d (b=%d)", tpgm, b);
#endif
	    tpgm = voice[tpgm].next - 1;
#ifdef FTONE_DEBUG
	    printf(" to %d (b=%d)\n", tpgm, voice[tpgm].bank);
#endif
	}
	if (b != bank) {
	    if (b > big_bank && b < bank) bank = b;
	    else if (!big_bank) tpgm = pgm;
	    else {
		bank = big_bank;
		tpgm = bp;
	    }
	}
	return(tpgm);
}


void read_cfg(char *, char *, char *);
static char local_path[MAXPATHLEN];

int loadvoice(int dev, int pgm, int reverb, int main_volume, int voicepan)
{
    static int std_fd = -1;
    static int drum_fd = -1;
    static int std_fm_fd = -1;
    static int drum_fm_fd = -1;
    static int fm_voicesize, drum_fm_voicesize;
    int lib_fd = -1, voicesize = 0;
    char *libname = NULL;

    int i, n, patfd, tmp, bank, tpgm;
    struct lib_pat_header *lib_header;
    struct lib_sample_header *lib_sample;
    struct pat_header header;
    struct sample_header sample;
    char buf[256];
    long offset;
    char *fname = NULL, *pathname;
    char pfname[80];
    char patfname[MAXPATHLEN];
    char patfname2[MAXPATHLEN];
    char local_patfname[MAXPATHLEN];
    struct voice_type *voice;

    if (pgm == -1) {
	for (i = 0; i < MAX_TONE_VOICES; i++) {
	    gus_voice[i].loaded = 0;
	    fm_voice[i].loaded = 0;
	}
	next_wave_prog = 0;
	next_fm_prog = 0;
	if (!cfg_read_flag) for (i = 0; i < MAX_TONE_VOICES; i++) {
	    gus_voice[i].trnsps = 64;
	    gus_voice[i].fix_dur = 0;
	    gus_voice[i].volume = -1;
	    fm_voice[i].trnsps = 64;
	    fm_voice[i].fix_dur = 0;
	    fm_voice[i].volume = -1;
	    if (i < 128 || i > 255) {
		gus_voice[i].fix_key = 0;
		fm_voice[i].fix_key = 0;
	    }
	    else {
		gus_voice[i].fix_key = i - 128;
		fm_voice[i].fix_key = i - 128;
	    }
	}
	patch_mem_used = 0;
	patch_mem_avail = -1;

	if (midi_file_path[0]) {
	    strcpy(local_path, midi_file_path);
	    fname = local_path;
	    fname += strlen(fname) - 1;
	    while (fname >= local_path && *fname != '/') fname--;
	    fname++;
	    fname[0] = '\0';

	    if (!cfg_read_flag) {
		if (local_path[0] == '*')
	            read_cfg(NULL, GUSPATSLDIR2, GUSPATSLDIR);
	        else read_cfg(local_path, GUSPATSLDIR2, GUSPATSLDIR);
	        cfg_read_flag = 1;
	    }
	}
	need_4op_mode = false;
	if (sb_dev >= 0 && card_info[sb_dev].synth_subtype != FM_TYPE_OPL3)
		setting_4op_mode = false;
	return(0);
    }

    if (dev < 0) return(0);

    if (dev != ext_index && card_info[dev].instr_bank_size == 0) return(1);

    want_4op_mode = setting_4op_mode;

    if (dev == sb_dev) voice = fm_voice;
    else if (dev == gus_dev) voice = gus_voice;
    else if (dev == ext_index) voice = ext_voice;
    else {
	fprintf(stderr, "loadvoice: unknown device\n");
	exit(1);
    }

    /* msb of controller #0 */
    bank = (pgm >> 16) & 0x7f;
    pgm &= 0xff;

    tpgm = find_tone_bank(dev, pgm, bank);

    if (voice[tpgm].loaded) return (1);

    if (voice[tpgm].volume == -2) return(0);

    if (vdata == NULL) vdata = (char *) malloc(GUSVOICESIZE);
    if (vdata == NULL) {
	fprintf(stderr, "out of memory\n");
    }

    if (dev == ext_index) {
	if (voice[tpgm].vname == NULL) {
	    voice[tpgm].volume = -2;
	    return(0);
	}

	strcpy(pfname, voice[tpgm].vname);
#ifdef K1
	strcat(pfname, ".k1s");
#else
	strcat(pfname, ".xxx");
#endif
	fname = pfname;

	strcpy(patfname, SBISLDIR);
	strcat(patfname, fname);
	strcpy(local_patfname, local_path);
	strcat(local_patfname, fname);

	pathname = local_patfname;
	if (local_path[0]=='*'||(patfd = open(pathname, O_RDONLY, 0)) == -1) {
	        pathname = patfname;
	        if ((patfd = open(pathname, O_RDONLY, 0)) == -1) {
	            perror(pathname);
	            exit(-1);
	        }
	}

	if ((voicesize = read (patfd, vdata, K1SINGLESIZE)) != K1SINGLESIZE) {
	    fprintf (stderr, "%s: wrong size file\n", pathname);
	    exit (-1);
	}
	close(patfd);

	if (!loadext(voice[tpgm].prog, vdata)) {
	    if (really_verbose) printf("could not load ext %s patch\n", fname);
	    voice[tpgm].volume = -2;
	    return(0);
	}
	if (really_verbose) printf("ext %s patch loaded\n", fname);
	voice[tpgm].loaded = 1;
	return(1);
    }

    if (dev == sb_dev && voice[tpgm].vname != NULL && voice[tpgm].vname[0]) {

	strcpy(pfname, voice[tpgm].vname);
	strcat(pfname, ".sbi");
	fname = pfname;

	strcpy(patfname, SBISLDIR);
	strcat(patfname, fname);
	strcpy(local_patfname, local_path);
	strcat(local_patfname, fname);

	pathname = local_patfname;
	if (local_path[0]=='*'||(patfd = open(pathname, O_RDONLY, 0)) == -1) {
	        pathname = patfname;
	        if ((patfd = open(pathname, O_RDONLY, 0)) == -1) {
	            perror(pathname);
	            exit(-1);
	        }
	}

	if ((voicesize = read (patfd, vdata, O3VOICESIZE)) != O3VOICESIZE) {
	  if (voicesize > O3VOICESIZE || voicesize < SBVOICESIZE - 1) {
	    fprintf (stderr, "%s: wrong size file\n", pathname);
	    exit (-1);
	  }
	  voicesize = SBVOICESIZE;
	}
	close(patfd);

#define STATIC_STEREO
	if (!loadfm(voicesize, tpgm, main_volume, voicepan, reverb)) {
	    if (really_verbose) printf("could not load fm %s patch\n", fname);
	    voice[tpgm].volume = -2;
	    return(0);
	}
	voice[tpgm].loaded = 1;
	voice[tpgm].prog = next_fm_prog++;
#ifdef STATIC_STEREO
	if (tpgm < 256) {
	if (!loadfm(voicesize, tpgm, main_volume, (voicepan+64)%128, reverb)) {
	    if (really_verbose) printf("could not load fm %s patch\n", fname);
	    voice[tpgm+256].volume = -2;
	}
	else {
	    voice[tpgm+256].loaded = 1;
	    voice[tpgm+256].prog = next_fm_prog++;
	}
	}
#endif
#ifdef XVOICE
	{   int xprog = fm_voice[tpgm].modes - 1;
	    if (xprog >= 0 && xprog < 256) {
		if (!loadfm(voicesize, xprog, main_volume, voicepan, reverb)) {
		    voice[xprog].volume = -2;
		}
		else {
		    voice[xprog].loaded = 1;
		    voice[xprog].prog = next_fm_prog++;
		}
	    }
	}
#endif
	if (really_verbose) printf("fm %s patch loaded\n", fname);
	return(1);
    }
    if (dev == gus_dev) {
        if (voice[tpgm].vname == NULL || !voice[tpgm].vname[0]) {
    	    if (verbose)
    		fprintf(stderr, "no gus patch for voice %d\n", pgm);
    	    voice[tpgm].volume = -2;
    	    return(0);
        }
    
        tmp = gus_dev;
        ioctl(seq_fd, SNDCTL_SYNTH_MEMAVL, &tmp);
        if (patch_mem_avail < 0) total_patch_mem_avail = tmp;
        patch_mem_avail = tmp;
        if (total_patch_mem_avail <= 0) {
    	fprintf(stderr, "warning: no memory found on gus\n");
    	return(0);
        }
        patch_mem_used = total_patch_mem_avail - patch_mem_avail;
    
        if (voice[pgm].mem_req > patch_mem_avail) {
    	return (0);
        }
    }

    if (pgm <= 127) {
      if (dev == gus_dev) {
	libname = SBSTDLIB;
	if (std_fd < 0 && (std_fd = open(libname, O_RDONLY, 0)) == -1) {
	    perror(libname);
	    exit(1);
	}
	lib_fd = std_fd;
	voicesize = GUSVOICESIZE;
      }
      else if (dev == sb_dev) {
	libname = SBDIRO3STD;
	if (std_fm_fd < 0) fm_voicesize = O3VOICESIZE;
	if (std_fm_fd < 0 && (!want_4op_mode ||
		 (std_fm_fd = open(libname, O_RDONLY, 0)) == -1)) {
	  libname = SBDIRSBSTD;
	  fm_voicesize = SBVOICESIZE;
	  if (std_fm_fd < 0 && (std_fm_fd = open(libname, O_RDONLY, 0)) == -1) {
	    perror(libname);
	    exit(1);
	  }
	}
	lib_fd = std_fm_fd;
	voicesize = fm_voicesize;
      }
      offset = pgm * voicesize;
    } else {
      if (dev == gus_dev) {
	if (pgm - 128 > 127) {
	    fprintf(stderr, "gusvoice: pgm %d out of range\n", pgm);
	    exit(1);
	}
	libname = SBDRUMLIB;
	if (drum_fd < 0 && (drum_fd = open(libname, O_RDONLY, 0)) == -1) {
	    perror(libname);
	    exit(1);
	}
	lib_fd = drum_fd;
	voicesize = GUSVOICESIZE;
      }
      else if (dev == sb_dev) {
	libname = SBDIRO3DRUMS;
	if (drum_fm_fd < 0) drum_fm_voicesize = O3VOICESIZE;
	if (drum_fm_fd < 0 && (!want_4op_mode ||
		 (drum_fm_fd = open(libname, O_RDONLY, 0)) == -1)) {
	  libname = SBDIRSBDRUMS;
	  drum_fm_voicesize = SBVOICESIZE;
	  if (drum_fm_fd < 0 && (drum_fm_fd = open(libname, O_RDONLY, 0)) == -1) {
	    perror(libname);
	    exit(1);
	  }
	}
	lib_fd = drum_fm_fd;
	voicesize = drum_fm_voicesize;
      }
      offset = (pgm - 128) * voicesize;
    }

    if (lseek(lib_fd, offset, 0) == -1) {
	perror(libname);
	exit(1);
    }

    if (read(lib_fd, vdata, voicesize) != voicesize) {
	fprintf(stderr, "%s: Short patch file\n", libname);
	exit(1);
    }

    if (dev == sb_dev) {
	if (!loadfm(voicesize, tpgm, main_volume, voicepan, reverb)) return(0);
    }
    else if (dev == gus_dev) {

	strcpy(pfname, voice[tpgm].vname);
	strcat(pfname, ".pat");
	fname = pfname;

	if (fname == NULL || !*fname) {
	    return(0);
	}
	strcpy(patfname, GUSPATSLDIR);
	strcat(patfname, fname);
	strcpy(patfname2, GUSPATSLDIR2);
	strcat(patfname2, fname);
	strcpy(local_patfname, local_path);
	strcat(local_patfname, fname);

	lib_header = (struct lib_pat_header *) vdata;
	lib_sample = (struct lib_sample_header *)(vdata + sizeof(*lib_header));

	if (voice[pgm].volume == -1) {
	    voice[pgm].fix_dur = lib_header->version[0];
	    voice[pgm].trnsps = lib_header->version[1];
/* (version[2,3] has patch tuning -- not used now) */
	    if (lib_header->version[4])
		voice[pgm].chorus_spread = lib_header->version[4];
	    if (lib_header->version[5])
		voice[pgm].echo_delay = lib_header->version[5];
	    if (lib_header->version[8])
		voice[pgm].echo_atten = lib_header->version[8];
	    voice[pgm].volume = lib_header->version[6];
	    if (lib_header->version[7])
	    voice[pgm].fix_key = lib_header->version[7];
	}

	pathname = local_patfname;
	if (local_path[0]=='*'||(patfd = open(pathname, O_RDONLY, 0)) == -1) {
	    pathname = patfname2;
	    if ((patfd = open(pathname, O_RDONLY, 0)) == -1) {
	        pathname = patfname;
	        if ((patfd = open(pathname, O_RDONLY, 0)) == -1) {
	            perror(pathname);
	            exit(-1);
	        }
	    }
	}

	if (read (patfd, buf, 0xef) != 0xef) {
	  fprintf (stderr, "%s: Short file\n", pathname);
	  exit (-1);
	}

	memcpy ((char *) &header, buf, sizeof (header));

	if (strncmp ((char *) header.magic, "GF1PATCH110", 12)) {
	    fprintf (stderr, "%s: Not a patch file\n", pathname);
	    exit (-1);
	}

	if (strncmp ((char *) header.version, "ID#000002", 10)) {
	    fprintf (stderr, "%s: Incompatible patch file version\n", pathname);
	    exit (-1);
	}

	header.nr_waveforms = *(unsigned short *) &buf[85];
	header.master_volume = *(unsigned short *) &buf[87];
	offset = 0xef;


	/*for (i = 0; i < lib_header->nr_samples; i++) {*/
	for (i = 0; i < header.nr_waveforms; i++) {

	    if (lseek (patfd, offset, 0) == -1) {
	        perror(pathname);
	        exit (-1);
	    }

	    if (read (patfd, &buf, sizeof (sample)) != sizeof (sample)) {
	        fprintf (stderr, "%s: Short file\n", pathname);
	        exit (-1);
	    }

	    memcpy ((char *) &sample, buf, sizeof (sample));

	    /*
	     * Since some fields of the patch record are not 32bit aligned, we must
	     * handle them specially.
	     */
	    sample.low_note = *(long *) &buf[22];
	    sample.high_note = *(long *) &buf[26];
	    sample.base_note = *(long *) &buf[30];
	    sample.detune = *(short *)&buf[34];
	    sample.panning = (unsigned char)buf[36];

	    memcpy(sample.envelope_rate, &buf[37], 6);
	    memcpy(sample.envelope_offset, &buf[43], 6);

	    sample.tremolo_sweep = (unsigned char) buf[49];
	    sample.tremolo_rate = (unsigned char) buf[50];
	    sample.tremolo_depth = (unsigned char) buf[51];

	    sample.vibrato_sweep = (unsigned char) buf[52];
	    sample.vibrato_rate = (unsigned char) buf[53];
	    sample.vibrato_depth = (unsigned char) buf[54];

	    gus_voice[tpgm].vibrato_sweep = sample.vibrato_sweep;
	    gus_voice[tpgm].vibrato_rate = sample.vibrato_rate;
	/* used for chorus_spread -- temporarily inherit */
	    gus_voice[tpgm].chorus_spread = gus_voice[pgm].chorus_spread;
	    gus_voice[tpgm].echo_delay = gus_voice[pgm].echo_delay;
	    gus_voice[tpgm].echo_atten = gus_voice[pgm].echo_atten;

	    gus_voice[tpgm].vibrato_depth = sample.vibrato_depth;

	    sample.modes = (unsigned char) buf[55];

	    gus_voice[tpgm].modes = sample.modes;

	    sample.scale_frequency = *(short *)&buf[56];
	    sample.scale_factor = *(unsigned short *)&buf[58];

	    offset = offset + 96;

	    patch = (struct patch_info *) malloc(sizeof(*patch) + sample.len);

	    patch->key = GUS_PATCH;
	    patch->device_no = gus_dev;

	    patch->instr_no = next_wave_prog;

	    patch->mode = sample.modes | WAVE_TREMOLO |
	                                 WAVE_VIBRATO | WAVE_SCALE;
	    patch->len = sample.len;
	    patch->loop_start = sample.loop_start;
	    patch->loop_end = sample.loop_end;
	    patch->base_note = sample.base_note;
	    patch->high_note = sample.high_note;
	    patch->low_note = sample.low_note;
	    patch->base_freq = sample.base_freq;
	    patch->detuning = sample.detune;
	    /*patch->panning = (sample.panning - 7) * 16;*/
	    patch->panning = 0;

	    memcpy(patch->env_rate, sample.envelope_rate, 6);
	    memcpy(patch->env_offset, sample.envelope_offset, 6);

	    if (lib_sample->envelope_rate[2] != patch->env_rate[2]) {
	        int r = (reverb * setting_reverb) / 50;
		int dec = lib_sample->envelope_rate[2];
		r = (127 - r) / 6;
		if (r < 0) r = 0;
		if (r > 28) r = 28;
		r += dec & 0x3f;
		if (r > 63) r = 63;
		dec = (dec & 0xc0) | r;
		patch->env_rate[1] = lib_sample->envelope_rate[1];
		patch->env_rate[2] = dec;
	    }

	    if (reverb > 0 && setting_reverb) {
	            int r = (reverb * setting_reverb) / 50;
	            if (r > 127) r = 127;
	            if (pgm < 120)
	        	patch->env_rate[3] = (2<<6) | (12 - (r>>4));
	            else if (pgm > 127)
	        	patch->env_rate[1] = (3<<6) | (63 - (r>>1));
	    }

#define VR_NUM 2
#define VR_DEN 3
	    {
	        unsigned voff, poff;
	        for (n = 0; n < 6; n++) {
	            voff = patch->env_offset[n];
	            poff = 2 + main_volume + 63 + gus_voice[tpgm].volume / 2;
	            voff = ((poff + VR_NUM*256) * voff + VR_DEN*128) / (VR_DEN*256);
	            patch->env_offset[n] = voff;
	        }
	    }

	    patch->tremolo_sweep = sample.tremolo_sweep;
	    patch->tremolo_rate = sample.tremolo_rate;
	    patch->tremolo_depth = sample.tremolo_depth;

	    patch->vibrato_sweep = sample.vibrato_sweep;
	    patch->vibrato_rate = sample.vibrato_rate;
	    patch->vibrato_depth = sample.vibrato_depth;

	    patch->scale_frequency = sample.scale_frequency;
	    patch->scale_factor = sample.scale_factor;

	    patch->volume = gus_voice[tpgm].volume;

	    if (lseek(patfd, offset, 0) == -1) {
	            perror(pathname);
	            exit(-1);
	    }
	    if (read(patfd, patch->data, sample.len) != sample.len) {
	            fprintf(stderr, "%s: Short file\n", pathname);
	            exit(-1);
	    }
	    if (write(seq_fd, (char *) patch, sizeof(*patch) + sample.len) == -1) {
	            /*patch_mem_used = patch_mem_avail;*/
	            close(patfd);
	            free(patch);
	            return (0);
	    }
	    offset += sample.len;
	    patch_mem_used += sample.len;
	    free(patch);
	}


	close(patfd);

    }

    voice[tpgm].loaded = 1;
    if (dev == gus_dev) voice[tpgm].prog = next_wave_prog++;
    else if (dev == sb_dev) voice[tpgm].prog = next_fm_prog++;

    if (dev == gus_dev && really_verbose && fname != NULL)
	printf("%s, %d mem used of %d, %d still free\n", fname,
	       patch_mem_used, total_patch_mem_avail, total_patch_mem_avail - patch_mem_used);

    return (1);
}
