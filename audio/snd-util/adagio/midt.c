/*
 * mftext
 * 
 * Convert a MIDI file to verbose text.
 */

#include <stdio.h>
#include <ctype.h>
#ifdef __386BSD__
#include <stdlib.h>
#else
#include <getopt.h>
#endif
#define NO_LC_DEFINES
#include "midifile.h"
#include "adagio.h"
#include "allphase.h"

void prtime();
extern int Mf_nomerge;
static FILE *F;
int SECONDS;      /* global that tells whether to display seconds or ticks */
int division;        /* from the file header */
long tempo = 500000; /* the default tempo is 120 beats/minute */
static int quiet = 0;
static long last_time = 0;
static long last_realtime = 0;
static int delta_flag = 0;

filegetc()
{
	return(getc(F));
}


main(argc,argv)
char **argv;
{
	extern int optind;
	extern char * optarg;
	int opt;
	FILE *efopen();

	SECONDS = 0;


	while((opt = getopt(argc,argv,"sqd")) != -1)
	    switch(opt) {
		case 's' : SECONDS = 1; break;
		case 'q': quiet = 1; break;
		case 'd': delta_flag = 1; break;
	}

	if ( argc == optind )
	    F = stdin;
	else
	    F = efopen(argv[optind],"r");

	initfuncs();
	Mf_getc = filegetc;
	midifile();
	Mf_nomerge = 1;
	fclose(F);
	exit(0);
}


FILE *
efopen(name,mode)
char *name;
char *mode;
{
	FILE *f;
	extern int errno;
	extern char *sys_errlist[];
	extern int sys_nerr;
	char *errmess, *midname;

	if ( (f=fopen(name, mode)) == NULL ) {
		midname = (char *)malloc(strlen(name)+5);
		strcpy(midname, name);
		strcat(midname, ".mid");
		if ( (f=fopen(midname, mode)) != NULL ) return(f);
		(void) fprintf(stderr,"*** ERROR *** Cannot open '%s'!\n",name);
		if ( errno <= sys_nerr )
			errmess = sys_errlist[errno];
		else
			errmess = "Unknown error!";
		(void) fprintf(stderr,"************* Reason: %s\n",errmess);
		exit(1);
	}
	return(f);
}

void
error(s)
char *s;
{
	fprintf(stderr,"Error: %s\n",s);
	exit(1);
}

void
txt_header(format,ntrks,ldivision)
{
        division = ldivision; 
	if (delta_flag)
		printf("Header DELTA format=%d ntrks=%d division=%d\n",format,ntrks,division);
	else
		printf("Header format=%d ntrks=%d division=%d\n",format,ntrks,division);
}

void
txt_trackstart()
{
	printf("Track start\n");
	last_time = 0;
}

void
txt_trackend()
{
	printf("Track end\n");
}

char *notename[12] = { "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" };

void
txt_noteon(chan,pitch,vol)
{
	char *drumname;

	if (quiet) return;
	prtime();
	if (chan == 9 || chan == 15) {
	    drumname = gm_voice[128+pitch].vname;
	    if (drumname == NULL) drumname = "unknown drum";
	    printf("Drum on, chan=%d %s (%s%d)[%d] vol=%d\n", chan+1,
		drumname, notename[pitch%12], pitch/12-2, pitch, vol);
	}
	else printf("Note on, chan=%d pitch=%s%d[%d] vol=%d\n", chan+1,
	    notename[pitch % 12], pitch/12 - 2, pitch, vol);
}

void
txt_noteoff(chan,pitch,vol)
{
	char *drumname;

	if (quiet) return;
	prtime();
	if (chan == 9 || chan == 15) {
	    drumname = gm_voice[128+pitch].vname;
	    if (drumname == NULL) drumname = "unknown drum";
	    printf("Drum off, chan=%d %s (%s%d)[%d] vol=%d\n", chan+1,
		drumname, notename[pitch%12], pitch/12-2, pitch, vol);
	}
	else printf("Note off, chan=%d pitch=%s%d[%d] vol=%d\n", chan+1,
		notename[pitch % 12], pitch/12 - 2, pitch, vol);
}

void
txt_pressure(chan,pitch,press)
{
	prtime();
	printf("Pressure, chan=%d pitch=%d press=%d\n",chan+1,pitch,press);
}
/**
     Table 3: Status Bytes 176-191; Control and Mode Changes (per channel)
------------------------------------------------------------------------------
 2nd Byte Value |  Function                  |  3rd Byte
Dec |                                        | Value  |  Use
- - | - - - - - - - - - - - - - - - - - - - -|- - - - | - - - - 
  0 | Continuous controller #0               | 0-127  |  MSB
  1 | Modulation wheel                       | 0-127  |  MSB
  2 | Breath control                         | 0-127  |  MSB
  3 | Continuous controller #3               | 0-127  |  MSB
  4 | Foot controller                        | 0-127  |  MSB
  5 | Portamento time                        | 0-127  |  MSB
  6 | Data Entry                             | 0-127  |  MSB
  7 | Main Volume                            | 0-127  |  MSB
  8 | Continuous controller #8               | 0-127  |  MSB
	...
 31 | Continuous controller #31              | 0-127  |  MSB
 32 | Continuous controller #0               | 0-127  |  LSB
 33 | Modulation wheel                       | 0-127  |  LSB
 34 | Breath control                         | 0-127  |  LSB
 35 | Continuous controller #3               | 0-127  |  LSB
 36 | Foot controller                        | 0-127  |  LSB
 37 | Portamento time                        | 0-127  |  LSB
 38 | Data entry                             | 0-127  |  LSB
 39 | Main volume                            | 0-127  |  LSB
 40 | Continuous controller #8               | 0-127  |  LSB
	...
 63 | Continuous controller #31              | 0-127  |  LSB
 64 | Damper pedal on/off (Sustain)          | 0=off  | 127=on
 65 | Portamento on/off                      | 0=off  | 127=on
 66 | Sustenuto on/off                       | 0=off  | 127=on
 67 | Soft pedal on/off                      | 0=off  | 127=on
 68 | Undefined on/off                       | 0=off  | 127=on
	...
 95 | Undefined on/off                       | 0=off  | 127=on
                                              -----------------
 96 | Data entry +1                          |       127
 97 | Data entry -1                          |       127
 98 | Undefined                              |        ?
	...
121 | Undefined                              |        ?
122 | Local control on/off                   | 0=off    127=on   
123 | All notes off (!!)                     |        0
124 | Omni mode off (includes all notes off) |        0
125 | Omni mode on (includes all notes off)  |        0
126 | Poly mode on/off(includes all notes off)|       **
127 | Poly mode on(incl mono=off&all notes off)|      0

 **Note: This equals the number of channels, or zero if the number of channels
         equals the number of voices in the receiver.

**/

char *cname1[8] = {
	"Continuous controller #0",
	"Modulation wheel",
	"Breath control",
	"Continuous controller #3",
	"Foot controller",
	"Portamento time",
	"Data Entry",
	"Main Volume"
};

char *cname2[7] = {
	"Reset controllers",
	"Local control",
	"All notes off",
	"Omni mode off",
	"Omni mode on",
	"Poly mode",
	"Poly mode on"
};

#define GENERAL_4 68
#define HOLD_2 69
#define GENERAL_5 80
#define GENERAL_6 81
#define GENERAL_7 82
#define GENERAL_8 83
#define TREMULO_DEPTH 92
#define CHORUS_DEPTH 93
#define DETUNE 94
#define PHASER_DEPTH 95

void
txt_parameter(chan,control,value)
{
	prtime();
	printf("Parameter, chan=%d ", chan+1);

	if (control < 8) printf("%s[%d] msb=%d\n", cname1[control], control, value);
	else if (control == 10) printf("Pan[%d] %d[=%d]\n", control, value-64, value);
	else if (control == 11) printf("Expression[%d] =%d\n", control, value);
	else if (control < 32) printf("Continuous controller [%d] msb=%d\n", control, value);
	else if (control < 40) printf("%s[%d] lsb=%d\n", cname1[control-32], control, value);
	else if (control < 64) printf("Continuous controller %d[%d] lsb=%d\n",
					control-32, control, value);
	else if (control == 64) printf("Damper pedal[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 65) printf("Portamento[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 66) printf("Sostenuto[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 67) printf("Soft pedal[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 69) printf("Hold2[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 91) printf("Reverb[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 92) printf("Tremulo depth[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 93) printf("Chorus depth[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 94) printf("Detune[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control == 95) printf("Phaser depth[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control < 96) printf("Undefined[%d] %s[=%d]\n", control,
		(value==0)? "off":"on", value);
	else if (control < 98) printf("Data entry[%d] %c1%s[=%d]\n", control,
		(control==96)? '+':'-', (value==127)? "":" (undef. value)", value);
	else if (control == 98) printf("Unregistered Param 1[%d] val=%d\n", control, value);
	else if (control == 99) printf("Unregistered Param 2[%d] val=%d\n", control, value);
	else if (control == 100) printf("Pitch bend sensitivity[%d] =%d\n", control, value);
	else if (control == 101) printf("Fine tuning[%d] =%d\n", control, value);
	else if (control == 102) printf("Coarse tuning[%d] =%d\n", control, value);
	else if (control < 121) printf("Undefined [%d] val=%d\n", control, value);
	else if (control < 128) printf("%s[%d] val=%d\n", cname2[control-121], control, value);
	else printf("impossible controller[%d] val=%d\n", control, value);
}

void
txt_pitchbend(chan,msb,lsb)
{
	prtime();
	printf("Pitchbend, chan=%d msb=%d lsb=%d\n",chan+1,msb,lsb);
}

void
txt_program(chan,program)
{
	prtime();
	printf("Program, chan=%d program=%s[%d]\n",chan+1,gm_voice[program].vname,program);
}

void
txt_chanpressure(chan,press)
{
	prtime();
	printf("Channel pressure, chan=%d pressure=%d\n",chan+1,press);
}
/**
	Sequential Circuits   1	     Bon Tempi	   0x20	    Kawai     0x40
	Big Briar	      2	     S.I.E.L.	   0x21	    Roland    0x41
	Octave / Plateau      3				    Korg      0x42
	Moog		      4	     SyntheAxe	   0x23	    Yamaha    0x43
	Passport Designs      5
	Lexicon		      6
	PAIA		      0x11
	Simmons		      0x12
	Gentle Electric	      0x13
	Fairlight	      0x14
**/
char *man_id[68] = {
	"",
	"Sequential Circuits",
	"Big Briar",
	"Octave/Plateau",
	"Moog",
	"Passport Designs",
	"Lexicon",
	"","","","","","","","","",

	"",
	"PAIA",
	"Simmons",
	"Gentle Electric",
	"Fairlight",
	"","","","","","","","","","","",

	"Bon Tempi",
	"S.I.E.L.",
	"",
	"SyntheAxe",
	"","","","","","","","","","","","",

	"Linux","","","","","","","","","","","","","","","",

	"Kawai",
	"Roland",
	"Korg",
	"Yamaha"
};

void
txt_sysex(leng,mess)
unsigned char *mess;
{
	register int n, c;
	register unsigned char *p = mess;

	prtime();
	printf("Sysex, leng=%d",leng);
	if (leng > 1 && *p == 0xf0) {
		n = *(p+1);
		if (n < 68 && man_id[n][0]) printf(" Id is %s", man_id[n]);
		else printf(" unknown id");
	}
	printf("\n     Dump = <");
	for ( n=0; n<leng; n++ ) {
		c = 0xff & *p++;
		printf( "\\0x%02x" , c);
	}
	printf(">\n");
}

void
txt_metamisc(type,leng,mess)
char *mess;
{	char *p = mess;
	int n, c;

	prtime();
	printf("Meta event, unrecognized, type=0x%02x leng=%d\n",type,leng);
	printf("     Text = \"");
	for ( n=0; n<leng; n++ ) {
		c = 0xff & *p++;
		printf( (isprint(c)||isspace(c)) ? "%c" : "\\0x%02x" , c);
	}
	printf("\"\n");
}

void
txt_metaspecial(leng,mess)
char *mess;
{	char *p = mess;
	int n, c;

	prtime();
	printf("Meta event, sequencer-specific, leng=%d\n",leng);
	printf("\n     Dump = <");
	for ( n=0; n<leng; n++ ) {
		c = 0xff & *p++;
		printf( "\\0x%02x" , c);
	}
	printf(">\n");
}

void
txt_metatext(type,leng,mess)
char *mess;
{
	static char *ttype[] = {
		NULL,
		"Text Event",		/* type=0x01 */
		"Copyright Notice",	/* type=0x02 */
		"Sequence/Track Name",
		"Instrument Name",	/* ...       */
		"Lyric",
		"Marker",
		"Cue Point",		/* type=0x07 */
		"Unrecognized"
	};
	int unrecognized = (sizeof(ttype)/sizeof(char *)) - 1;
	register int n, c;
	register char *p = mess;

	if ( type < 1 || type > unrecognized )
		type = unrecognized;
	prtime();
	printf("Meta Text, type=0x%02x (%s)  leng=%d\n",type,ttype[type],leng);
	printf("     Text = \"");
	for ( n=0; n<leng; n++ ) {
		c = 0xff & *p++;
		printf( (isprint(c)||isspace(c)) ? "%c" : "\\0x%02x" , c);
	}
	printf("\"\n");
}

void
txt_metaseq(num)
{
	prtime();
	printf("Meta event, sequence number = %d\n",num);
}

void
txt_metaeot()
{
	prtime();
	printf("Meta event, end of track\n");
}

void
txt_keysig(sf,mi)
{
	prtime();
	printf("Key signature, sharp/flats=%d  minor=%d\n",sf,mi);
}

void
txt_tempo(ltempo)
long ltempo;
{
	tempo = ltempo;
	prtime();
	printf("Tempo, microseconds-per-MIDI-quarter-note=%d\n",tempo);
}

void
txt_timesig(nn,dd,cc,bb)
{
	int denom = 1;
	int savedd = dd;
	while ( dd-- > 0 )
		denom *= 2;
	prtime();
	printf("Time signature=%d/%d[%d]  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
		nn,denom,savedd,cc,bb);
}

void
txt_smpte(hr,mn,se,fr,ff)
{
	prtime();
	printf("SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d\n",
		hr,mn,se,fr,ff);
}

void
txt_arbitrary(leng,mess)
char *mess;
{	char *p = mess;
	int n, c;

	prtime();
	printf("Arbitrary bytes, leng=%d\n",leng);
	printf("     Text = \"");
	for ( n=0; n<leng; n++ ) {
		c = 0xff & *p++;
		printf( (isprint(c)||isspace(c)) ? "%c" : "\\0x%02x" , c);
	}
	printf("\"\n");
}

void
prtime()
{	long the_time;
	long the_realtime;

	if (delta_flag) {
		the_time = Mf_currtime - last_time;
		the_realtime = Mf_realtime - last_realtime;
	}
	else {
		the_time = Mf_currtime;
		the_realtime = Mf_realtime;
	}
	last_time = Mf_currtime;
	last_realtime = Mf_realtime;

    if(SECONDS)
	printf("%3d.%02d.%02d ", the_realtime / (100*16),
		(the_realtime / 16) % 100,
		the_realtime % 16);
    else
	printf("%6ld ",the_time);
}

initfuncs()
{
	Mf_error = error;
	Mf_header =  txt_header;
	Mf_trackstart =  txt_trackstart;
	Mf_trackend =  txt_trackend;
	Mf_noteon =  txt_noteon;
	Mf_noteoff =  txt_noteoff;
	Mf_pressure =  txt_pressure;
	Mf_parameter =  txt_parameter;
	Mf_pitchbend =  txt_pitchbend;
	Mf_program =  txt_program;
	Mf_chanpressure =  txt_chanpressure;
	Mf_sysex =  txt_sysex;
	Mf_metamisc =  txt_metamisc;
	Mf_seqnum =  txt_metaseq;
	Mf_eot =  txt_metaeot;
	Mf_timesig =  txt_timesig;
	Mf_smpte =  txt_smpte;
	Mf_tempo =  txt_tempo;
	Mf_keysig =  txt_keysig;
	Mf_seqspecific =  txt_metaspecial;
	Mf_text =  txt_metatext;
	Mf_arbitrary =  txt_arbitrary;
}
