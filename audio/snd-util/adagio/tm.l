%{
/*
 * midi file assembler, Greg Lee, 8/93.
 */
#include <ctype.h>
#include "midifile.h"

static int format, ntrks, division = 96, currtempo = 500000;
static unsigned long currtime = 0, lasttime = 0, delta;
static int note_off_flag,
    note_on_flag,
    this_chan,
    this_pitch,
    this_vol;
static char mdata[5];
static int delta_flag = 0;

static unsigned long old_currtempo = 500000;
static unsigned long realtime = 0, lastrealtime = 0, old_realtime = 0;
static unsigned long old_currtime = 0, revised_time;

#define MAX_HISTORY 512
static unsigned long tempo_history[MAX_HISTORY];
static unsigned long tempo_history_time[MAX_HISTORY];
static int tempo_history_count = 0;
static unsigned long find_tempo();
unsigned long scsec2ticks (double scsecs, int division, unsigned long tempo);

#undef yywrap
int yywrap();

%}
%s H HX T U C P V PR PC
wh	[ \t]+
%%
^#.*\n	;
<HX>^"Header"{wh}.*\n {
/*printf("(header of later file is ignored)\n");*/
	BEGIN(0);
}
<H>^"Header"{wh}"DELTA"{wh} {
	delta_flag = 1;
}
<H>^"Header"{wh} ;
<H>"format="[0-2]{wh} {
	format = atoi(yytext+7);
}
<H>"ntrks="[0-9]+{wh} {
	ntrks = atoi(yytext+6);
}
<H>"division="[0-9]+[ ]*\n {
	division = atoi(yytext+9);
	BEGIN(0);
/*printf("format %d, ntrks %d, division %d\n", format, ntrks, division);*/
	return;
}

^"Track start"\n {
	realtime = old_realtime = lastrealtime = lasttime = currtime = old_currtime = 0;
	currtempo = old_currtempo = 500000;
	BEGIN(T);
}
^"Track end"\n {
	BEGIN(0);
	return;
}

<T>^[ ]*[0-9]+"."[0-9][0-9]"."[0-9][0-9]{wh} {
	int i = 0;
	unsigned long secs, csecs, scsecs, thistime, delta_realtime;

	secs = atoi(yytext);
	while (yytext[i] != '.') i++;
	i++;
	csecs = atoi(yytext + i);
	while (yytext[i] != '.') i++;
	i++;
	scsecs = atoi(yytext + i);
	thistime = secs * 1600 + csecs * 16 + scsecs;

	revised_time = realtime;
	if (delta_flag) realtime = lastrealtime + thistime;
	else realtime = thistime;

	while (revised_time < realtime) {
		unsigned long save_time = revised_time;

		currtempo = find_tempo();
		if (currtempo != old_currtempo) {
			old_currtempo = currtempo;
			old_realtime = save_time;
			old_currtime = currtime;
		}
		delta_realtime = revised_time - old_realtime;
		currtime = old_currtime + scsec2ticks(delta_realtime, division, currtempo);
	}

	delta = currtime - lasttime;
	lasttime = currtime;
	lastrealtime = realtime;
	note_on_flag = note_off_flag = 0;
	this_chan = -1;
	BEGIN(U);
/**
	printf("\nTime=%d ticks, real %d = %d sec + %d csec + %d scsecs",
		currtime, realtime, secs, csecs, scsecs);
**/
}

<T>^[ ]*[0-9]+{wh} {
	currtime = atoi(yytext);
	if (delta_flag) delta = currtime;
	else delta = currtime - lasttime;
	lasttime = currtime;
	note_on_flag = note_off_flag = 0;
	this_chan = -1;
	BEGIN(U);
	/*printf("Time=%d ", currtime);*/
}
<U>"Time signature="[0-9]+"/"[0-9]+"["[0-9]+"]".*"="[0-9]+.*"="[0-9]+[ ]*\n {
/*0 Time signature=4/4[2]  MIDI-clocks/click=24  32nd-notes/24-MIDI-clocks=8*/
	int i;
	mdata[0] = atoi(yytext+15);
	for (i = 14; yytext[i] != '['; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[2] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[3] = atoi(yytext+i);
	if(mf_write_meta_event(delta,time_signature,mdata,4)!=4) return(-1);
	BEGIN(T);
}
<U>"SMPTE"(","){wh}"hour="[0-9]+{wh}"minute="[0-9]+{wh}"second="[0-9]+{wh}.*"="[0-9]+\n {
	int i;
	for (i = 5; yytext[i] != '='; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[2] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[3] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[4] = atoi(yytext+i);
	if(mf_write_meta_event(delta,smpte_offset,mdata,5)!=5) return(-1);
	BEGIN(T);
}
<U>"Key signature"(","){wh}.*"flats="[0-9]+{wh}"minor="[0-9]+[ ]*\n {
	int i;
	for (i = 14; yytext[i] != '='; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	if(mf_write_meta_event(delta,key_signature,mdata,2)!=2) return(-1);
	BEGIN(T);
}
<U>("Sysex"|"Arbitrary bytes")(","){wh}"leng="[0-9]+.*\n{wh}"Dump = <".*">"\n {
	int i, j, leng;
	int type = 0xf7;
	char *data;
	for (i = 6; yytext[i] != '='; i++) ;
	i++;
	leng = atoi(yytext+i);
	for (; yytext[i] != '<'; i++) ;
	i++;
	if (yytext[0] == 'S') {
		i += 5; /* initial "\0xf0" is not part of the dump */
		leng--;
		type = system_exclusive;
	}
	data = (char *)malloc(leng);
	for (j = 0; j < leng; j++) {
		if (yytext[j+i] == '\\' && yytext[j+i+1] == '0') {
			data[j] = strtol(yytext+j+i+1, (char **)NULL, 16);
			i += 4;
		}
		else {
			fprintf(stderr, "mistake in sysex\n");
			exit(1);
		}
	}
	if(mf_write_midi_event(delta,type,0,data,leng)!=leng) return(-1);
	free(data);
	BEGIN(T);
}
<U>"Meta".*"sequence number ="[ ]*"="[ ]*[0-9]+\n {
	int i, seqnum;
	for (i = 4; yytext[i] != '='; i++) ;
	i++;
	seqnum = atoi(yytext+i);
	mf_write_seqnum(delta,seqnum);
}
<U>"Meta Text"(","){wh}"type=".*"leng="[0-9]+\n{wh}.*" = \"".*\"\n {
	int i, j, type, leng;
	char *data;
	for (i = 10; yytext[i] != '='; i++) ;
	i++;
	type = strtol(yytext+i, (char **)NULL, 16);
	for (; yytext[i] != '='; i++) ;
	i++;
	leng = atoi(yytext+i);
	for (; yytext[i] != '"'; i++) ;
	i++;
	data = (char *)malloc(leng);
	for (j = 0; j < leng; j++) {
		if (yytext[j+i] == '\\' && yytext[j+i+1] == '0') {
			data[j] = strtol(yytext+j+i+1, (char **)NULL, 16);
			i += 4;
		}
		else data[j] = yytext[j+i];
	}
	if(mf_write_meta_event(delta,type,data,leng)!=leng) return(-1);
	BEGIN(T);
}
<U>"Meta".*"sequencer-specific".*"leng="[0-9]+\n{wh}.*" = <".*">"\n {
	int i, j, leng;
	char *data;
	for (i = 6; yytext[i] != '='; i++) ;
	i++;
	leng = atoi(yytext+i);
	for (; yytext[i] != '<'; i++) ;
	i++;
	data = (char *)malloc(leng);
	for (j = 0; j < leng; j++) {
		if (yytext[j+i] == '\\' && yytext[j+i+1] == '0') {
			data[j] = strtol(yytext+j+i+1, (char **)NULL, 16);
			i += 4;
		}
		else {
			fprintf(stderr, "mistake in seq. specific\n");
			exit(1);
		}
	}
	if(mf_write_meta_event(delta,sequencer_specific,data,leng)!=leng) return(-1);
	free(data);
	BEGIN(T);
}
<U>"Meta".*"unrecognized".*"type=".*"leng="[0-9]+\n{wh}.*" = \"".*\"\n {
	int i, j, leng, type;
	char *data;
	for (i = 6; yytext[i] != '='; i++) ;
	i++;
	type = strtol(yytext+i, (char **)NULL, 16);
	for (; yytext[i] != '='; i++) ;
	i++;
	leng = atoi(yytext+i);
	for (; yytext[i] != '"'; i++) ;
	i++;
	data = (char *)malloc(leng);
	for (j = 0; j < leng; j++) {
		if (yytext[j+i] == '\\' && yytext[j+i+1] == '0') {
			data[j] = strtol(yytext+j+i+1, (char **)NULL, 16);
			i += 4;
		}
		else data[j] = yytext[j+i];
	}
	if(mf_write_meta_event(delta,type,data,leng)!=leng) return(-1);
	free(data);
	BEGIN(T);
}
<U>"Meta".*"end".*"track"\n {
	if(mf_write_meta_event(delta,end_of_track,mdata,0)!=0) return(-1);
	BEGIN(T);
}
<U>"Parameter"(","){wh}"chan="[0-9]+{wh}.*[^\[\n]*"["[0-9]+"]".*"="[0-9]+.*\n {
	int i;
	for (i = 10; yytext[i] != '='; i++) ;
	i++;
	this_chan = atoi(yytext+i);
	for (; yytext[i] != '['; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	if(mf_write_midi_event(delta,control_change,this_chan-1,mdata,2)!=2) return(-1);
	BEGIN(T);
}
<U>"Channel pressure"(","){wh}"chan="[0-9]+{wh}"pressure="[0-9]+\n {
/*     8 Channel pressure, chan=1 pressure=2*/
	int i;
	for (i = 16; yytext[i] != '='; i++) ;
	i++;
	this_chan = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	if(mf_write_midi_event(delta,channel_aftertouch,this_chan-1,mdata,1)!=1) return(-1);
	BEGIN(T);
}
<U>"Pressure"(","){wh}"chan="[0-9]+{wh}"pitch="[0-9]+{wh}"press="[0-9]+\n {
	int i;
	for (i = 9; yytext[i] != '='; i++) ;
	i++;
	this_chan = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	if(mf_write_midi_event(delta,poly_aftertouch,this_chan-1,mdata,2)!=2) return(-1);
	BEGIN(T);
}
<U>"Pitchbend"(","){wh}"chan="[0-9]+{wh}"msb="[0-9]+{wh}"lsb="[0-9]+\n {
/*    57 Pitchbend, chan=1 msb=124 lsb=62*/
	int i;
	for (i = 10; yytext[i] != '='; i++) ;
	i++;
	this_chan = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[0] = atoi(yytext+i);
	for (; yytext[i] != '='; i++) ;
	i++;
	mdata[1] = atoi(yytext+i);
	if(mf_write_midi_event(delta,pitch_wheel,this_chan-1,mdata,2)!=2) return(-1);
	BEGIN(T);
}
<U>"Tempo".*=[0-9]+[ ]*\n {
	int i;
	for (i = 5; yytext[i] != '='; i++) ;
	currtempo = atoi(yytext+i+1);
	if (tempo_history[tempo_history_count] != currtempo &&
	    tempo_history_time[tempo_history_count] <= realtime) {
		if (tempo_history_count < MAX_HISTORY - 1) tempo_history_count++;
		tempo_history[tempo_history_count] = currtempo;
		tempo_history_time[tempo_history_count] = realtime;
	}
	mf_write_tempo(delta,currtempo); 
	BEGIN(T);
}
<U>"Program"(","){wh} {
	/*printf("program d=%d\n", delta);*/
	BEGIN(PC);
}
<PR>"program="[^\[\n]*"["[0-9]+"]"[ ]*\n {
	int i;
	for (i = 6; yytext[i] != '['; i++) ;
	mdata[0] = atoi(yytext+i+1);
	/*printf("doing program chng %d\n", mdata[0]);*/
	if(mf_write_midi_event(delta,program_chng,this_chan-1,mdata,1)!=1) return(-1);
	BEGIN(T);
}
<U>("Note"|"Drum"|"Roll")" on"(","){wh} {
	/*printf("note on at d=%d ", delta);*/
	note_on_flag = 1;
	BEGIN(C);
}
<U>("Note"|"Drum"|"Roll")" off"(","){wh} {
	/*printf("note off at d=%d ", delta);*/
	note_off_flag = 1;
	BEGIN(C);
}
<C>"chan="[0-9]+{wh} {
	this_chan = atoi(yytext+5);
	/*printf("note channel is %d ", this_chan);*/
	BEGIN(P);
}
<PC>"chan="[0-9]+{wh} {
	this_chan = atoi(yytext+5);
	/*printf("program channel is %d ", this_chan);*/
	BEGIN(PR);
}
<P>[^\[]*"["[0-9]+"]"{wh} {
	int i;
	for (i = 6; yytext[i] != '['; i++) ;
	this_pitch = atoi(yytext+i+1);
	/*printf("note pitch is %d ", this_pitch);*/
	BEGIN(V);
}
<V>"vol="[0-9]+[ ]*\n {
	this_vol = atoi(yytext+4);
	/*printf("note vol is %d\n", this_vol);*/
	mdata[0] = this_pitch;  /** note number **/
	mdata[1] = this_vol; /** velocity **/
	if (note_on_flag) {
		if(!mf_write_midi_event(delta,note_on,this_chan-1,mdata,2)) return(-1);
	}
	else if (note_off_flag) {
		if(!mf_write_midi_event(delta,note_off,this_chan-1,mdata,2)) return(-1);
	}
	BEGIN(T);
}
<C,P,U>\n {
	printf("(not finished)\n");
	BEGIN(T);
}

.	;
%%

static
unsigned long
find_tempo()
{
  int i;
  unsigned long old_tempo = currtempo;
  unsigned long new_tempo = currtempo;

  for (i = 0; i <= tempo_history_count; i++) {
    if (tempo_history_time[i] <= realtime) old_tempo = tempo_history[i];
    new_tempo = tempo_history[i];
    if (tempo_history_time[i] > revised_time) break;
  }
  if (i > tempo_history_count) {
#ifdef DEBUG_TIMES
printf("[big i %d, old_tempo %d]\n", i, old_tempo);
#endif
    revised_time = realtime;
    return(old_tempo);
  }
  if (tempo_history_time[i] > realtime) {
#ifdef DEBUG_TIMES
printf("[past %d, old_tempo %d]\n", tempo_history_time[i], old_tempo);
#endif
    revised_time = realtime;
    return(old_tempo);
  }
  revised_time = tempo_history_time[i];
#ifdef DEBUG_TIMES
printf("[revised_time %d, new_tempo %d]\n", revised_time, new_tempo);
#endif
  return(new_tempo);
}


unsigned long
scsec2ticks (double scsecs, int division, unsigned long tempo)
{
  double smpte_format, smpte_resolution;

  if (division > 0)
    return (unsigned long) ( 0.5 + (scsecs * (double)division * 625.0) / (double)tempo );
  else {
    smpte_format = (double)((unsigned char)((division & 0xff00)>>8));
    smpte_resolution = (double)((unsigned char)(division & 0xff));
    return (unsigned long) ( 0.5 + scsecs * smpte_format * smpte_resolution * 625.0 );
  }
}


FILE *fp;
myputc(c) { return(putc(c,fp));}

int mywritetrack(track)
int track;
{
	yylex();
	return(1);
}

static int optind = 1;
static int xargc;
static int extra_tracks;
static char **xargv;

main(argc,argv)
char **argv;
{
	tempo_history_count = 0;
	tempo_history[tempo_history_count] = currtempo;
	tempo_history_time[tempo_history_count] = 0;

	extra_tracks = argc - 2;
	xargv = argv;
	xargc = argc;

	Mf_putc = myputc;
	Mf_writetrack = mywritetrack;
	BEGIN(H);

	if (optind >= argc) {
		if((fp = fopen("tmout.mid","w")) == 0L)
			exit(1);
		yylex();
		mfwrite(format, ntrks, division, fp);
	}
	else {
		(void) yywrap();
		yylex();
		mfwrite(format, ntrks + extra_tracks, division, fp);
	}
}


char   *basename (s)
char   *s;
{
	char   *p;

	if (p = (char *)strrchr (s, '/'))
		return (++p);
	else
		return (s);
}				/* basename */

int yywrap()
{	char *file, *mtname;

	if (optind == xargc) return(1);

	file = basename(xargv[optind]);
	mtname = (char *)malloc(strlen(file)+4);
	strcpy (mtname, file);
	if (yyin == NULL) yyin = stdin;
	if (freopen (mtname, "r", yyin) == NULL) {
		strcat (mtname, ".mt");
		if ((freopen (mtname, "r", yyin) == NULL)) {
			(void) fprintf (stderr,
		    	"Couldn't open file: %s\n", xargv[optind]);
			exit (1);
		}
	}
	if (optind > 1) {
		BEGIN(HX);
		return(0);
	}
	if (!strcmp(".mt", mtname + strlen(mtname) - 3))
		mtname[strlen(mtname)-3] = '\0';
	strcat(mtname,".mid");
	if((fp = fopen(mtname,"w")) == 0L)
		exit(1);
	optind++;
	return(0);
}
