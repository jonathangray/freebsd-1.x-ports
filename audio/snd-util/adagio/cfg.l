%{
/*
 * cfg file reader, Greg Lee, 8/93.
 */
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
/*
 * can't include allphase.h because of
 * a conflict with the #define of ECHO
 */
#undef ECHO
#include "adagio.h"
#include "allphase.h"
#undef ECHO
#define ECHO (void) fwrite( (char *) yytext, yyleng, 1, yyout )

#ifndef SBDIR
#define SBDIR "/etc"
#endif
#define DEFCFGFILE SBDIR##"/default.cfg"
#define DEFFMCFGFILE SBDIR##"/fm.cfg"
#define DEFEXTCFGFILE SBDIR##"/external.cfg"

static char doing_drums = 0;
static int patchno = -1, i;
static char *patchname;
static char cfg_flag = 1;
static char fm_flag = 0;
static char ext_flag = 0;
static int track = 0, channel = 0, prog = 0, device = 0;
static int tone_bank;
static void attach_new_tone(int patchno, char *vc_name);
static void set_ext_prog(int patchno);
static int cfg_verbose;
static struct voice_type *voice;

%}
%s T P Q R
wh	[ \t]+
owh	[ \t]*
%%
^#.*\n	;
^{owh}"FM"{owh}\n {
	fm_flag = 1;
	ext_flag = 0;
	voice = fm_voice;
}
^{owh}"EXTERNAL"{owh}\n {
	fm_flag = 0;
	ext_flag = 1;
	voice = ext_voice;
}
^{owh}"GUS"{owh}\n {
	fm_flag = 0;
	ext_flag = 0;
	voice = gus_voice;
}
^{owh}"[Melodic"{wh}"Patches]" {
	doing_drums = 0;
}
^{owh}"[Drum"{wh}"Patches]" {
	doing_drums = 1;
}
"{owh}129-256"{wh}"begin_multipatch"{wh}"default"{wh}"blank" {
	doing_drums = 1;
}
^.*"use_note".*		;
^.*"override_patch".*	;
^{owh}"PatchDir:".*	;

^{owh}[0-9,]+("="|{wh})[^\n\r\t\ ]+ {
	char *vc_name, *dev_name, *gm_name = NULL;
	patchno = atoi(yytext);
	if (cfg_flag && !doing_drums) patchno--;
	tone_bank = 0;
	if (patchno >= 0) {
		i = 0;
		while (isspace(yytext[i])) i++;
		while (isdigit(yytext[i])) i++;
		if (yytext[i] == ',') {
			i++;
			tone_bank = atoi(yytext + i);
			while (isdigit(yytext[i])) i++;
		}
		while (isspace(yytext[i])) i++;
		if (!cfg_flag && yytext[i] == '=') i++;
		if ( (patchname = strrchr(yytext + i, '\\')) == NULL)
			patchname = yytext + i;
		else patchname++;
		if (doing_drums) patchno += 128;
		vc_name = strcpy( malloc(strlen(patchname)+1), patchname );
		if (patchno < 256) gm_name = gm_voice[patchno].vname;
		if (gm_name == NULL) gm_name = "drum";
		if (fm_flag) dev_name = "fm";
		else if (ext_flag) dev_name = "external";
		else dev_name = "wave";
		if (cfg_verbose) printf("voice %s[%d,%d] = %s patch %s\n",
			gm_name, patchno, tone_bank, dev_name, vc_name);
		if (!tone_bank) voice[patchno].vname = vc_name;
		else attach_new_tone(patchno, vc_name);
		if (ext_flag) set_ext_prog(patchno);
	}
	BEGIN(P);
}

^{owh}[A-Z][^\n\r\t\=,]+(","{owh}[0-9]+)?{owh}"="{owh}[^\n\r\t\ ]+ {
	char *vc_name, *dev_name, *gm_name = NULL;
	int i,j,w;
	char *vn;
	patchno = -1;
	w = 0;
	while (isspace(yytext[w])) w++;
	for (i = 0; i < 256; i++) {
		vn = gm_voice[i].vname;
		if (vn == NULL) continue;
		for (j = 0; yytext[j+w] && vn[j] && yytext[j+w] == vn[j]; j++) ;
		if (!yytext[j+w] || yytext[j+w] == '=' || yytext[j+w] == ',') break;
		if (!vn[j] && isspace(yytext[j+w])) break;
	}
	if (i < 256 && vn != NULL) patchno = i;
	tone_bank = 0;
	if (patchno >= 0) {
		i = w;
		while (yytext[i] != '=' && yytext[i] != ',') i++;
		if (yytext[i] == ',') {
			i++;
			tone_bank = atoi(yytext + i);
		}
		while (yytext[i] != '=') i++;
		i++;
		while (isspace(yytext[i])) i++;
		if ( (patchname = strrchr(yytext + i, '\\')) == NULL)
			patchname = yytext + i;
		else patchname++;
		vc_name = strcpy( malloc(strlen(patchname)+1), patchname );
		if (patchno < 256) gm_name = gm_voice[patchno].vname;
		if (gm_name == NULL) gm_name = "drum";
		if (fm_flag) dev_name = "fm";
		else if (ext_flag) dev_name = "external";
		else dev_name = "wave";
		if (cfg_verbose) printf("voice %s[%d,%d] = %s patch %s\n",
			gm_name, patchno, tone_bank, dev_name, vc_name);
		if (!tone_bank) voice[patchno].vname = vc_name;
		else attach_new_tone(patchno, vc_name);
		if (ext_flag) set_ext_prog(patchno);
	}
	else {
		fprintf(stderr, "Bad line \"%s ...\" in cfg file.\n", yytext);
		exit(1);
	}
	BEGIN(P);
}

^{owh}"track"{wh}[0-9]+{wh}"->"{wh} {
	track = atoi(yytext+5);
	device = prog = 0;
	BEGIN(T);
}
^{owh}"track"{wh}[0-9]+{wh}"channel"{wh}[0-9]+"->"{wh} {
	track = atoi(yytext+5);
	for (i = 0; yytext[i] != 'l'; i++) ;
	channel = atoi(yytext + i + 1);
	device = prog = 0;
	BEGIN(T);
}
^{owh}"channel"{wh}[0-9]+{wh}"melodic" {
	i = atoi(yytext + 7) - 1;
	if (i >= 0 && i <= 15) percsel &= ~(1 << i);
}
^{owh}"channel"{wh}[0-9]+{wh}"percussion" {
	i = atoi(yytext + 7) - 1;
	if (i >= 0 && i <= 15) percsel |= (1 << i);
}
^{owh}"channel"{wh}[0-9]+{wh}"->"{wh} {
	channel = atoi(yytext+7);
	device = prog = 0;
	BEGIN(T);
}
<T>\n  {
    int eligible = 1;

    if (	(device || prog) &&
		(track || channel) &&
        	track >= 0 && track <= 128 &&
        	prog >= 0 && prog <= 256 &&
        	channel >= 0 && channel <= 16 ) {

	if (device) switch (device) {
	    case 1: if (sb_dev < 0) eligible = 0; break;
	    case 2: if (gus_dev < 0) eligible = 0; break;
	    case 3: if (ext_dev < 0) eligible = 0; break;
	}

    	if (cfg_verbose) {
	    if (track) {
		if (track_direction[track-1])
		fprintf(stderr, "repeated track redirection\n");
	    }
	    else if (channel && channel_direction[channel-1])
		fprintf(stderr, "repeated channel redirection\n");
	}

	if (channel && prog && device == 3 && eligible) {
	    if (!ext_chan[channel-1] &&
		ext_voice[prog-1].vname != NULL) {
		    int ext_tot = 0;

		    if (recording_track) ext_tot++;
		    while (ext_program[ext_tot] > 0 && ext_tot < NUM_CHANS) {
			ext_tot++;
		    }
		    if (ext_tot < NUM_CHANS &&
			ext_tot < XMAXCHAN ) {
				ext_program[ext_tot] = prog;
				ext_tot++;
				ext_chan[channel-1] = ext_tot;
		    }
	    	    else eligible = 0;
	    }
	    else eligible = 0;
	}

	if (track && eligible) track_direction[track-1] =
    		(prog << 7) | (channel << 2) | device;
	else if (channel && eligible) channel_direction[channel-1] =
    		(prog << 7) | device;

    	if (cfg_verbose) {
    	    if (track) printf("track %d ", track);
    	    if (channel) printf("channel %d ", channel);
    	    printf("->");
    	    if (device) switch (device) {
    	    	case 1: printf(" fm"); break;
    	    	case 2: printf(" wave"); break;
    	    	case 3: printf(" ext"); break;
    	    }
	    if (prog > 0 && prog <= 256 && gm_voice[prog-1].vname != NULL)
    		printf(" prog %s[%d]", gm_voice[prog-1].vname, prog-1);
	    if (!eligible) printf(" not eligible");
	    printf("\n");
    	}
    }
    else if (cfg_verbose) printf("illegal or null redirection\n");
    track = channel = 0;
    patchno = -1;
    BEGIN(0);
}
<T>"f"[a-z]*{owh} {
	device = 1;
}
<T>"w"[a-z]*{owh} {
	device = 2;
}
<T>"e"[a-z]*{owh} {
	device = 3;
}
<T>[0-9]+ {
	prog = 1 + atoi(yytext);
}
<T>[A-Z][^\n\r]+ {
	int j;
	char *vn;
	for (i = 0; i < 128; i++) {
		vn = gm_voice[i].vname;
		for (j = 0; yytext[j] && vn[j] && yytext[j] == vn[j]; j++) ;
		if (!yytext[j]) break;
	}
	if (i < 128) prog = i + 1;
}
^{owh}"no"{wh}"fm"                        { exclude_fm = 1; sb_dev = -1; }
^{owh}"no"{wh}"gus"                       { exclude_gus = 1; gus_dev = -1; }
^{owh}"no"{wh}"external"                  { extflag = 0; ext_dev = ext_index = -1; }
^{owh}"piano"{wh}"only"                   piano_only = 1;
^{owh}"no"{wh}"drum"{wh}"rolls"           setting_drum_rolls = 0;
^{owh}"no"{wh}"pseudo"{wh}"stereo"        setting_pstereo = 0;
^{owh}"no"{wh}"4"{wh}"op"{wh}"fm"         setting_4op_mode = 0;
^{owh}"gus"{wh}"tuning"{wh}[0-9]+         setting_gus_tuning = atoi(yytext+10);
^{owh}"gus"{wh}"voices"{wh}[0-9]+         setting_gus_voices = atoi(yytext+10);
^{owh}"reverb"{wh}"delay"{wh}[0-9]+       setting_reverb = atoi(yytext+6);
^{owh}"chorus"{wh}"spread"{wh}[0-9]+      setting_chorus_spread = atoi(yytext+13);
^{owh}"vibrato"{wh}"depth"{wh}[0-9]+      setting_vibrato_depth = atoi(yytext+13);
^{owh}"vibrato"{wh}"speed"{wh}[0-9]+      setting_vibrato_speed = atoi(yytext+13);
^{owh}"vibrato"{wh}"sweep"{wh}[0-9]+      setting_vibrato_sweep = atoi(yytext+13);

<P>{wh}[0-9]+	{
	if (voice[patchno].volume == -1) {
		voice[patchno].trnsps = 64;
		voice[patchno].echo_delay = 64;
		voice[patchno].echo_atten = 64;
		voice[patchno].fix_dur = 0;
		if (patchno < 128) {
			voice[patchno].fix_key = 0;
			voice[patchno].chorus_spread = 80;
		}
		else if (patchno < 256) {
			voice[patchno].fix_key = patchno - 128;
			voice[patchno].chorus_spread = 64;
		}
	}
	voice[patchno].volume = atoi(yytext);
	BEGIN(Q);
}
<Q>{wh}[0-9\+\-]+	{
	voice[patchno].trnsps = 64 + atoi(yytext);
	BEGIN(R);
}
<R>{wh}[0-9]+	{
	voice[patchno].fix_key = atoi(yytext);
}

. |
\n	{
	patchno = -1;
	BEGIN(0);
}
%%

static int k1_prog_used[64];

static void
set_ext_prog(int patchno)
{
	int i;

	/* clear record of prog numbers in use */
	for (i = 0; i < 64; i++) k1_prog_used[i] = 0;

	/* mark other prog numbers in use */
	for (i = 0; i < MAX_TONE_VOICES; i++) {
		if (i != patchno && ext_voice[i].vname != NULL && ext_voice[i].prog < 64)
			k1_prog_used[ ext_voice[i].prog ] = 1;
	}
	/* find first unused prog number */
	for (i = 0; i < 64; i++) if (!k1_prog_used[i]) break;

	if (i == 64) {
		fprintf(stderr, "cfg: no more k1 voices available for %s\n",
			ext_voice[patchno].vname);
		ext_voice[patchno].vname = NULL;
		return;
	}
	ext_voice[patchno].prog = i;
	ext_voice[patchno].loaded = 0;
	if (cfg_verbose) printf("prog %d used for voice %d\n", i, patchno);
}

static void
attach_new_tone(int patchbase, char *vc_name)
{
	int pgm, linkfrom;

	for (pgm = 256; pgm < 512; pgm++)
		if (voice[pgm].vname == NULL) break;
	if (pgm == 512) {
		fprintf(stderr, "no room for tone patch %s\n", vc_name);
		exit(1);
	}
	linkfrom = patchbase;
	while (voice[linkfrom].next && voice[linkfrom].bank != tone_bank)
		linkfrom = voice[linkfrom].next - 1;
	if (voice[linkfrom].next) pgm = linkfrom;
	else voice[linkfrom].next = pgm + 1;
	voice[pgm].vname = vc_name;
	voice[pgm].bank = tone_bank;
	voice[pgm].fix_dur = 0;
	voice[pgm].trnsps = 64;
	voice[pgm].echo_delay = 64;
	voice[pgm].echo_atten = 64;
	if (patchbase < 128) {
		voice[pgm].volume = 64;
		voice[pgm].fix_key = 0;
		voice[pgm].chorus_spread = 80;
	}
	else {
		voice[pgm].volume = 100;
		voice[pgm].fix_key = patchbase - 128;
		voice[pgm].chorus_spread = 64;
	}
	patchno = pgm;
/*printf("%s at pgm=%d linked to %d\n", vc_name, pgm, linkfrom);*/
}

void read_cfg(char *localdir, char *defaultdir2, char *defaultdir)
{
	char pname[200], midname[80];
	char *fname;
	FILE *f;

	cfg_verbose = really_verbose;
	fname = midi_file_path;
	fname += strlen(fname) - 1;
	while (fname >= midi_file_path && *fname != '/') fname--;
	fname++;
	strcpy(midname, fname);
	if (!strcmp(".mid", midname + strlen(midname) - 4))
		midname[strlen(midname)-4] = '\0';
	if (!strcmp(".gio", midname + strlen(midname) - 4))
		midname[strlen(midname)-4] = '\0';
	strcat(midname,".cfg");

	if (yyin == NULL) yyin = stdin;

	if ((f = freopen(DEFCFGFILE, "r", yyin)) != NULL) {
		voice = gus_voice;
		yylex();
		yy_init = 1;
		fm_flag = 0;
		ext_flag = 0;
	}
	if ((f = freopen(DEFFMCFGFILE, "r", yyin)) != NULL) {
		fm_flag = 1;
		voice = fm_voice;
		yylex();
		yy_init = 1;
		fm_flag = 0;
		ext_flag = 0;
	}
	if ((f = freopen(DEFEXTCFGFILE, "r", yyin)) != NULL) {
		ext_flag = 1;
		voice = ext_voice;
		yylex();
		yy_init = 1;
		fm_flag = 0;
		ext_flag = 0;
	}

	strcpy(pname, localdir);
	strcat(pname, midname);
	f = freopen(pname, "r", yyin);

	if (f == NULL) {
		cfg_flag = 0;
		strcpy(pname, localdir);
		strcat(pname, "ultrasnd.ini");
		f = freopen(pname, "r", yyin);
	}
	if (f == NULL) {
		strcpy(pname, defaultdir2);
		strcat(pname, midname);
		f = freopen(pname, "r", yyin);
	}
	if (f == NULL) {
		strcpy(pname, defaultdir);
		strcat(pname, midname);
		f = freopen(pname, "r", yyin);
	}

	if (f == NULL) return;

	cfg_verbose = verbose;

	voice = gus_voice;
	yylex();
}
	/** not used now
^{owh}[A-Z][^\n\r\t\=,]+{wh}("solo"|"poly")$ {
	int i,j,w;
	char *vn;
	w = 0;
	while (isspace(yytext[w])) w++;
	for (i = 0; i < 128; i++) {
		vn = gm_voice[i].vname;
		for (j = 0; yytext[j+w] && vn[j] && yytext[j+w] == vn[j]; j++) ;
		if (!yytext[j+w] || yytext[j+w] == '=' || yytext[j+w] == ',') break;
		if (!vn[j] && isspace(yytext[j+w])) break;
	}
	if (i >= 128) {
		fprintf(stderr, "Bad line \"%s\" in cfg file.\n", yytext);
		exit(1);
	}
	if (yytext[yyleng-1] == 'o') gm_voice[i].flags = 1;
	else gm_voice[i].flags = 0;
	if (cfg_verbose) printf("set voice %s[%d] to %s\n",
		gm_voice[i].vname, i, gm_voice[i].flags? "solo":"poly");
}
	**/
