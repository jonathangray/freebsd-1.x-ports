#include <stdio.h>
#include "adagio.h"
#include "allphase.h"

/* Change default settings here: */
int piano_only = false;		/* change all instruments to piano ? */
int exclude_fm = false;		/* don't use fm card? */
int exclude_gus = false;	/* don't use GUS? */
int extflag = true;		/* use external synth? */
int percsel = PSELECT;		/* which channels are for percussion? */
int no_solo = true;             /* treal all instruments as polyphonic? */
int setting_drum_rolls = true;	/* use midi chorus depth controller? */
int setting_pstereo = true;	/* add pseudo-stereo effect? */
int setting_4op_mode = true;	/* use 4 operator fm in preference to 2 operator? */
int setting_gus_tuning = -600;	/* -1000 to +1000 */
int setting_gus_volume = 60;	/* 0 to 100 */
int setting_gus_voices = 32;	/* 14 to 32 */
int setting_meter_color = 0;	/* 0=expression, 1=volume, 2=chorus depth,
				   3=reverberation, 4=stereo pan */
int setting_meter_column = 0;	/* 0=channel, 1=pitch, 2=instrument (GM group) */
int setting_reverb = 50;	/* 0=none, 0 to 100 */
int setting_chorus_spread = 32;	/* 0 to 100 */
int setting_vibrato_depth = 32;	/* 0 to 100 */
int setting_vibrato_speed = 75;	/* 0 to 100 */
int setting_vibrato_sweep = 20;	/* 0 to 100 */

/* set by command line flags */
int verbose = false;
int really_verbose = false;
int recording_track = false;
int recording_program = 0;
int recording_channel = 1;

/* misc */
int midi_is_open_for_input = false;
int seq_max_queue = 1024;
long running_time = 0;

/* path + name of file being played */
char midi_file_path[MAXPATHLEN];

/* for xmp meter: */
unsigned char curr_note_count[NUM_CHANS];
unsigned short curr_note_velocity[NUM_CHANS];

int program[NUM_CHANS] =
{NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI,
 NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI};
int ext_program[NUM_CHANS] =
{NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI,
 NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI, NO_VOI};
int ext_chan[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
int ext_poly[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
int ext_pan[NUM_CHANS] =
{-1, -1, -1, -1, -1, -1, -1, -1,
 -1, -1, -1, -1, -1, -1, -1, -1};
int chorus_depth[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
int modulation_wheel[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
int vibrato_rate[NUM_CHANS] =
{-1, -1, -1, -1, -1, -1, -1, -1,
 -1, -1, -1, -1, -1, -1, -1, -1};
int vibrato_depth[NUM_CHANS] =
{-1, -1, -1, -1, -1, -1, -1, -1,
 -1, -1, -1, -1, -1, -1, -1, -1};
int voice_bank[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
int ext_voice_bank[NUM_CHANS] =
{0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0};
unsigned char *fm_sysex[NUM_CHANS] =
{NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
 NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

int ext_polyphony = 0;
int seq_fd = -1;
struct synth_info card_info[MAXCARDS];
int gus_dev = -1, sb_dev = -1, ext_dev = -1, ext_index = -1;
int nrsynths = 0;
int nrmidis = 0;
int need_4op_mode = false;


/* current values of some midi channel controllers */
int main_volume[NUM_CHANS] =
{NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME,
 NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME,
 NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME,
 NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME, NORMAL_VOLUME};

int expression[NUM_CHANS] =
{NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR,
 NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR,
 NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR,
 NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR, NORMAL_EXPR};

int reverberation[NUM_CHANS] =
{-1, -1, -1, -1, -1, -1, -1, -1,
 -1, -1, -1, -1, -1, -1, -1, -1};

unsigned char urpn1[NUM_CHANS];
unsigned char urpn2[NUM_CHANS];
int drum_reverberation[NUM_DRUMS];
int drum_pan[NUM_DRUMS];
int drum_chorus_depth[NUM_DRUMS];

int user_ctrl_height[5][NUM_CHANS];

struct cfg_type {
	char *vname;
} cfg_voice[256];

unsigned long track_direction[MAX_TRACKS];
unsigned long channel_direction[MAX_TRACKS];

struct meta_text_type *meta_text_list = NULL;

/*
 * names and info for general-midi instruments
 */
struct short_voice_type gm_voice[256] = {
/* 0*/	{ "Acoustic Grand Piano",0 },
/* 1*/	{ "Bright Acoustic Grand",0 },
/* 2*/	{ "Electric Grand Piano",0 },
/* 3*/	{ "Honky-tonk Piano",	0 },
/* 4*/	{ "Rhodes Piano",	0 },
/* 2*/	{ "Chorused Piano",	0 },
/* 6*/	{ "Harpsichord",	0 },
/* 7*/	{ "Clavinet",		0 },
/* 8*/	{ "Celesta",		0 },
/* 9*/	{ "Glockenspiel",	0 },
/* 10*/	{ "Musicbox",		0 },
/* 11*/	{ "Vibraphone",		0 },
/* 12*/	{ "Marimba",		0 },
/* 13*/	{ "Xylophone",		0 },
/* 14*/	{ "Tubular Bells",	0 },
/* 15*/	{ "Dulcimer",		0 },
/* 16*/	{ "Hammond Organ",	0 },
/* 17*/	{ "Percussive Organ",	0 },
/* 18*/	{ "Rock Organ",		0 },
/* 19*/	{ "Church Organ",	0 },
/* 20*/	{ "Reed Organ",		0 },
/* 21*/	{ "Accordion",		0 },
/* 22*/	{ "Harmonica",		1 },
/* 23*/	{ "Tango Accordion",	0 },
/* 24*/	{ "Nylon Guitar",	0 },
/* 25*/	{ "Steel Guitar",	0 },
/* 26*/	{ "Jazz Guitar",	1 },
/* 27*/	{ "Clean Guitar",	0 },
/* 28*/	{ "Muted Guitar",	1 },
/* 29*/	{ "Overdriven Guitar",	0 },
/* 30*/	{ "Distortion Guitar",	0 },
/* 31*/	{ "Guitar Harmonics",	0 },
/* 32*/	{ "Acoustic Bass",	0 },
/* 33*/	{ "Finger Bass",	0 },
/* 34*/	{ "Pick Bass",		0 },
/* 35*/	{ "Fretless Bass",	0 },
/* 36*/	{ "Slap Bass 1",	0 },
/* 37*/	{ "Slap Bass 2",	0 },
/* 38*/	{ "Synth Bass 1",	0 },
/* 39*/	{ "Synth Bass 2",	0 },
/* 40*/	{ "Violin",		0 },
/* 41*/	{ "Viola",		0 },
/* 42*/	{ "Cello",		0 },
/* 43*/	{ "Contrabass",		0 },
/* 44*/	{ "Tremolo Strings",	0 },
/* 45*/	{ "Pizzicato String",	0 },
/* 46*/	{ "Orchestral Harp",	0 },
/* 47*/	{ "Timpani",		1 },
/* 48*/	{ "String Ensemble 1",	0 },
/* 49*/	{ "String Ensemble 2",	0 },
/* 50*/	{ "Synth Strings 1",	0 },
/* 51*/	{ "Synth Strings 2",	0 },
/* 52*/	{ "Choir Aahs",		0 },
/* 53*/	{ "Voice Oohs",		0 },
/* 54*/	{ "Synth Voice",	0 },
/* 55*/	{ "Orchestra Hit",	0 },
/* 56*/	{ "Trumpet",		1 },
/* 57*/	{ "Trombone",		1 },
/* 58*/	{ "Tuba",		1 },
/* 59*/	{ "Muted Trumpet",	1 },
/* 60*/	{ "French Horn",	0 },
/* 61*/	{ "Brass Section",	0 },
/* 62*/	{ "Synth Brass 1",	0 },
/* 63*/	{ "Synth Brass 2",	0 },

/* 64*/	{ "Soprano Sax",	1 },
/* 65*/	{ "Alto Sax",		1 },
/* 66*/	{ "Tenor Sax",		1 },
/* 67*/	{ "Baritone Sax",	1 },
/* 68*/	{ "Oboe",		1 },
/* 69*/	{ "English Horn",	1 },
/* 70*/	{ "Bassoon",		1 },
/* 71*/	{ "Clarinet",		1 },
/* 72*/	{ "Piccolo",		1 },
/* 73*/	{ "Flute",		1 },
/* 74*/	{ "Recorder",		1 },
/* 75*/	{ "Pan Flute",		1 },
/* 76*/	{ "Bottle Blow",	1 },
/* 77*/	{ "Shakuhachi",		1 },
/* 78*/	{ "Whistle",		1 },
/* 79*/	{ "Ocarina",		1 },
/* 80*/	{ "Lead1 squareea",	0 },
/* 81*/	{ "Lead2 sawtooth",	0 },
/* 82*/	{ "Lead3 calliope",	0 },
/* 83*/	{ "Lead4 chiff",	0 },
/* 84*/	{ "Lead5 charang",	0 },
/* 85*/	{ "Lead6 voice",	0 },
/* 86*/	{ "Lead7 fifths",	0 },
/* 87*/	{ "Lead8 brass+ld",	0 },
/* 88*/	{ "Pad1 newage",	0 },
/* 89*/	{ "Pad2 warm",		0 },
/* 90*/	{ "Pad3 polysynth",	0 },
/* 91*/	{ "Pad4 choir",		0 },
/* 92*/	{ "Pad5 bowed",		0 },
/* 93*/	{ "Pad6 metallic",	0 },
/* 94*/	{ "Pad7 halo",		0 },
/* 95*/	{ "Pad8 sweep",		0 },
/* 96*/	{ "FX1 rain",		0 },
/* 97*/	{ "FX2 soundtrack",	0 },
/* 98*/	{ "FX3 crystal",	1 },
/* 99*/	{ "FX4 atmosphere",	1 },
/*100*/	{ "FX5 brightness",	1 },
/*101*/	{ "FX6 goblins",	1 },
/*102*/	{ "FX7 echoes",		0 },
/*103*/	{ "FX8 sci-fi",		1 },
/*104*/	{ "Sitar",		0 },
/*105*/	{ "Banjo",		0 },
/*106*/	{ "Shamisen",		0 },
/*107*/	{ "Koto",		0 },
/*108*/	{ "Kalimba",		0 },
/*109*/	{ "Bagpipe",		0 },
/*110*/	{ "Fiddle",		0 },
/*111*/	{ "Shanai",		0 },
/*112*/	{ "Tinkle Bell",	1 },
/*113*/	{ "Agogo Bells",	1 },
/*114*/	{ "Steel Drums",	1 },
/*115*/	{ "Woodblock",		1 },
/*116*/	{ "Taiko Drum",		1 },
/*117*/	{ "Melodic Tom",	1 },
/*118*/	{ "Synth Drum",		1 },
/*119*/	{ "Reverse Cymbal",	1 },
/*120*/	{ "Guitar Fret Noise",	1 },
/*121*/	{ "Breath Noise",	1 },
/*122*/	{ "Seashore",		1 },
/*123*/	{ "Bird Tweet",		1 },
/*124*/	{ "Telephone Ring",	1 },
/*125*/	{ "Helicopter Blade",	1 },
/*126*/	{ "Applause/Noise",	1 },
/*127*/	{ "Gunshot",		1 },
/*128+000*/{  NULL,		0 },
/*128+001*/{  NULL,		0 },
/*128+002*/{  NULL,		0 },
/*128+003*/{  NULL,		0 },
/*128+004*/{  NULL,		0 },
/*128+005*/{  NULL,		0 },
/*128+006*/{  NULL,		0 },
/*128+007*/{  NULL,		0 },
/*128+008*/{  NULL,		0 },
/*128+009*/{  NULL,		0 },
/*128+010*/{  NULL,		0 },
/*128+011*/{  NULL,		0 },
/*128+012*/{  NULL,		0 },
/*128+013*/{  NULL,		0 },
/*128+014*/{  NULL,		0 },
/*128+015*/{  NULL,		0 },
/*128+016*/{  NULL,		0 },
/*128+017*/{  NULL,		0 },
/*128+018*/{  NULL,		0 },
/*128+019*/{  NULL,		0 },
/*128+020*/{  NULL,		0 },
/*128+021*/{  NULL,		0 },
/*128+022*/{  NULL,		0 },
/*128+023*/{  NULL,		0 },
/*128+024*/{  NULL,		0 },
/*128+025*/{  NULL,		0 },
/*128+026*/{  NULL,		0 },
/*128+027*/{  NULL,		0 },
/*128+028*/{  NULL,		0 },
/*128+029*/{  NULL,		0 },
/*128+030*/{  NULL,		0 },
/*128+031*/{  NULL,		0 },
/*128+032*/{  NULL,		0 },
/*128+033*/{  NULL,		0 },
/*128+034*/{  NULL,		0 },
/*128+035*/{ "Acoustic Bass Drum",0 },
/*128+036*/{ "Bass Drum 1",	0 },
/*128+037*/{ "Side Stick",	0 },
/*128+038*/{ "Acoustic Snare",	0 },
/*128+039*/{ "Hand Clap",	0 },
/*128+040*/{ "Electric Snare",	0 },
/*128+041*/{ "Low Floor Tom",	0 },
/*128+042*/{ "Closed High Hat",	0 },

/*128+043*/{ "Hi Floor Tom",	0 },
/*128+044*/{ "Pedal High Hat",	0 },
/*128+045*/{ "Low Tom",		0 },
/*128+046*/{ "Open High Hat",	0 },
/*128+047*/{ "Low-Mid Tom",	0 },
/*128+048*/{ "High-Mid Tom",	0 },
/*128+049*/{ "Crash Cymbal 1",	0 },
/*128+050*/{ "High Tom",	0 },

/*128+051*/{ "Ride Cymbal 1",	0 },
/*128+052*/{ "Chinese Cymbal",	0 },
/*128+053*/{ "Ride Bell",	0 },
/*128+054*/{ "Tambourine",	0 },
/*128+055*/{ "Splash Cymbal",	0 },
/*128+056*/{ "Cow Bell",	0 },
/*128+057*/{ "Crash Cymbal 2",	0 },
/*128+058*/{ "Vibraslap",	0 },

/*128+059*/{ "Ride Cymbal 2",	0 },
/*128+060*/{ "High Bongo",	0 },
/*128+061*/{ "Low Bongo",	0 },
/*128+062*/{ "Mute High Conga",	0 },
/*128+063*/{ "Open High Conga",	0 },
/*128+064*/{ "Low Conga",	0 },
/*128+065*/{ "High Timbale",	0 },
/*128+066*/{ "Low Timbale",	0 },

/*128+067*/{ "High Agogo",	0 },
/*128+068*/{ "Low Agogo",	0 },
/*128+069*/{ "Cabasa",		0 },
/*128+070*/{ "Maraccas",	0 },
/*128+071*/{ "Short Whistle",	0 },
/*128+072*/{ "Long Whistle",	0 },
/*128+073*/{ "Short Guiro",	0 },
/*128+074*/{ "Long Guiro",	0 },

/*128+075*/{ "Claves",		0 },
/*128+076*/{ "High Wood Block",	0 },
/*128+077*/{ "Low Wood Block",	0 },
/*128+078*/{ "Mute Cuica",	0 },
/*128+079*/{ "Open Cuica",	0 },
/*128+080*/{ "Mute Triangle",	0 },
/*128+081*/{ "Open Triangle",	0 }
};
