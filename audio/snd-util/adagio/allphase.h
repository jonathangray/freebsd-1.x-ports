#ifdef __386BSD__
#include <machine/soundcard.h>
#else
#include <sys/soundcard.h>
#endif
#include <sys/param.h>

/* MAXCARDS must be at least one greater than nrsynths as returned by driver */
#define MAXCARDS 3
/* MAXCELLS is the maximum note polyphony available on any synth */
#define MAXCELLS 32

#define MAX_TONE_VOICES 512
#define MAX_GM_VOICES 256
#define MAX_MELODIC_VOICES 128

extern int seq_fd, nrsynths, nrmidis, gus_dev, sb_dev, ext_dev, ext_index;
extern struct synth_info card_info[MAXCARDS];
extern int need_4op_mode;
extern int ext_polyphony;
extern int extflag;
extern int piano_only; /* ignore voice requests */
extern int exclude_fm;
extern int exclude_gus;
extern int verbose;
extern int really_verbose;
extern int recording_track;
extern int recording_program;
extern int recording_channel;
extern int percsel;
extern int no_solo;
extern int setting_drum_rolls;
extern int setting_pstereo;
extern int setting_4op_mode;
extern int setting_gus_tuning;
extern int setting_gus_volume;
extern int setting_gus_voices;
extern int setting_reverb;
extern int setting_chorus_spread;
extern int setting_vibrato_depth;
extern int setting_vibrato_speed;
extern int setting_vibrato_sweep;

/* for xmp meter: */
extern int setting_meter_color;
extern int setting_meter_column;
extern unsigned char curr_note_count[NUM_CHANS];
extern unsigned short curr_note_velocity[NUM_CHANS];

extern int program[NUM_CHANS];
extern int ext_program[NUM_CHANS];
extern int ext_chan[NUM_CHANS];
extern int ext_poly[NUM_CHANS];
extern int chorus_depth[NUM_CHANS];
extern int modulation_wheel[NUM_CHANS];
extern int vibrato_rate[NUM_CHANS];
extern int vibrato_depth[NUM_CHANS];
extern int voice_bank[NUM_CHANS];
extern int ext_voice_bank[NUM_CHANS];
extern int ext_pan[NUM_CHANS];
extern unsigned char *fm_sysex[NUM_CHANS];

#ifndef XMAXCHAN
#define XMAXCHAN 16
#endif

#define NO_VOI -1

extern struct short_voice_type {
	char *vname;
	char flags;
} gm_voice[];

extern struct voice_type {
	char *vname;
	int mem_req;
	unsigned char prog;
	unsigned char fix_key;
	unsigned char modes;
	unsigned char loaded;
	unsigned char fix_dur, trnsps, chorus_spread;
	unsigned char echo_delay, echo_atten;
	unsigned char vibrato_rate, vibrato_depth, vibrato_sweep;
	char volume;
	unsigned char bank;
	unsigned short next;
} gus_voice[];

extern struct voice_type fm_voice[];
extern struct voice_type ext_voice[];

/* default values for main volume and expression controllers for the sake of
 * midi files which do not have those controls (90 is supposed to be normal
 * main volume, but this just seems too low to me): */
#define NORMAL_VOLUME 90
#define NORMAL_EXPR 100
/* no reverb if controller value is <= threshhold */
#define REVERB_THRESHHOLD 10

extern int main_volume[NUM_CHANS];
extern int expression[NUM_CHANS];
extern int reverberation[NUM_CHANS];

#define NUM_DRUMS 82
extern unsigned char urpn1[NUM_CHANS];
extern unsigned char urpn2[NUM_CHANS];
extern int drum_reverberation[NUM_DRUMS];
extern int drum_pan[NUM_DRUMS];
extern int drum_chorus_depth[NUM_DRUMS];


#define USER_EXPRESSION 0
#define USER_MAIN_VOLUME 1
#define USER_CHORUS_DEPTH 2
#define USER_REVERBERATION 3
#define USER_PAN 4
#define USER_NUM_CTLS 5
extern int user_ctrl_height[USER_NUM_CTLS][NUM_CHANS];

extern char midi_file_path[MAXPATHLEN];

#define MAX_TRACKS 128
extern unsigned long track_direction[MAX_TRACKS];
extern unsigned long channel_direction[MAX_TRACKS];

extern int midi_is_open_for_input;
extern int seq_max_queue;
extern long running_time;

struct meta_text_type {
    unsigned long time;
    unsigned char type;
    char *text;
    struct meta_text_type *next;
};

extern struct meta_text_type *meta_text_list;

