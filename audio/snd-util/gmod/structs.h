/* This file is part of the GMOD package */

struct note_info
{
  unsigned char note;
  unsigned char sample;
  unsigned char command[2];
  unsigned char parm1[2], parm2[2];
};

struct voice_info
{

  short slide_period;
  short slide_period_goal;
  short pitchbender;
  short slide_goal;
  short slide_rate;
  short last_rate;		/* last rate of slide */

  short arpeg_note[3];		/* for arpeggio */

  short position;		/* for "pattern-loop" command */
  short pattern;		/* for "pattern-loop" command */

  unsigned char sample;
  unsigned char note;
  unsigned char volume;

  signed char panning;

  /* Pitch sliding */

  unsigned char slide_pitch;
  unsigned char glissando;

  signed char volslide;
  unsigned char finevol;

  unsigned char loop_times;	/* for "pattern-loop" command */

  unsigned char cut_count;	/* for "note cut" command */
  unsigned char delay_count;	/* for "note delay" command */

  unsigned char arpeg_num;	/* for arpeggio */
  unsigned char arpeg_curr;	/* for arpeggio */

  unsigned char retrigger;	/* for note retrigger */
  unsigned char retrig_count;	/* for note retrigger */

  unsigned char vibra_rate;	/* vibrato rate */
  unsigned char vibra_old_rate;	/* previous vibrato rate */
  unsigned char vibra_depth;	/* vibrato depth */
  unsigned char vibra_position;	/* vibrato position */
  unsigned char vibra_wave;	/* vibrato waveform */

  unsigned char tremolo;	/* tremolo rate */
  unsigned char tremolo_old;	/* previous tremolo rate */
  unsigned char tremolo_depth;	/* tremolo depth */
  unsigned char tremolo_position;	/* tremolo position */
  unsigned char tremolo_wave;	/* tremolo waveform */
};

struct effect_info
{
  short pattern;		/* position in pattern for jumps, etc */
  short position;		/* position in song for jumps, etc */
  unsigned char delay_notes;	/* for "pattern delay" command */
  unsigned char loop_chan;	/* for "pattern loop" command */
};

struct song_info
{
  short nr_patterns;
  short songlength;
  unsigned char nr_channels;
  unsigned char lowest_note;
  unsigned char highest_note;
  unsigned char play_speed;
  unsigned char vol_type;
  signed char panning[MAX_TRACK];
};

struct options_info
{
  unsigned char main_volume;
  unsigned char loop_breaker;
  unsigned char show_empty_samples;
};

typedef struct note_info pattern[MAX_TRACK][64];
