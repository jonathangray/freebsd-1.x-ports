
/*
 *	gmod.c	- Module player for GUS and Linux.
 *		(C) Hannu Savolainen, 1993
 *
 *	NOTE!	This program doesn't try to be a complete module player.
 *		It's just a too I used while developing the driver. In
 *		addition it can be used as an example on programming
 *		the LInux Sound Driver with GUS.
 *  NOTE NOTE!  This is only a portion of Hannu's code, adapted for
 *	        mp, with mistakes introduced by Greg Lee.
 */

#include <string.h>
#include <unistd.h>
#include "adagio.h"
#include "allphase.h"
#include "midicode.h"

extern int next_wave_prog;

unsigned char *Mf_file_contents;
int Mf_file_size;

#define CMD_ARPEG		0x00
#define CMD_SLIDEUP		0x01
#define CMD_SLIDEDOWN		0x02
#define CMD_SLIDETO		0x03
#define    SLIDE_SIZE		8
#define CMD_VOLSLIDE		0x0a
#define CMD_JUMP		0x0b
#define CMD_VOLUME		0x0c
#define CMD_BREAK		0x0d
#define CMD_SPEED		0x0f
#define CMD_NOP			0xfe
#define CMD_NONOTE		0xff

#define _MIN(a, b)               ((a) < (b) ? (a) : (b))

#define MAX_TRACK	8
#define MAX_PATTERN	128
#define MAX_POSITION	128

struct note_info
  {
    unsigned char   note;
    unsigned char   vol;
    unsigned char   sample;
    unsigned char   command;
    short           parm1, parm2;
  };

struct voice_info
  {
    int             sample;
    int             note;
    int             volume;
    int             pitchbender;

    /* Pitch sliding */

    int             slide_pitch;
    int             slide_goal;
    int             slide_rate;

    int             volslide;
  };

static int	curr_program[NUM_CHANS] = {
 -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
static int	curr_expression[NUM_CHANS] = {
 NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,
 NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,
 NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR,NORMAL_EXPR};
static int	curr_note[NUM_CHANS];
static int	curr_sample[MAX_TRACK];
static int	curr_arpeggio[MAX_TRACK];

typedef struct note_info pattern[MAX_TRACK][64];
static int             pattern_len[MAX_POSITION];
static int             pattern_tempo[MAX_POSITION];
static pattern        *pattern_table[MAX_PATTERN];

static struct voice_info voices[MAX_TRACK];

static int             nr_channels, nr_patterns, songlength;
static int             tune[MAX_POSITION];
static double          tick_duration;

static int             period_table[] =
{
  856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
  428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
  214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113
};

static int             sample_ok[128], sample_vol[128];
static double          this_time, next_time;
static int             ticks_per_division;
static double          clock_rate;	/* HZ */

static void            play_module ();
static int             play_note (int channel, struct note_info *pat);
static void            lets_play_voice (int channel, struct voice_info *v);
static void	       lets_play_arpeggio (int channel, int tick);

static void
init_voices ()
{
  int             i;

  for (i = 0; i < MAX_TRACK; i++)
    {
      voices[i].sample = 0;
      voices[i].note = 0;
      voices[i].volume = 64;

      voices[i].slide_pitch = 0;
      voices[i].slide_goal = 0;
      voices[i].slide_rate = 0;
      voices[i].pitchbender = 0;

      voices[i].volslide = 0;
    }
}

static unsigned short
intelize (unsigned short v)
{
  return ((v & 0xff) << 8) | ((v >> 8) & 0xff);
}

/**
static unsigned long
intelize4 (unsigned long v)
{
  return
  (((v >> 16) & 0xff) << 8) | (((v >> 16) >> 8) & 0xff) |
  (((v & 0xff) << 8) | ((v >> 8) & 0xff) << 16);
}
**/

static int load_module()
{

  struct sample_header
    {
      char            name[22];
      unsigned short  length;	/* In words */

      unsigned char   finetune;
      unsigned char   volume;

      unsigned short  repeat_point;	/* In words */
      unsigned short  repeat_length;	/* In words */
    };

  int             i, total_mem;
  int             sample_ptr, pattern_loc;

  int             position;

  unsigned char  *tune_ptr;	/* array 0-127 */

  char            *header;
  char		  mname[22];

  int             nr_samples;	/* 16 or 32 samples */
  int             slen, npat;

  header = (char *)Mf_file_contents;

  clock_rate = 50.0;

  for (i = 0; i < MAX_POSITION; i++)
    pattern_len[i] = 64;

  for (i = 0; i < MAX_POSITION; i++)
    pattern_tempo[i] = 0;

  if (header[28] == 0x1a) {
    if (verbose) printf( "load_stm_module\n");
    return(0);
  }

  else if (*(unsigned short *) &header[0] == 0x6669) {
    if (verbose) printf( "not implemented: load_669_module\n");
    return(0);
  }

  else if (!strncmp (header, "MMD0", 4)) {
    if (verbose) printf( "not implemented: load_mmd0_module\n");
    return(0);
  }

  else {
    strncpy (mname, header, 22);
    if (verbose) printf( "\nModule: %s - ", mname);
  }

  if (!strncmp (&header[1080], "M.K.", 4))
    {
      if (verbose) printf( "31 samples\n");
      nr_samples = 31;
    }
  else if (!strncmp (&header[1080], "FLT8", 4))
    {
      if (verbose) printf( "15 samples (FLT8)\n");
      nr_samples = 15;
    }
  else
    {
      if (verbose) printf( "15 samples\n");
      nr_samples = 15;
    }

  if (nr_samples == 31)
    {
      sample_ptr = pattern_loc = 1084;
      slen = header[950];
      tune_ptr = (unsigned char *) &header[952];
    }
  else
    {
      sample_ptr = pattern_loc = 600;
      slen = header[470];
      tune_ptr = (unsigned char *) &header[472];
    }

  npat = 0;
  for (i = 0; i < 128; i++)
    {
      tune[i] = tune_ptr[i];

      if (tune_ptr[i] > npat)
	npat = tune_ptr[i];
if (npat > 63) {
fprintf(stderr, "bad mod format\n");
return(0);
}
    }
  npat++;

  if (verbose) printf("Song length %d, %d patterns.\n", slen, npat);

  sample_ptr += (npat * 1024);	/* Location where the first sample is stored */
  total_mem = 0;

  for (i = 0; i < 32; i++)
    sample_ok[i] = (gus_dev >= 0)? 0 : 1;


  if (gus_dev >= 0) for (i = 0; i < nr_samples; i++)
    {
      int             len, loop_start, loop_end;
      unsigned short  loop_flags = 0;
      char            pname[22];

      struct sample_header *sample;

      struct patch_info *patch;

      sample = (struct sample_header *) &header[20 + (i * 30)];

      len = intelize (sample->length) * 2;
      loop_start = intelize (sample->repeat_point) * 2;
      loop_end = loop_start + (intelize (sample->repeat_length) * 2);

      if (loop_start > len)
	loop_start = 0;
      if (loop_end > len)
	loop_end = len;

      if (loop_end <= loop_start)
	loop_end = loop_start + 1;

      if (loop_end > 2 && loop_end > loop_start)
         loop_flags = WAVE_LOOPING;

      strncpy (pname, sample->name, 20);

      if (len > 0)
	{
	  unsigned char *file_patch_data;

	  if (verbose) printf("Sample %02d: L%05d, S%05d, E%05d V%02d %s\n",
		   i,
		   len,
		   loop_start,
		   loop_end,
		   sample->volume,
		   pname);

	  total_mem += len;

	  patch = (struct patch_info *) malloc (sizeof (*patch) + len);

	  patch->key = GUS_PATCH;
	  patch->device_no = gus_dev;
	  patch->instr_no = next_wave_prog;
	  patch->mode = loop_flags;
	  patch->len = len;
	  patch->loop_start = loop_start;
	  patch->loop_end = loop_end;
	  patch->base_note = 261630;	/* Middle C */
	  patch->base_freq = 8448;
	  patch->low_note = 0;
	  patch->high_note = 20000000;
	  patch->volume = 120;
	  patch->panning = 0;

	  if (sample_ptr + len > Mf_file_size)
	    {
	      perror ("short mod file");
	      free (patch);
	      return 0;
	    }
	  file_patch_data = Mf_file_contents + sample_ptr;
	  sample_ptr += len;

	  memcpy(patch->data, file_patch_data, len);

	  if (write(seq_fd, (char *) patch, sizeof(*patch) + len) == -1) {
		fprintf(stderr, "mod sample not loaded\n");
	        free (patch);
		continue;
	  }

	  sample_ok[i] = 1;
	  if (sample->volume == 0) sample->volume = 64;
	  sample_vol[i] = sample->volume;

          gus_voice[i].loaded = 1;
          gus_voice[i].vname = strcpy(malloc(strlen(pname)+1), pname);
          gus_voice[i].volume = 64;
	  gus_voice[i].modes = patch->mode;
	  gus_voice[i].fix_dur = 0;
	  gus_voice[i].trnsps = 64;
	  gus_voice[i].chorus_spread = 8;
	  gus_voice[i].echo_delay = 64+16;
	  gus_voice[i].echo_atten = 64-16;
	  gus_voice[i].fix_key = 0;

	  gus_voice[i].vibrato_sweep = 100;
	  gus_voice[i].vibrato_rate = 0;
	  gus_voice[i].vibrato_depth = 14;

          gus_voice[i].prog = next_wave_prog++;

	  free (patch);
	}
    }


  nr_patterns = npat;
  songlength = slen;
  nr_channels = 4;

  for (position = 0; position < npat; position++)
    {
      /*unsigned char   patterns[64][4][4];*/
      unsigned char   *patterns;
      int             pat, channel;

      int             pp = pattern_loc + (position * 1024);


      if (pp + 1024 > Mf_file_size)
	{
	  fprintf (stderr, "Short file (pattern %d) %d\n", tune[position], pp);
	  return 0;
	}

      patterns = Mf_file_contents + pp;

      if ((pattern_table[position] = (pattern *) malloc (sizeof (struct note_info) * 64 * nr_channels)) == NULL)
	{
	  fprintf (stderr, "Can't allocate memory for a pattern\n");
	  return 0;
	}

      for (pat = 0; pat < 64; pat++)
	{
	  for (channel = 0; channel < 4; channel++)
	    {
	      unsigned short  tmp;
	      unsigned char  *p;

	      unsigned        period, sample, effect, params, note, vol;

	      /*p = &patterns[pat][channel][0];*/
	      p = patterns + (pat*16) + (channel*4);

	      tmp = (p[0] << 8) | p[1];
	      sample = (tmp >> 8) & 0x10;
	      period =
		_MIN (tmp & 0xFFF, 1023);
	      tmp = (p[2] << 8) | p[3];
	      sample |= tmp >> 12;
	      effect = (tmp >> 8) & 0xF;
	      params = tmp & 0xFF;

	      note = 0;

	      if (period)
		{
		  /*
		   * Convert period to a Midi note number
		   */

		  for (note = 0; note < 37 && period != period_table[note]; note++);
		  if (note >= 37)
		    note = 0;

		  note += 48;
		}

/*fprintf(stderr,"got note %d, period %d\n", note, period);*/
	      vol = 64;

	      if (sample)
		if (effect == 0xc)
		  {
		    vol = params;
		  }
		else
		  vol = 42 /*sample_vol[sample - 1]*/;

	      vol *= 2;
	      if (vol>64)vol--;

	      (*pattern_table[position])[channel][pat].note = note;
	      (*pattern_table[position])[channel][pat].sample = sample;
	      (*pattern_table[position])[channel][pat].command = effect;
	      (*pattern_table[position])[channel][pat].parm1 = params;
	      (*pattern_table[position])[channel][pat].parm2 = 0;
	      (*pattern_table[position])[channel][pat].vol = vol;
/*fprintf(stderr,"chan %d: note %d, sample %d, effect %d, params %d, period %d, vol%d\n",
 channel, note, sample, effect, params, period, vol);*/
	    }
	}
    }

  return 1;

}


int
panning (int ch)
{
  static int      panning_tab[] =
  {-110, 110, 110, -110};

  return panning_tab[ch % 4];
}

void
set_speed (int parm)
{
  if (!parm)
    parm = 1;

  if (parm < 32)
    {
      ticks_per_division = parm;
    }
  else
    {
      tick_duration = (60.0 / parm) * 10.0;
    }

}


void
sync_time ()
{
  if (next_time > this_time)
    {
      /*SEQ_WAIT_TIME ((long) next_time);*/
      this_time = next_time;
      Mf_realtime = (unsigned long)(0.5 + next_time * 16);
    }
}


void
set_volslide (int channel, struct note_info *pat)
{
  int             n;

  voices[channel].volslide = 0;

  if ((n = (pat->parm1 & 0xf0) >> 4))
    voices[channel].volslide = n;
  else
    voices[channel].volslide = pat->parm1 & 0xf;
}

void
set_slideto (int channel, struct note_info *pat)
{
  int             size, rate, dir, range = 200;

  rate = pat->parm1;
  size = voices[channel].note - pat->note;
  if (!size)
    return;

  if (size < 0)
    {
      size *= -1;
      dir = -1;
    }
  else
    dir = 1;

  if (size > 2)
    {
      range = size * 100;
      rate = rate * size / 200;
    }

  rate = pat->parm1 * dir / 30;
  if (!rate)
    rate = 1;

  voices[channel].slide_pitch = 1;
  voices[channel].slide_goal = (dir * 8192 * 200 * 2 / size) / range;
  voices[channel].pitchbender = 0;
  voices[channel].slide_rate = rate;
  /*SEQ_BENDER_RANGE (gus_dev, channel, range);*/
}

int
play_note (int channel, struct note_info *pat)
{
  int             jump = -1;
  int             sample, mchan;

  if (pat->sample == 0x3f)
    pat->sample = 0;

  if (pat->command == CMD_NONOTE)
    return -1;			/* Undefined */

  sample = pat->sample;

  if (sample && !pat->note)
    {
      pat->note = voices[channel].note;
    }

  if (sample)
    voices[channel].sample = sample;
  else
    sample = voices[channel].sample;

  sample--;

  sync_time();
/**
if (pat->note) fprintf(stderr,"chan %d, samp %d, note %d, vol %d, at %d = %d\n",
channel, sample, pat->note, pat->vol, (long)next_time, Mf_realtime/16);
**/

/* moved next up --gl */
      if (sample < 0)
	sample = voices[channel].sample - 1;

      if (!sample_ok[sample])
	sample = voices[channel].sample - 1;

      if (sample < 0)
	sample = 0;

      mchan = sample % 16;

  if (pat->note && pat->command != 3)	/* Have a note -> play */
    {
/**
      if (sample < 0)
	sample = voices[channel].sample - 1;

      if (!sample_ok[sample])
	sample = voices[channel].sample - 1;

      if (sample < 0)
	sample = 0;

      mchan = sample % 16;
**/
      if (sample_ok[sample])
	{
	  sync_time ();

          if (pat->vol > 127) pat->vol=127;

/**
	  if (curr_note[mchan]) {
	    (*Mf_noteon) (curr_program[mchan] % 16, curr_note[mchan], 0);
	    curr_note[mchan] = 0;
	  }
**/
	  if (voices[channel].note) {
	    (*Mf_noteon) (curr_sample[channel] % 16, voices[channel].note, 0);
	    curr_note[curr_sample[channel] % 16] = 0;
	  }
	  voices[channel].note = pat->note;

	  /*SEQ_SET_PATCH (gus_dev, channel, sample);*/
	  if (curr_program[mchan] != sample)
	    {
	      /*(*Mf_program) (mchan, (gus_dev >= 0)? sample : ((8*sample)%128));*/
	      (*Mf_program) (mchan, sample);
	        curr_program[mchan] = sample;
/*fprintf(stderr,"chan %d = sample %d\n", channel+1, sample);*/
	    }
	  curr_sample[channel] = sample;

	  /*SEQ_PANNING (gus_dev, channel, panning (channel));*/
	  /*SEQ_PITCHBEND (gus_dev, channel, 0);*/
	  (*Mf_pitchbend) (mchan, 0, 0);
	  /*SEQ_START_NOTE (gus_dev, channel, pat->note, pat->vol);*/

	  if (pat->vol && pat->note) {
	    (*Mf_noteon) (mchan, pat->note, pat->vol);
	    curr_note[mchan] = pat->note;
	  }
/*fprintf(stderr,"note %d, vol %d, s %d\n", pat->note, pat->vol, sample);*/
	  voices[channel].volume = pat->vol;
	  voices[channel].note = pat->note;
	  voices[channel].slide_pitch = 0;
	}
      else
	/*SEQ_STOP_NOTE (gus_dev, channel, pat->note, pat->vol);*/
	(*Mf_noteon) (mchan, pat->note, 0);
    }

  switch (pat->command)
    {

    case CMD_NOP:;
      break;

    case CMD_JUMP:
      jump = pat->parm1;
      break;

    case CMD_BREAK:
      jump = -2;
      break;

    case CMD_SPEED:
      set_speed (pat->parm1);
      break;

    case CMD_SLIDEUP:
      voices[channel].slide_pitch = 1;
      voices[channel].slide_goal = 8191;
      voices[channel].pitchbender = 0;
      voices[channel].slide_rate = pat->parm1 * SLIDE_SIZE;
      /*SEQ_BENDER_RANGE (gus_dev, channel, 200);*/
      break;

    case CMD_SLIDEDOWN:
      voices[channel].slide_pitch = 1;
      voices[channel].slide_goal = -8192;
      voices[channel].pitchbender = 0;
      voices[channel].slide_rate = -pat->parm1 * SLIDE_SIZE;
      /*SEQ_BENDER_RANGE (gus_dev, channel, 200);*/
      break;

    case CMD_SLIDETO:
      set_slideto (channel, pat);
      break;

    case CMD_VOLUME:
      {
        int vol = pat->parm1*2;
        if (vol>127) vol=127;
      if (pat->note && pat->command != 3)
	break;
      /*SEQ_START_NOTE (gus_dev, channel, 255, vol);*/
	/*(*Mf_parameter) (channel, EXPRESSION, vol);*/
	/*if (voices[channel].sample)
	  (*Mf_parameter) ((voices[channel].sample-1) % 16, EXPRESSION, vol);*/
	if (curr_expression[mchan] != vol) {
	  (*Mf_parameter) (mchan, EXPRESSION, vol);
	  curr_expression[mchan] = vol;
	}
      }
      break;

    case CMD_ARPEG:
if ((pat->parm1) && really_verbose)
	printf("arpeggio %d %d\n", pat->parm1 >> 4, (pat->parm1) & 0x0f);
      curr_arpeggio[channel] = pat->parm1;
      break;

    case 4:
/*if (really_verbose) printf("vibrato %d %d\n", pat->parm1, pat->parm2);*/
	  (*Mf_parameter) (mchan, MODWHEEL, pat->parm1);
      break;

    case 5:
if (really_verbose) printf("cont. slide %d %d\n", pat->parm1, pat->parm2);
      break;

    case 6:
if (really_verbose) printf("cont. vibrato %d %d\n", pat->parm1, pat->parm2);
      break;

    case 7:
if (really_verbose) printf("tremulo %d %d\n", pat->parm1, pat->parm2);
      break;

    case 9:
if (really_verbose) printf("set samp. offset %d %d\n", pat->parm1, pat->parm2);
      break;

    case 0x0e:
if (really_verbose) printf ("Cmd 0xE%02x\n", pat->parm1);
      break;

    case CMD_VOLSLIDE:
      set_slideto (channel, pat);
      break;

    /* default: */
      /* printf ("Command %x %02x\n", pat->command, pat->parm1);	*/
    }

  return jump;
}

static void
lets_play_arpeggio (int channel, int tick)
{
	int a_notes = curr_arpeggio[channel];
	int o_note = voices[channel].note;
	int vol = voices[channel].volume;
	int note1, note2, mchan, div1 = 0, div2 = 0;

	if (!a_notes || !o_note || !vol) return;

	note1 = (a_notes >> 4) & 0x0f;
	note2 = a_notes & 0x0f;
	mchan = curr_sample[channel] % 16;

	if (note1 && !note2) div1 = ticks_per_division/3;
	else if (!note1 && note2) {
		div2 = ticks_per_division/2;
	}
	else {
		div1 = ticks_per_division/2;
		div2 = 2 * div1;
	}

	if (tick == div1 && note1) {
	    sync_time();
	    (*Mf_noteon) (mchan, o_note, 0);
	    (*Mf_noteon) (mchan, o_note + note1, vol);
	}
	else if (tick == div2 && note2) {
	    sync_time();
	    if (note1) (*Mf_noteon) (mchan, o_note + note1, 0);
	    else (*Mf_noteon) (mchan, o_note, 0);
	    (*Mf_noteon) (mchan, o_note + note2, vol);
	}
	else if (tick == ticks_per_division - 1) {
	    sync_time();
	    if (note1 && !note2) (*Mf_noteon) (mchan, o_note + note1, 0);
	    if (note2) (*Mf_noteon) (mchan, o_note + note2, 0);
	    (*Mf_noteon) (mchan, o_note, vol);
	    curr_arpeggio[channel] = 0;
	}
}

void
lets_play_voice (int channel, struct voice_info *v)
{
  if (v->slide_pitch)
    {
      v->pitchbender += v->slide_rate;
      if (v->slide_goal < 0)
	{
	  if (v->pitchbender <= v->slide_goal)
	    {
	      v->pitchbender = v->slide_goal;
	      v->slide_pitch = 0;	/* Stop */
	    }
	}
      else
	{
	  if (v->pitchbender >= v->slide_goal)
	    {
	      v->pitchbender = v->slide_goal;
	      v->slide_pitch = 0;	/* Stop */
	    }
	}

      sync_time ();
      /*SEQ_PITCHBEND (gus_dev, channel, v->pitchbender);*/
      (*Mf_pitchbend) ((voices[channel].sample-1) % 16,
	(v->pitchbender >> 7), (v->pitchbender & 0x7f));
    }

  if (v->volslide)
    {
      v->volume += v->volslide;
      sync_time ();

      if (v->volume > 127) v->volume = 127;
      /*SEQ_START_NOTE (gus_dev, channel, 255, v->volume);*/
	if (voices[channel].sample)
	  (*Mf_parameter) ((voices[channel].sample-1) % 16, EXPRESSION, v->volume);
    }
}

void
play_module ()
{
  int             i, position, jump_to_pos;

  init_voices ();

  percsel = 0;

#if 0
  for (i=0;i<32;i++)
  {
  	/*SEQ_EXPRESSION(gus_dev, i, 127);*/
  	/*SEQ_MAIN_VOLUME(gus_dev, i, 100);*/
  }
#endif
  next_time = 0.0;

  Mf_currtime = Mf_realtime = 0;

  (*Mf_trackstart) ();


  set_speed (6);

  for (position = 0; position < songlength; position++)
    {
      int             tick, pattern, channel, pos, go_to;

      pos = tune[position];
      if (pattern_tempo[position])
	set_speed (pattern_tempo[position]);

      jump_to_pos = -1;
      for (pattern = 0; pattern < pattern_len[position] && jump_to_pos == -1; pattern++)
	{
	  this_time = 0.0;

	  for (channel = 0; channel < nr_channels; channel++)
	    {
	      if ((go_to = play_note (channel, &(*pattern_table[pos])[channel][pattern])) != -1)
		  jump_to_pos = go_to;
	    }

	  next_time += tick_duration;

	  for (tick = 1; tick < ticks_per_division; tick++)
	    {
	      for (channel = 0; channel < nr_channels; channel++) {
		lets_play_voice (channel, &voices[channel]);
		lets_play_arpeggio (channel, tick);
	      }
	      next_time += tick_duration;
	    }

	}

      /*if (jump_to_pos >= 0 && (Mf_realtime < 400000 || jump_to_pos > position))*/
      if (verbose && jump_to_pos >= 0 && Mf_realtime >= 500000)
	printf("can't jump to %d at %d\n", jump_to_pos, Mf_realtime);

      if (jump_to_pos >= 0 && Mf_realtime < 500000)
	position = jump_to_pos;
    }

  /*SEQ_WAIT_TIME ((int) next_time + 200);*/	/* Wait extra 2 secs */

#if 0
  for (i = 0; i < nr_channels; i++)
    SEQ_STOP_NOTE (gus_dev, i, 0, 127);
  SEQ_DUMPBUF ();
#endif
  next_time += 100;
  sync_time();
  for (i = 0; i < nr_channels; i++)
    if (voices[i].note) {
      (*Mf_noteon) (curr_sample[i] % 16, voices[i].note, 0);
      curr_note[ curr_sample[i] % 16 ] = 0;
    }

  for (i = 0; i < NUM_CHANS; i++)
	  if (curr_note[i]) {
	    (*Mf_noteon) (curr_program[i] % 16, curr_note[i], 0);
		fprintf(stderr, "yet another note off\n");
	    curr_note[i] = 0;
	  }

  for (i = 0; i < nr_patterns; i++)
    free (pattern_table[i]);
}



static void do_module()
{
  int j;

  /* play only on gus, if available */
  if (gus_dev >= 0) sb_dev = ext_dev = ext_index = -1;

  for (j = 0; j < MAX_PATTERN; j++)
    pattern_table[j] = NULL;

  if (load_module ())
    {
	tick_duration = 100.0 / clock_rate;
	play_module ();
    }
  else mferror("unknown file format");
}
