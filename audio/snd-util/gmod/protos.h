/* This file is part of the GMOD package */
#ifndef _STDIO_H
#include <stdio.h>
#endif

/* in cvt_period.c */
void period_to_note (int period, int *note, int *pitchbend);

/* in effects.c */
void set_speed (int parm);
void set_volslide (int channel, int amount);
void set_slideto (int channel, int rate, int note);
void set_arpeg (int channel, int amount);
void set_vibrato (int channel, int amount);
void set_tremolo (int channel, int amount);
void set_panning (int channel, signed char panning, unsigned char hw_flag);

/* in init.c */
void init_voices (void);

/* int load_669.c */
int load_669_module (FILE * mod_fd, char *name, struct song_info *song_char,
		     struct options_info options);

/* in load_mod.c */
int load_module (char *name, struct song_info *song_char,
		 struct options_info options);

/* in load_mtm.c */
int load_mtm_module (FILE * mod_fd, char *name, struct song_info *song_char,
		     struct options_info options);

/* in load_s3m.c */
int load_s3m_module (FILE * mod_fd, char *name, struct song_info *song_char,
		     struct options_info options);

/* in load_ult.c */
int load_ult_module (FILE * mod_fd, char *name, struct song_info *song_char,
		     struct options_info options);

/* in misc.c */
int panning (int ch);
void sync_time (void);
unsigned short intelize (unsigned short v);
int gus_mem_free (int);
unsigned char vol_log_to_lin (unsigned char volume);

/* in parse.c */
int parse_args (int argc, char *argv[], struct options_info *options);

/* in play_mod.c */
void play_module (char *name, struct song_info *song_char,
		  struct options_info options);

/* in play_note.c */
int play_note (int channel, int position, int pattern, struct note_info *pat,
	       struct song_info *song_char, struct effect_info *effects);

/* in play_voice.c */
void lets_play_voice (int channel, struct voice_info *v,
		      struct song_info *song_char);
