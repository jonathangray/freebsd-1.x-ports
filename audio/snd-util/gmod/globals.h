

/* This file is part of the GMOD package */

extern int pattern_len[MAX_POSITION];
extern int pattern_tempo[MAX_POSITION];
extern pattern *pattern_table[MAX_PATTERN];

extern struct voice_info voices[MAX_TRACK];

extern int tune[MAX_POSITION];
extern double tick_duration;

extern int seqfd;
extern int sample_ok[128], sample_vol[128];
extern int tmp, gus_dev;
extern double this_time, next_time;
extern int ticks_per_division;
extern double clock_rate;	/* HZ */

extern unsigned char _seqbuf[];
extern int _seqbuflen, _seqbufptr;

extern unsigned short base_freq_table[];
extern unsigned short period_table[];
extern short vibra_table[][NUM_VIBRA];
