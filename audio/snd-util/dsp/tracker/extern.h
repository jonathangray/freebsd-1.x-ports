/* extern.h 
	vi:se ts=3 sw=3:
 */

/* $Id: extern.h,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: extern.h,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.1  1994/02/04  14:54:08  espie
 * Fixed up ansi C stupid bug.
 *
 * Revision 4.0  1994/01/11  17:46:25  espie
 * Lots of new proto for all functions.
 *
 * Revision 1.10  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.9  1994/01/09  04:50:56  Espie
 * Comments.
 *
 * Revision 1.8  1994/01/08  19:43:57  Espie
 * Added checkbrk.
 *
 * Revision 1.7  1994/01/08  03:55:43  Espie
 * General cleanup.
 *
 * Revision 1.6  1994/01/06  22:32:42  Espie
 * Suppressed some unused code.
 *
 * Revision 1.5  1994/01/05  13:53:25  Espie
 * Better portability.
 *
 * Revision 1.4  1993/12/28  13:54:44  Espie
 * Removed init_display.
 * No more reentrency problems with INIT_ONCE.
 * Protos for ui: notice, info, scroller, pattern display.
 *
 * Revision 1.3  1993/12/26  22:48:18  Espie
 * Mostly working.
 * Just dies with a guru.
 * Plus timing problems at start.
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * New prototypes.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.15  1993/12/04  16:12:50  espie
 * Added prototypes.
 *
 * Revision 3.14  1993/12/02  15:45:33  espie
 * Added some protos.
 *
 * Revision 3.13  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.12  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.10  1993/07/18  10:39:44  espie
 * Added pid, fork, show.
 *
 * Revision 3.8  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.7  1992/12/03  15:00:50  espie
 * stty_sane.
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * New audio functions.
 *
 * Revision 3.2  1992/11/22  17:20:01  espie
 * Simplified delay_pattern.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.19  1992/11/17  17:06:25  espie
 * Lots of new functions to account for new interface.
 * open_file support.
 * Separated mix/stereo stuff.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Added some new song types to automatize the choice process.
 * Moved resampling to audio, added prototype.
 * Added SAMPLE_FAULT, for trying to play
 * a note without a sample (not really an error).
 *
 * Revision 1.7  1991/11/08  14:25:55  espie
 * Modified audio prototype so that you can change
 * frequency.
 * Added prototype for release_song.
 * Added arpeggio effect.
 * Added entries for new effects.
 * Added entries for commands.c.
 */



/* audio.c */
#define ACCURACY 12
#define fix_to_int(x) ((x) >> ACCURACY)
#define int_to_fix(x) ((x) << ACCURACY)


/* release_audio_channels:
 * free every audio channel previously allocated
 */
XT void release_audio_channels P((void));

/* chan = new_channel_tag_list(prop):
 * allocates a new channel for the current song
 * No properties are currently defined.
 */
XT struct audio_channel *new_channel_tag_list P((struct tag *prop));

/* init_tables(oversample, frequency):
 * precomputes the step_table and the pitch_table
 * according to the desired oversample and frequency.
 * This is static, you can call it again whenever you want.
 * Adjust the currently used audio channels if needed.
 */
XT void init_tables P((int oversample, int frequency));

/* resample(oversample, number):
 * send number samples out computed according
 * to the current state of channels
 * and oversample.
 */
XT void resample P((int oversample, int number));

/* play_note(au, samp, pitch)
 * set audio channel au to play samp at pitch
 */
XT void play_note P((struct audio_channel *au, struct sample_info *samp, \
int pitch));

/* set_play_pitch(au, pitch):
 * set channel au to play at pitch pitch
 */
XT void set_play_pitch P((struct audio_channel *au, int pitch));

/* set_play_volume(au, volume):
 * set channel au to play at volume volume
 */
XT void set_play_volume P((struct audio_channel *au, int volume));

/* set_play_position(au, pos):
 * set position in sample for channel au at given offset
 */
XT void set_play_position P((struct audio_channel *au, int pos));


/* automaton.c */
/* init_automaton(a, song, start):
 * put the automaton a in the right state to play song from pattern start.
 */
XT void init_automaton P((struct automaton *a, struct song *song, int start));

/* next_tick(a):
 * set up everything for the next tick.
 */
XT void next_tick P((struct automaton *a));


/* commands.c */
/* init_effects(): sets up all data for the effects */
/* (not set up as auto_init due to huge overhead) */
XT void init_effects P((void (*table[])()));

/* do_nothing: this is the default behavior for an effect.
 */
XT void do_nothing P((struct channel *ch));


/* dump_song.c */
/* dump_song(s): 
 * displays some information pertinent to the given 
 * song s.
 */
XT void dump_song P((struct song *song));


/* display.c */

/* dump_event(ch, e): dump event e as occuring on channel ch
 */
XT void dump_event P((struct channel *ch, struct event *e));


/* main.c */

#define OLD 0
#define NEW 1
/* special new type: for when we try to read it as both types.
 */
#define BOTH 2
/* special type: does not check the signature */
#define NEW_NO_CHECK 3


/* error types. Everything is centralized,
 * and we check in some places (see st_read, player and main)
 * that there was no error. Additionnally signal traps work
 * that way too.
 */
 
/* normal state */
#define NONE 0  
/* read error */
#define FILE_TOO_SHORT 1
#define CORRUPT_FILE 2
/* trap error: goto next song right now */
#define NEXT_SONG 3
/* run time problem */
#define FAULT 4
/* the song has ended */
#define ENDED 5
/* unrecoverable problem: typically, trying to 
 * jump to nowhere land.
 */
#define UNRECOVERABLE 6
/* Missing sample. Very common error, not too serious. */
#define SAMPLE_FAULT 7
/* New */
#define PREVIOUS_SONG 8
#define OUT_OF_MEM 9
XT int error;

/* notes.c */
#define NUMBER_NOTES 120
#define NUMBER_FINETUNES 17
XT short pitch_table[NUMBER_NOTES][NUMBER_FINETUNES];    /* 120 * 17 = big ! */

/* note = find_note(pitch):
 * find note corresponding to a given pitch. 
 */
XT int find_note P((int pitch));

/* oldtranspose = transpose_song(song, newtranspose):
 * tranpose song to a new pitch
 */
XT int transpose_song P((struct song *song, int newtranspose));

/* name = name_of_note(note):
 * name of the note. Warning! This name is only valid
 * until a new call to name_of_note.
 */
XT char *name_of_note P((int note));





/* open.c */
/* handle = open_file(filename, mode, path):
 * transparently open a compressed file.
 */
XT struct exfile *open_file P((char *fname, char *fmode, char *path));

/* handle = file_handle(f):
 * obtain the actual FILE handle from the private structure
 */
XT FILE *file_handle P((struct exfile *f));

/* close_file(handle):
 * close a file that was opened with open_file.
 */
XT void close_file P((struct exfile *file));

XT int getc_file P((struct exfile *file));
XT int tell_file P((struct exfile *file));




/* player.c */

/* reset_note(ch, note, pitch):
 * set channel ch to play note at pitch pitch
 */
XT void reset_note P((struct channel *ch, int note, int pitch));

/* set_current_pitch(ch, pitch):
 * set ch to play at pitch pitch
 */
XT void set_current_pitch P((struct channel *ch, int pitch));

/* set_current_volume(ch, volume):
 * set channel ch to play at volume volume
 */
XT void set_current_volume P((struct channel *ch, int volume));

/* set_position(ch, pos):
 * set position in sample for current channel at given offset
 */
XT void set_position P((struct channel *ch, int pos));

/* init_player(oversample, frequency):
 * sets up the player for a given oversample and
 * output frequency.
 * Note: we can call init_player again to change oversample and
 * frequency.
 */
XT void init_player P((int o, int f));

/* play_song(song, start):
 * play the song.
 */
XT struct tag *play_song P((struct song *song, int start));
/* returns tags as shown further down in get_ui */



/* st_read.c */
/* s = read_song(f, type):
 * tries to read f as a song of type NEW/OLD.
 * returns NULL (and an error) if it doesn't work.
 * Returns a dynamic song structure if successful.
 */
XT struct song *read_song P((struct exfile *f, int type));

/* release_song(s):
 * release all the memory song occupies.
 */
XT void release_song P((struct song *song));



/* setup_audio.c */
/* setup_audio(ask_freq, stereo, oversample):
 * setup the audio output with these values 
 */
XT void setup_audio P((int f, boolean s, int o));
/* do_close_audio():
 * close audio only if needed
 */
XT void do_close_audio P((void));







/* xxx_audio.c */
/* frequency = open_audio(f, s):
 * try to open audio with a sampling rate of f, and eventually stereo.
 * We get the real frequency back. If we ask for 0, we
 * get the ``preferred'' frequency.
 * Note: we have to close_audio() before we can open_audio() again.
 * Note: even if we don't ask for stereo, we still have to give a
 * right and left sample.
 */
XT int open_audio P((int f, int s));
/* close_audio():
 * returns the audio to the system control, doing necessary
 * cleanup
 */
XT void close_audio P((void));
/* set_mix(percent): set mix channels level.
 * 0: spatial stereo. 100: mono.
 */
XT void set_mix P((int percent));

/* output_samples(l, r): outputs a pair of stereo samples.
 * Samples are 15 bits signed.
 */
XT void output_samples P((int left, int right));

/* flush_buffer(): call from time to time, because buffering
 * is done by the program to get better (?) performance.
 */
XT void flush_buffer P((void));

/* discard_buffer(): try to get rid of the buffer contents
 */
XT void discard_buffer P((void));

/* new_freq = update_frequency():
 * if !0, frequency changed and playing should be updated accordingly
 */
XT int update_frequency P((void));

/* set_synchro(boolean):
 * try to synchronize audio output by using a smaller buffer
 */
XT void set_synchro P((boolean s));

#ifdef SPECIAL_SAMPLE_MEMORY
XT GENERIC alloc_sample P((int len));
XT void free_sample P((GENERIC s));
XT int obtain_sample P((GENERIC start, int l, FILE *f));

#else
#define alloc_sample(len)		calloc(len, 1)
#define free_sample(sample)		free(sample)
#define obtain_sample(start, l, f)	fread(start, 1, l, f)
#endif



/* tools.c */
/* v = read_env(name, default):
 * read a scalar value in the environment
 */
XT int read_env P((char *name, int def));




/* autoinit.c */
/* used for decentralizing initialization/termination of various
 * system routines
 */

/* end_all(s): the program must exit now, after displaying s to the user, usually 
 * through notice and calling all stacked at_end() functions. s may be 0 for normal
 * exit. DO NOT use exit() anywhere in tracker but end_all() instead.
 */
XT void end_all P((char *s));

/* at_end(cleanup): stack cleanup to be called at program's termination
 */
XT void at_end P((void (*cleanup)(void)));

/* INIT_ONCE: macro for autoinitialization of routines.
 * modules that need an init routine should LOCAL void INIT = init_routine,
 * and add INIT_ONCE; at EVERY possible first entry point for their routine.
 * (I mean every, don't try to second-guess me !)
 */
#define INIT_ONCE		if (INIT)	{void (*func)P((void)) = INIT; INIT = 0; (*func)();}


/* $(UI)/ui.c */
/* see unix/ui.c for the general unix implementation.
 * The old may_getchar() has been replaced by the tag-based
 * get_ui
 */
/* get_ui(): returns an array of tags that reflect the current user-interface
 * actions. Unknown tags WILL be ignored.
 * Note that get_ui will be called about once every tick, providing a poor man's
 * timer to the interface writer if needed to code multiple actions on the same
 * user-input. See unix/termio.c for a good example.
 * see amiga/ui.c for the correct way to do it when you have a real timer.
 *
 * VERY IMPORTANT: who do the tags belong to ?
 *    as a general rule, result (and their values) MUST only be considered
 *    valid between two calls to get_ui ! Be careful to call get_ui ONLY at
 *    reasonable places.
 *    One exception: structures that are dynamically allocated (like UI_LOAD_SONG
 *    values) will ONLY get freed when you ask for it !
 */
XT struct tag *get_ui P((void));
#define BASE_UI 10
#define UI_NEXT_SONG	(BASE_UI)            /* load next song */
#define UI_PREVIOUS_SONG (BASE_UI + 1)    /* load previous song */
#define UI_LOAD_SONG (BASE_UI + 2)        /* load song. Name as value */
#define UI_SET_BPM (BASE_UI + 3)          /* set beat per minute to value */
#define UI_JUMP_TO_PATTERN (BASE_UI + 4)  /* jump to pattern #value. Use display_pattern to
                                           * keep in sync with the player
                                           */
#define UI_RESTART (BASE_UI + 5)          /* restart current song. Not quite jump to 0 */
#define UI_QUIT (BASE_UI + 6)             /* need I say more ? */
#define UI_DISPLAY (BASE_UI + 7)          /* status of scrolling window: true or false */


/* player.c translates the get_ui() tags in a standard way.
 * Actually it doesn't translate anything right now...
 */
#define BASE_PLAY 20
#define PLAY_NEXT_SONG UI_NEXT_SONG
#define PLAY_PREVIOUS_SONG UI_PREVIOUS_SONG
#define PLAY_LOAD_SONG UI_LOAD_SONG

#define PLAY_ERROR BASE_PLAY

/* Most of these functions are information display function.
 * A correct implementation should heed run_in_fg() if needed
 */

/* notice(s): important message for the user (terminal error maybe).
 * take extra pain to make it apparent even if run in background
 */
XT void notice P((char *s));

/* status(s): some indication of the system current status... 
 * Used for fleeing error messages too. 
 * s = 0 is valid and indicates return to the default status.
 */
XT void status P((char *s));

/* begin_info: open a logical information window.
 * returns 0 if the window couldn't be opened.
 * A NULL window shouldn't be used, but don't count on it !
 */
XT GENERIC begin_info P((char *title));
/* info(handle, line): add a line to the info window,
 * completing the current line if applicable
 */
XT void info P((GENERIC handle, char *line));
/* infos(handle, line): add to the current line of the info window
 */
XT void infos P((GENERIC handle, char *s));
/* end_info(handle): this window is complete...
 */
XT void end_info P((GENERIC handle));

/* Scrolling score display:
 * new_scroll() returns a writable buffer of 4*14-1 characters in
 * which the program (usually display.c) will write what it wills.
 * It can return 0 if not applicable.
 */
XT char *new_scroll P((void));

/* scroll: returns this scrolling line to the program. Note that
 * scroll doesn't take any argument, and implies calls to new_scroll/scroll
 * are paired. After a call to scroll, the last pointer returned by new_scroll
 * should be considered invalid !
 */
XT void scroll P((void));

/* display_pattern(current, total): we are at current/total in the current song
 * may be used as a poor man's timer.
 */
XT void display_pattern P((int current, int total));

/* song_title(s): the current song title is s.
 * ui implementors: Don't count on this pointer remaining valid AFTER the call,
 * make a copy if needed
 */
XT void song_title P((char *s));

/* boolean checkbrk():
 * check whether a break occured and we should end right now.
 * Call it often enough (like when loading songs and stuff)
 */
XT boolean checkbrk P((void));
