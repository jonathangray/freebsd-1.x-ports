/*
 * admp.c
 * common stuff for ad and mp
 * This file is included in phase1.c (for ad) and in phasem.c (for mp).
 */

/* in gusvoice.c, called by enter_prog() */
int find_tone_bank(int dev, int pgm, int bank);

#define MAXSPACE 500000
static long space = MAXSPACE;	/* needed to keep track of space malloc'd
				 * in msdos version, for some reason -- not
				 * kept updated now for linux */

static int pitch_flag;		/* set when a pitch is indicated */
	/* (if controls changes are given, only allocate a note event if
	 *  a pitch was specified -- i.e. when pitch_flag is set)
	 */
static int rest_flag;		/* set when a rest (R) is found */
	/* this flag is NOT inherited by the next line */

static int symbolic_dur_flag;
	/* true if last dur was not absolute
	 * (if this is set, then the default duration is changed
	 *  accordingly when the tempo is changed.)
	 */

static int ctrlflag[nctrl];
	/* true if control change was present
	 * ctrlflag[0] true if ANY control change
	 * was present
	 */
static int ctrlval[nctrl];
	/* the new value of the control */

static int new_prog = -1;
static int last_prog[NUM_CHANS];/*saved value of program from previous line */
	/* (this is needed to implement the rule that note
	 *  events are generated for rests if the program has changed.)
	 */
/****************************************************************************
*				state variables
* Because each line of an Adagio score inherits properties from the previous
* line, it makes sense to implement the parser as a collection of routines
* that make small changes to some global state.	 For example, pitch is a
* global variable.  When the field G4 is encountered, the dopitch routine
* assigns the pitch number for G4 to the variable pitch.  After all fields
* are processed, these variables describe the current note and contain the
* default parameters for the next note as well.
*
* Global variables that are used in this way by the parsing rountines are:
****************************************************************************/
static int
#ifndef I_AM_MP
 linex,				/* index of the next character to be scanned */
 fieldx,			/* index of the current character within a field */
#endif
 lineno,			/* current line number */
 pitch,				/* pitch of note */
 loud,				/* loudness of note */
 dur,				/* the duration of the note */
 voice;				/* voice (midi channel) of note */

static int ndurp;		/* set when a next (N) is indicated */
 /* (next time defaults to the current time plus duration unless
	 *  overridden by a next (N) command whose presence is signalled
	 *  by ndurp.)
	 */

static unsigned long
 thetime,			/* the starting time of the note */
 thetrack,			/* the midi track of the note */
 thecontrol,			/* the controller */
 theeffect,			/* special effect */
 rate,				/* time rate -- scales time and duration, default = 100 */
 ntime,				/* the starting time of the next note */
 tempo,				/* the current tempo */
 start;				/* the reference time (time of last !tempo or !rate cmd) */

extern char score_na[];

static int note_count = 0;	/* the number of notes translated */
static int ctrl_count = 0;	/* ditto for control commands */

void var_init()
{   int i;

    for (i = 0; i < NUM_CHANS; i++) {
	last_prog[i] = program[i] = ext_program[i] = NO_VOI;
	ext_chan[i] = ext_poly[i] = /*chorus_depth[i] =*/ 0;
	curr_note_count[i] = curr_note_velocity[i] = 0;
	/*ext_pan[i] = -1;*/
	fm_sysex[i] = NULL;
    }
    for (i = 0; i < NUM_DRUMS; i++) {
	drum_reverberation[i] = drum_chorus_depth[i] = drum_pan[i] = -1;
    }

    percsel = PSELECT;
    ext_polyphony = 0;
    note_count = 0;
    ctrl_count = 0;
    new_prog = -1;
}

/****************************************************************************
*				init
* Outputs:	Returns true if OK, false on error.
* Effect:	Initializes program state.
****************************************************************************/

static int ad_init()
{
    int i;

#ifdef I_AM_MP
    /* see if any notes were played on the percussion channels,
     */
    for (i = 0; i < NUM_CHANS; i++)
	if (program[i] < 0)
	    percsel &= ~(1 << i);
#else
    vverbose = cl_switch("-trace");
    vverbose = vverbose || cl_switch("-t");
#endif

    for (i = 0; i < nctrl; i++)
	ctrlflag[i] = false;

    dur = precise((long) 60);	/* quarter note */
    lineno = 0;
    thetime = 0;
    thetrack = 0;
    theeffect = 0;
    pitch = 48;
    loud = 127;
    voice = 1;
    for (i = 0; i < NUM_CHANS; i++)
	last_prog[i] = -1;
    tempo = 100;
    rate = 100;
    start = 0;
    symbolic_dur_flag = true;	/*scale dur by tempo*/
    return true;
}

/****************************************************************************
*				ins_note
* Inputs:
*	event_type *score: a linked list in which to insert
* Outputs:
*	returns true on success, false on error (not enough space)
* Effect:
*	note event (if any) corresponding to current line are inserted in
*	score
* Implementation:
*	if a note on should occur after a note off and doesn't, and the
*	two notes have the same pitch, then the note off can cancel the
*	note on:
*		|------------------| <- this cancels *
*			   this -> |-----------|
*	To make it unlikely that roundoff will cause this situation,
*	dur is decreased by one half of a clock tick before rounding.
*	Also, phase2 gives precedence to note-offs that are simultaneous
*	with note-ons.
****************************************************************************/


static int ins_note(score)
event_type *score;
{
    event_type nalloc(), note;
    int bank = 0, card = -1;
    int midi_program = program[voice-1] - 1;
    int reverb = reverberation[voice-1];
    int panning = ext_pan[voice-1];

    if (PERCCHAN(voice-1)) midi_program = pitch + 128;
    else if (midi_program < 0) midi_program = 0;

    if ((note = nalloc()) == NULL) {
	return false;
    }
#ifdef MAINTAIN_PRECISION
    note->ntime = thetime;
#else
    note->ntime = round(thetime);
#endif
    note->nvoice = voice - 1;
    note->nline = lineno;
    if (PERCCHAN(voice-1)) {
	if (program[voice-1] > 0) bank = (program[voice-1] - 1) << 8;
    }
#ifdef I_AM_MP
    else bank = voice_bank[voice - 1];
    note->ntrack = thetrack;
#else
    note->ntrack = 0;
#endif
    note->u.note.nbank = (bank >> 8) & 0x7f;
    note->next = NULL;
    if (rest_flag) {
	note->u.note.npitch = NO_PITCH;	/* a rest */
	note_count--;
    }
#ifdef I_AM_MP
    else
	note->u.note.npitch = pitch;
#else
    /* compensate for adagio convention that middle C is 48 instead of 60 */
    else
	note->u.note.npitch = pitch + 12;
#endif
    note->u.note.ndur = round(dur - (unity / 2));
    note->u.note.nloud = loud;
    note->u.note.neffect = theeffect;
    if (program[voice - 1] > 0) {
	int xc = ext_chan[voice-1];
	int xv;
	if (xc > 0) xv = ext_program[xc-1];
	else xv = -1;
	note->u.note.nprogram = program[voice-1];
	if (xc &&
		ext_dev >= 0 &&
		xv == program[voice-1] &&
		(rest_flag || loadvoice(ext_index, (xv-1) | (bank << 8),
			reverb, main_volume[voice-1], panning)) ) {
	    note->ndest = xc;
	    card = ext_index;
	    ext_voice_bank[voice-1] = bank;
#ifdef HELP_EXT_REVERB
	    if (reverb > REVERB_THRESHHOLD && gus_dev >= 0) {
		(void)loadvoice(gus_dev, (xv-1) | (bank << 8),
			reverb, main_volume[voice-1], panning);
	    }
#endif
	}
	else note->ndest = 0;
    } else {
	/* I don't think this can happen */
/** CAN'T DO THIS FOR PERC
	note->u.note.nprogram = 1;
	program[voice - 1] = 0;
**/
	note->ndest = 0;
    }

    if (!rest_flag && note->ndest == 0) {
#ifdef I_AM_MP
	/* Track assignment from cfg file? */
	int r_device = (redirection & 3);
	/* ... if not, channel assignment from cfg file? */
	if (!r_device) r_device = channel_direction[voice - 1] & 3;
#endif
	if (PERCCHAN(voice - 1)) {
	    if (drum_reverberation[pitch] >= 0) reverb = drum_reverberation[pitch];
	    if (drum_pan[pitch] >= 0) panning = drum_pan[pitch];
#ifdef FM_PERC_PRIORITY
	    if (sb_dev >= 0 && loadvoice(sb_dev, (pitch + 128) | (bank << 8),
			reverb, main_volume[voice-1], panning))
		card = sb_dev;
	    else if (gus_dev >= 0 && loadvoice(gus_dev, (pitch + 128) | (bank << 8),
			reverb, main_volume[voice-1], panning))
		card = gus_dev;
#else
	    if (gus_dev >= 0 &&
#ifdef I_AM_MP
		r_device != 1 &&
#ifdef WPOLYPHONY
		wave_poly[midi_program] >= 0 &&
#endif
#endif
			loadvoice(gus_dev, midi_program | (bank << 8),
			reverb, main_volume[voice-1], panning))
		card = gus_dev;
	    else if (sb_dev >= 0 && loadvoice(sb_dev, midi_program | (bank << 8),
			reverb, main_volume[voice-1], panning))
		card = sb_dev;
#endif
	    else
		card = -1;
	}
	else if (gus_dev >= 0 &&
#ifdef I_AM_MP
		r_device != 1 &&
#ifdef WPOLYPHONY
		wave_poly[midi_program] >= 0 &&
#endif
#endif
#ifdef I_AM_MP
		loadvoice(gus_dev, midi_program | (bank << 8),
#else
		loadvoice(gus_dev, midi_program,
#endif
			reverb, main_volume[voice-1], panning))
	    card = gus_dev;
	else if (sb_dev >= 0 &&
		loadvoice(sb_dev, midi_program | (bank << 8),
			reverb, main_volume[voice-1], panning))
	    card = sb_dev;
	else
	    card = -1;
	/*note->ncard = card;*/
    }
/**
    } else
	note->ncard = 0;
**/
    note->ncard = card;

    if (theeffect & CHORUS2_EFFECT) {
	if (card == ext_index) card = -1;
	else if (card == sb_dev && fm_voice[midi_program].chorus_spread == 64)
		card = -1;
	else if (card == gus_dev && gus_voice[midi_program].chorus_spread == 64)
		card = -1;
    }
    if (card == -1) {
	free(note);
	return(true);
    }
    if (vverbose)
	printf("note: track=%d time=%ld dur=%ld pitch=%d voice=%d prog=%d vel=%d\n",
	note->ntrack, note->ntime, note->u.note.ndur, note->u.note.npitch,
	       note->nvoice, note->u.note.nprogram, note->u.note.nloud);
    ins_event(score, note);
    return true;
}

/****************************************************************************
*				ins_ctrl
* Inputs:
*	event_type *score: a linked list in which to insert
* Outputs:
*	returns true on success, false on error (not enough space)
* Effect:
*	control events corresponding to current line are inserted in score
* Implementation:
*	ctrlflag[i] is true if control i was specified in this line, so
*	insert one control change for each ctrlflag[i] that is true
****************************************************************************/

static int ins_ctrl(score)
event_type *score;
{
#ifndef I_AM_MP
    int ctrl_list[] =
    {0, PORTARATE, PORTASWITCH, MODWHEEL, MIDI_TOUCH, FOOT,
     MIDI_BEND, VOLUME, PAN, EXPRESSION, 0, 0, 0, 0, 0, 0};
#endif
    int i;
    event_type ctrl;

    for (i = 1; i < nctrl; i++) {
	if (ctrlflag[i]) {
	    ctrlflag[i] = false;
	    if ((ctrl = ctrlalloc()) == NULL) {
		return false;
	    }
#ifdef MAINTAIN_PRECISION
	    ctrl->ntime = thetime;
#else
	    ctrl->ntime = round(thetime);
#endif
	    ctrl->nvoice = ctrl_voice(i, voice - 1);
	    if (program[voice - 1] > 0) {
		if (ext_chan[voice - 1] /*&& ext_program[ext_chan[voice-1]-1] == program[voice-1]*/ )
		    ctrl->ndest = ext_chan[voice - 1];
		else
		    ctrl->ndest = 0;
	    } else
		ctrl->ndest = 0;
	    if (ctrl->ndest == 0) {
		int card;
		if (gus_dev >= 0 /*&& gus_voice[program[voice-1]-1].loaded*/ )
		    card = gus_dev;
		else if (sb_dev >= 0)
		    card = sb_dev;
		else
		    card = -1;
		ctrl->ncard = card;
	    } else
		ctrl->ncard = ext_index;
	    ctrl->nline = lineno;
	    ctrl->ntrack = thetrack;
	    ctrl->next = NULL;
#ifndef I_AM_MP
	    if (i == 6)
		ctrlval[i] <<= 6;
#endif
	    ctrl->u.ctrl.value = ctrlval[i];
#ifdef I_AM_MP
	    ctrl->u.ctrl.control = thecontrol;
#else
	    ctrl->u.ctrl.control = ctrl_list[i];
#endif
/**
if (ctrl->u.ctrl.control > 127)
fprintf(stderr, "ins_ctrl: bad controller %d\n", ctrl->u.ctrl.control);
**/
	    if (vverbose)
		printf("ctrl %d (ad%d): time=%ld voice=%d line=%d val=%d\n", thecontrol, i,
		       ctrl->ntime, ctrl->nvoice, ctrl->nline, ctrl->u.ctrl.value);
	    ins_event(score, ctrl);
	    ctrl_count++;
	}
    }
    return true;
}

#define NEW_PROG_METHOD
/* Note new program -- decide which device to use for it. */
static void enter_prog(int chan, int prog, int bank)
{
    int tpgm;
#ifndef NEW_PROG_METHOD
  static
#endif
    int ext_tot = 0;

    if (piano_only) prog = 0;

    /* Note new current program on this channel, with numbering starting
     * at 1 (this has confused me a lot).
     */
    program[chan] = prog + 1;

    if (ext_dev < 0) return;

#ifdef I_AM_MP
    if (m_track >= 0 && m_track < 128 &&
	(track_direction[m_track]&3) > 0 &&
	(track_direction[m_track]&3) < 3)
	return;
    if ( (channel_direction[chan]&3) > 0 &&
	(channel_direction[chan]&3) < 3)
	return;
#endif

    /* If recording, reserve first external channel for recordist (should
     * this be done only for the K1?).
     */
#ifndef NEW_PROG_METHOD
    if (!ext_tot && recording_track)
	ext_tot++;
#else
    if (recording_track) ext_tot++;
    while (ext_program[ext_tot] > 0 && ext_tot < NUM_CHANS) {
	/*if (ext_program[ext_tot] == prog + 1) return;*/
	ext_tot++;
    }
    if (ext_tot == NUM_CHANS) return;
#endif

    tpgm = find_tone_bank(ext_index, prog, bank);

    /* Shall this program on this channel go off board? */
    if (
	!ext_chan[chan] &&	/* only if no other program on this channel is already
				 * scheduled to go off board
				 */
	fm_sysex[chan] == NULL &&	/* unless there is a System Exclusive (why? -- I've
				 * forgotten the point of this)
				 */
#ifdef K1
	!(PERCCHAN(chan)) &&	/* don't send percussion off board (this is parochial --
				 * my Kawaii K1 can't do percussion)
				 */
#endif
	ext_tot < XMAXCHAN &&	/* not when channel capacity of synth would be exceeded */
#ifdef K1
	ext_voice[tpgm].vname != NULL /* only if K1 can do this particular voice */
#else
	(XSELECT & (1 << chan))	/* only if this channel is permitted off board */
#endif
	) {
	/* Yes, it's going off board.  ext_tot is the channel it will be redirected to. */
	ext_program[ext_tot] = prog + 1;
	ext_tot++;
	ext_chan[chan] = ext_tot;
    }
}

/****************************************************************************
*				parsenote
* Inputs:
*	event_type *scoreptr: pointer to the note list
* Effect:
*	parses a note line -- control events (if any) and note event (if
*	present) are inserted into *scoreptr
* Assumes:
*	line contains a string to be parsed
****************************************************************************/

static int parsenote(scoreptr)
event_type *scoreptr;		/* the translated note list */
{
    int out_of_memory = false;

    ndurp = false;

#ifndef I_AM_MP
    rest_flag = false;

    /* this loop reads tokens for a note */
    while (strlen(token) > 0) {
	parsefield();
	linex += scan(&line[linex]);
    }

    parseend();			/* take care of note terminator */
#endif

#ifdef I_AM_MP
    if (!piano_only && new_prog >= 0)
	program[voice - 1] = new_prog;
#else
    if (!piano_only && new_prog > 0)
	enter_prog(voice - 1, new_prog - 1, 0);
#endif

/* try controls first (see note below) -- gl */
    if (ctrlflag[0]) {
	out_of_memory |= !ins_ctrl(scoreptr);
	/*ctrlflag[0] = false;*/
    }
    /* insert a note if
     *	(1) a pitch was specified OR
     *	(2) no control was specified and this is not a rest
     *		(it's a pitch by default) OR
     *	(3) there is a program change (even if this is a rest)
     *
     * NOTE: program changes during rests are advised since
     *	synthesizers may not be able to process a program
     *	change followed immediately by a note-on.  In fact, this
     *	is why we insert notes whose pitch is NO_PITCH -- so that
     *	the program change can be processed during the rest.
     */
    if (pitch_flag ||
	(!ctrlflag[0] && !rest_flag) ||
	(program[voice-1] != last_prog[voice-1])) {
	    /* Double the note to get a "layer" interpretation of chorus
	     * depth.  ins_note stows the effect information in the note
	     * structure, then, in phase2, the extra note will discarded if
	     * there is insufficient polyphony, or else the pitch with be
	     * bent in opposite directions for the two paired notes.  (The
	     * notes are kept on the same channel and have the same velocity,
	     * so unfortunately, we cannot send the extra note to an external
	     * synth -- this is just for the sound cards.)
	     */
	    int depth = chorus_depth[voice-1];
	    if (PERCCHAN(voice-1) && pitch < NUM_DRUMS &&
		drum_chorus_depth[pitch] >= 0)
		    depth = drum_chorus_depth[pitch];
	    if (setting_chorus_spread && pitch_flag &&
			/*!PERCCHAN(voice-1) &&*/ depth) {
		theeffect = CHORUS1_EFFECT;
		out_of_memory = !ins_note(scoreptr);
		theeffect = CHORUS2_EFFECT;
		out_of_memory = !ins_note(scoreptr);
		theeffect = 0;
	    }
	    else out_of_memory = !ins_note(scoreptr);
	note_count++;
    }
    /*
     * insert ctrl's last so that when the score is reversed,
     * they will be first.
     */
/* above strategy does not seem to work, so I have moved this
   to before a note is generated.  Then ctrl's really do wind
   up before notes, when times are equal -- gl */
/*
    if (ctrlflag[0]) {
	out_of_memory |= !ins_ctrl(scoreptr);
	ctrlflag[0] = false;
    }
*/

/* but still have to note that the ctrl's have now been generated -- gl */
    ctrlflag[0] = false;

    last_prog[voice - 1] = program[voice - 1];
    new_prog = -1;

    if (ndurp)
	thetime += ntime;
    else
	thetime += dur;

    return out_of_memory;
}

/****************************************************************************
*				reverse
* Inputs:
*	event_type *p: pointer to a list of notes and control events
* Effect: reverses note and control events in p
****************************************************************************/

static void reverse(p)
event_type *p;
{
    event_type p1, p2, p3;
    p1 = *p;
    if (p1 == NULL)
	return;
    p2 = p1->next;
    p1->next = NULL;
    while (p2 != NULL) {
	p3 = p2->next;
	p2->next = p1;
	p1 = p2;
	p2 = p3;
    }
    *p = p1;
}

/****************************************************************************
*				ins_event
* Inputs:
*	event_type *p: a linked list in which to insert
*	event_type event: the new event to insert
* Effect:
*	inserts event into score in reverse time order (this makes inserts
*	that are sequential in time go fast)
****************************************************************************/

static void ins_event(p, event)
event_type *p;			/* the score */
event_type event;		/* the new event to insert */
{
    event_type ptr = *p;
    event_type prv;

    if (ptr == NULL || event->ntime >= ptr->ntime) {
	event->next = ptr;	/* insert at head of list */
	*p = event;
    } else {			/* list insert */
	while (ptr != NULL && event->ntime < ptr->ntime) {
	    prv = ptr;
	    ptr = ptr->next;
	}
	prv->next = event;
	event->next = ptr;
    }
}

/****************************************************************************
*				nalloc
* Outputs: returns event_type for an event allocated from heap, NULL on error
* Effect: allocates memory, decreases space accordingly
****************************************************************************/

static event_type nalloc()
{
    event_type result;
#ifndef UNIX
    space -= sizeof(struct event_struct);
#endif
    if (space > 0) {
	result = ((event_type) malloc(sizeof(struct event_struct)));
	if (result == NULL)
	    fprintf(stderr, "Internal error: Out of memory, space = %ld.\n", space);
    } else
	result = NULL;
    return result;
}

/****************************************************************************
*				ctrlalloc
* Outputs: returns an event_type for representing a control change
*	   returns NULL if space is unavailable
* Effect: allocates ctrlsize bytes
* Assumes: space tells how many bytes are available
****************************************************************************/
static event_type ctrlalloc()
{
    event_type result;
#ifndef UNIX
    space -= ctrlsize;
#endif
    if (space > 0) {
	result = (event_type) malloc(ctrlsize);
	if (result == NULL)	/* this should never happen ... */
	    fprintf(stderr, "Internal error: Out of memory, space = %ld.\n", space);
    } else
	result = NULL;
    return result;
}
