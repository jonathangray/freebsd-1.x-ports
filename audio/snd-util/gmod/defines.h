/* This file is part of the GMOD package */

#define TRUE 	1
#define FALSE	0

#define CMD_ARPEG		0x00
#define CMD_SLIDEUP		0x01
#define CMD_SLIDEDOWN		0x02
#define CMD_SLIDETO		0x03
#define CMD_VIBRATO		0x04
#define CMD_PORTANDVOL		0x05
#define CMD_VIBRAANDVOL		0x06
#define CMD_TREMOLO		0x07
#define CMD_SETOFFSET		0x09
#define CMD_VOLSLIDE		0x0a
#define CMD_JUMP		0x0b
#define CMD_VOLUME		0x0c
#define CMD_BREAK		0x0d
#define CMD_EXTENDED		0x0e
#define CMD_SPEED		0x0f
#define CMD_NOP			0xfe

/* "extended" commands */
#define CMD_FINEPORTUP		0xe1
#define CMD_FINEPORTDOWN	0xe2
#define CMD_GLISSANDO		0xe3
#define CMD_VIBRA_WAVE		0xe4
#define CMD_PATTERN_LOOP	0xe6
#define CMD_TREMOLO_WAVE	0xe7
#define CMD_SET_PAN		0xe8
#define CMD_RETRIGGER		0xe9
#define CMD_FINEVOLUP		0xea
#define CMD_FINEVOLDOWN		0xeb
#define CMD_CUT_NOTE		0xec
#define CMD_DELAY_NOTE		0xed
#define CMD_DELAY_PAT		0xee

/* more commands */
#define CMD_SET_TICKS           0x10
#define CMD_SET_BPM             0x11

#define CVT_MOD_SPEED(x)        ((x) < 32 ? CMD_SET_TICKS : CMD_SET_BPM)

#define MIN(a, b)		((a) < (b) ? (a) : (b))

#define BYTE(x)		(*(u_char *)(x))
#define INTEL_SHORT(x)	(BYTE(x)  | (BYTE(x+1) <<8))
#define INTEL_LONG(x)	(INTEL_SHORT(x) | (INTEL_SHORT(x+2) <<16))

#define MAX_TRACK	32
#define MAX_PATTERN	256
#define MAX_POSITION	256
#define MAX_SAMPLES	255

#define VOL_SLIDE_RATE	4	/* rate for volume slides, bigger is faster */

/* slide types */

#define SLIDE_UPDOWN	1	/* slide up or down */
#define SLIDE_ONCE	2	/* only slide once */
#define SLIDE_PORT	3	/* tone portamento */

/* panning flags */
#define PAN_NO_HARDWARE	0
#define PAN_HARDWARE	1

/* move types (bit flags) */

#define MOVE_LOOP	0x01
#define MOVE_JUMP	0x02
#define MOVE_EXIT	0x04

/* volume types */

#define VOL_LINEAR      0
#define VOL_LOG         1

#define NUM_PERIODS 128		/* number of periods in period table */
#define NUM_VIBRA 64		/* number of values in vibrato table */
#define NOTE_BASE 0		/* lowest note possible */
