
#ifndef true
#define true 1
#endif
#ifndef false
#define false 0
#endif

#define NUM_CHANS 16
#define NO_PITCH (128)

struct note_struct {
    unsigned short ndur;	/* duration */
    unsigned char npitch;	/* pitch (middle C = 48) */
    unsigned char nloud;	/* loudness (MIDI velocity) */
    unsigned char nprogram;	/* adagio Z parameter (MIDI program) */
    unsigned char nbank;	/* tone bank */
    unsigned char neffect;	/* special effects */
    };

struct ctrl_struct {
    unsigned char control;
    unsigned short value;
    };

typedef struct event_struct {
    struct event_struct *next;
    unsigned long ntime;	/* start time */
    unsigned short nline;	/* line number from source code */
    unsigned char nvoice;	/* adagio voice (MIDI Channel)
				 *  if this is a control change, high order 4 bits
				 *  contain the control number, otherwise high order
				 *  4 bits are 0 (see is_note macro below)
			 	 */
    unsigned char ntrack;	/* source track, for interpreting midi files */
    unsigned char ndest;	/* the external channel to send this note to */
    char ncard;			/* the soundcard to send this note to */
    union {
	struct note_struct note;
	struct ctrl_struct ctrl;
	} u;
    } *event_type;

#define nctrl 16
#define ctrlsize (sizeof(struct event_struct) - \
		  sizeof(struct note_struct) + sizeof(struct ctrl_struct))
#define ctrl_voice(c, v) (((c) << 4) + (v))
#define vc_ctrl(v) ((v) >> 4)
#define vc_voice(v) ((v) & 0x0F)
#define is_note(n) (((n)->nvoice & 0xF0) == 0)

extern int program[NUM_CHANS];	/* midi program (timbre control) of note */
#define PERCCHAN(v) (percsel&(1<<(v)))
#define ID_LINUX 0x30
#define LSYSEX_SIZE 52+8+1
#ifndef XSELECT
#define XSELECT 0
#endif
#ifndef PSELECT
#define PSELECT 0x8200
#endif
#ifndef SBDIR
#define SBDIR "/etc"
#endif

#define ECHO_EFFECT 1
#define CHORUS1_EFFECT 2
#define CHORUS2_EFFECT 4

