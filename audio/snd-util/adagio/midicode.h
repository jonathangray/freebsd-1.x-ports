/****************************************************************************
*
* Midi codes
*
****************************************************************************/

#define MIDI_DATA(d) (0x7f & (d))
#define MIDI_CHANNEL(c) (0x0f & ((c) - 1))
#define MIDI_PROGRAM(p) MIDI_DATA((p) - 1)

#define MIDI_CODE_MASK		0xf0
#define MIDI_CHN_MASK		0x0f
#define MIDI_OFF_NOTE		0x80
#define MIDI_ON_NOTE		0x90
#define MIDI_POLY_TOUCH  	0xa0
#define MIDI_CTRL		0xb0
#define MIDI_CH_PROGRAM		0xc0
#define MIDI_TOUCH		0xd0
#define MIDI_BEND		0xe0
#define MIDI_EOX		0xF7

#define MODWHEEL 1
#define BREATH 2
#define FOOT 4
#define PORTARATE 5
#define DATA_ENTRY 6
#define VOLUME 7
#define PAN 10
#define EXPRESSION 11
#define SUSTAIN 64
#define PORTASWITCH 65
#define ALL_NOTES_OFF 123
#define SOSTENUTO 66
#define SOFT_PEDAL 67
#define GENERAL_4 68
#define HOLD_2 69
#define GENERAL_5 80
#define GENERAL_6 81
#define GENERAL_7 82
#define GENERAL_8 83
#define REVERBERATION 91
#define TREMULO_DEPTH 92
#define CHORUS_DEPTH 93
#define DETUNE 94
#define PHASER_DEPTH 95
#define NRPN_L 98
#define NRPN_M 99
