/* song.h 
	vi:se ts=3 sw=3:
 */

/* internal data structures for the soundtracker player routine....
 */

/* $Id: song.h,v 1.1 1994/02/19 16:03:09 ache Exp $
 * $Log: song.h,v $
 * Revision 1.1  1994/02/19 16:03:09  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:55:59  espie
 * REAL_MAX_PITCH for better player.
 *
 * Revision 1.3  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1993/12/28  13:54:44  Espie
 * REAL_MAX_PITCH != MAX_PITCH.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.5  1993/12/02  15:45:33  espie
 * Added samples_start.
 *
 * Revision 3.4  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.5  1992/10/31  11:18:00  espie
 * New fields for optimized resampling.
 * Exchanged __ANSI__ to SIGNED #define.
 */

#ifdef SIGNED
typedef signed char SAMPLE;
#else
typedef char SAMPLE;
#endif

#define NUMBER_SAMPLES 32

#define BLOCK_LENGTH 64
#define NUMBER_TRACKS 4
#define NUMBER_PATTERNS 128

#define NUMBER_EFFECTS 40

/* some effects names */
#define EFF_ARPEGGIO    0
#define EFF_DOWN        1
#define EFF_UP          2
#define EFF_PORTA       3
#define EFF_VIBRATO     4
#define EFF_PORTASLIDE  5
#define EFF_VIBSLIDE    6
#define EFF_OFFSET      9
#define EFF_VOLSLIDE    10
#define EFF_FF          11
#define EFF_VOLUME      12
#define EFF_SKIP        13
#define EFF_EXTENDED    14
#define EFF_SPEED       15
#define EFF_NONE        16
#define EXT_BASE        16
#define EFF_SMOOTH_UP   (EXT_BASE + 1)
#define EFF_SMOOTH_DOWN (EXT_BASE + 2)
#define EFF_CHG_FTUNE   (EXT_BASE + 5)
#define EFF_LOOP        (EXT_BASE + 6)
#define EFF_RETRIG      (EXT_BASE + 9)
#define EFF_S_UPVOL     (EXT_BASE + 10)
#define EFF_S_DOWNVOL   (EXT_BASE + 11)
#define EFF_NOTECUT     (EXT_BASE + 12)
#define EFF_LATESTART   (EXT_BASE + 13)
#define EFF_DELAY       (EXT_BASE + 14)

#define SAMPLENAME_MAXLENGTH 22
#define TITLE_MAXLENGTH 20

#define MIN_PITCH 113
#define MAX_PITCH 856
#define REAL_MAX_PITCH 1050

#define MIN_VOLUME 0
#define MAX_VOLUME 64

/* the fuzz in note pitch */
#define FUZZ 2

/* we refuse to allocate more than 500000 bytes for one sample */
#define MAX_SAMPLE_LENGTH 500000

struct sample_info
   {
   char *name;
   int  length, rp_offset, rp_length;
   unsigned long  fix_length, fix_rp_length;
   int volume;
   int finetune;
   SAMPLE *start, *rp_start;
   };

/* the actual parameters may be split in two halves occasionnally */

#define LOW(para) ((para) & 15)
#define HI(para) ((para) >> 4)

struct event
   {
   unsigned char sample_number;
   unsigned char effect;
   unsigned char parameters;
   unsigned char note;
   int pitch;
   };

struct block
   {
   struct event e[NUMBER_TRACKS][BLOCK_LENGTH];
   };
    
        
struct song_info
   {
   int length;
   int maxpat;
   int transpose;
   char patnumber[NUMBER_PATTERNS];
   struct block *pblocks;
   };

struct song
   {
   char *title;
      /* sample 0 is always a dummy sample */
   struct sample_info samples[NUMBER_SAMPLES];
   struct song_info info;
   long samples_start;
   };

#define AMIGA_CLOCKFREQ 3575872
