/* This program was written by Alexander Siegel in September of 1989   */
/* at Cornell University.  It may may copied freely for private use or */
/* public dispersion provided that this comment is not removed.  This  */
/* program, any portion of this program, or any derivative of this     */
/* program may not be sold or traded for financial gain.               */

/* Start up default values */
#define DEFWORLD "goldlev"  /* Default world name */

int xsize,ysize;        /* Current level width and height */
int levelstart;         /* Level that player started at */
int levelnum;           /* Current level number */
int score;              /* Total score */
int speed;              /* Speed of game.  1 is slowest, 5 is default */
int goldleft;           /* Total number of treasure blocks left */
char *worldname;        /* Name of world (set of levels) */
char filename[300];     /* Current file name of this level */
int curtick;            /* Current clock tick number */

Display *disp;          /* X11 display of client */
Window wind;            /* X11 window where game is displayed */

/* Enumerated type to described direction or activity */
enum directs {
  STAND,UP,DOWN,LEFT,RIGHT,DIGLEFT,DIGRIGHT,PUTDOWN
};
/* Structure describing all stats of thing */
struct thing_s {
  int xstart,ystart;        /* Starting position of thing.  For both */
                            /* this pair and (xpos,ypos), the value is */
                            /* actually 2 times the expected value. */
                            /* This allows for describing when a thing */
                            /* is half way between one block and another. */
  int xpos,ypos;            /* Current position of thing */
  int xold,yold;            /* Previous position before last movement */
  int redraw;               /* Non-zero if thing should be redrawn */
  enum directs dir;         /* Current movement direction or action */
  char hold;                /* What is it carrying */
} player;   /* The player is a thing too */

/* These are used in the bit pattern for the generalized block */
/* description data structure which is in shared.c.  New bit */
/* descriptor can be added without modifying anything else. */
#define UPLEVEL 0x2         /* Jump to next level on contact */
#define ULEAVE  0x4         /* Can you leave this block upwards */
#define DLEAVE  0x8         /* Can you leave this block downwards */
#define LLEAVE  0x10        /* Can you leave this block to the left */
#define RLEAVE  0x20        /* Can you leave this block to the right */
#define HENTER  0x40        /* Can you enter this block horizontally */
#define VENTER  0x80        /* Can you enter this block vertically */
#define STOPBAD 0x100       /* Stops badguys from passing through. */
                            /* They must climb out. */
#define CANDIG  0x200       /* Can this block be dug */
#define INACTIVE 0x400      /* Does this block activate after treasure */
                            /* is gone, space is used otherwise */
#define TREASURE 0x800      /* Get points for contact, then turned */
                            /* into space */
#define KILLIN  0x1000      /* Kills anyone standing in it */
#define NODRAW  0x2000      /* Can not draw it in editor */
#define DIGUND  0x4000      /* Can dig out underneath this block */
#define PICKUP  0x8000      /* Can pickup block and replace it with space */
#define TELEPORT 0x10000    /* Does this cause teleportation on contact */
#define ARMOR   0x20000     /* Grants invulnerability to player if held */
#define STOPFALL 0x40000    /* Prevent holder from falling */
#define NSHOVEL 0x80000     /* Holder can dig much bigger holes */
#define TIMESTOP 0x100000   /* Stop time for all except player if held */
#define REVERSE 0x200000    /* Reverse bad guy direction if player holds */
#define UPTWO   0x400000    /* Causes jump up two spaces on contact */
#define ANCHOR  0x800000    /* Slows movement to one half if held, and */
                            /* prevents jumps or teleports */
#define UPALL  0x1000000    /* Jump to the top of screen on contact */
#define SPEED  0x2000000    /* Allow player double movement if held */
#define PSHOVEL 0x4000000   /* Makes holes permanent when dug */
#define RSHOVEL 0x8000000   /* Make bricks out of space */
#define DFALL   0x10000000  /* Fall downwards out of this space */
#define UFALL   0x20000000  /* Fall upwards out of this space */
#define LFALL   0x40000000  /* Fall to the left out of this space */
#define RFALL   0x80000000  /* Fall to the right out of this space */
#define CANFALL (DFALL | UFALL | LFALL | RFALL) /* Bitmask for */
                                                /* detecting gravity */

/* Predefined important block types.  Other types can be added WITHOUT */
/* adding to this list. */
#define SPACE ' '
#define BRICK '#'
#define STONE '@'
#define HOLE1 '1'
#define PLAYER 'p'
#define BADGUY 'b'

#define MAXLEVEL 5000     /* Maximum size of a level */
char level[MAXLEVEL];     /* Array describing level using characters */
                          /* from above */
char moveallow[MAXLEVEL]; /* Array describing which directions can be moved */
                          /* out of any position in level */
/* Bit patterns for moveallow array */
#define MOVEUP    0x1   /* Upward movement is allowed */
#define MOVEDOWN  0x2   /* Downward movement is allowed */
#define MOVELEFT  0x4   /* Left movement is allowed */
#define MOVERIGHT 0x8   /* Right movement is allowed */
#define FORCEUP    0x10 /* Upward movement is forced */
#define FORCEDOWN  0x20 /* Downward movement is forced */
#define FORCELEFT  0x40 /* Left movement is forced */
#define FORCERIGHT 0x80 /* Right movement is forced */
#define FORCEALL   0xf0 /* Movement is forced in some direction */

/* Array of block type descriptors.  The actual values for this array */
/* are in shared.c. */
struct symbs_s {
  char symb;    /* Character symbol representing block */
  char inplay;  /* Character describing what this block looks like */
                /* during actual play.  It is the symbol character */
                /* for this or another block type. */
  char *name;   /* Symbol name in English */
  char *bits;   /* Bitmap defined by a X11 bitmap file. */
  long code;    /* Bit code describing properties of block */
  KeySym xkey1,xkey2;   /* One or two X11 keyboard symbols for symbol */
};

/* Array for fast lookup of block types.  This array is index by the */
/* actual block character. */
struct fast_s {
  GC gc;        /* Graphics cursor used for drawing block */
  long code;    /* Code describing block properties */
} fast_lookup[256];

/* Global declarations of some functions */
GC makegc();
void xstart();
void xend();
void setchar();
void draw_block();
void draw_level();
void load_level();
void save_level();
int overlap_badguy();
void move_badguy();
void init_badguy();
void start_badguy();
void move_badguys();
void draw_badguys();
int in_hole();
int movething();
void fill_hole();
void regen_tree();
void regen_allow();
void add_score();
void draw_score();
void change_holes();
void redrawall();
