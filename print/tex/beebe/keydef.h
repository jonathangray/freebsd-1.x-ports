#ifndef ANSI_LIBRARY
#define ANSI_LIBRARY 0
#endif

#if    ANSI_LIBRARY
#define ANSI_PROTOTYPES 1
#include <stdlib.h>
#endif

#ifndef ANSI_PROTOTYPES
#define ANSI_PROTOTYPES 0
#endif

typedef int KEYMODE;			/* mode flag value */

#if    ANSI_PROTOTYPES			/* ANSI style declarations */
int	kbclose(void);
FILE*	kbfile(void);
int	kbflush(void);
int	kbget(void);
int	kbinput(void);
KEYMODE	kbmode(KEYMODE);
int	kbopen(int);
int	kbunget(char);
#else					/* K&R style declarations */
int	kbclose();
FILE*	kbfile();
int	kbflush();
int	kbget();
int	kbinput();
KEYMODE	kbmode();
int	kbopen();
int	kbunget();
#endif /* ANSI_PROTOTYPES */

/* major function request codes */

#define KB_INQUIRE	0x0001		/* return current keyboard mode */
#define KB_RESTORE	0x0002		/* restore saved keyboard state */
#define KB_SAVE		0x0004		/* save keyboard state (internally) */

/* major mode codes */

#define KB_CBREAK	0x0010		/* 'cbreak' (rare) mode */
#define KB_NORMAL	0x0020		/* 'normal' terminal mode */
#define KB_RAW		0x0040		/* 'raw' mode--all characters input */

/* minor mode modifier codes */

#define KB_ECHO		0x0100		/* echo input characters */
