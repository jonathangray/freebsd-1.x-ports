#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include <time.h>

#if NeedFunctionPrototypes
extern int fprintf(FILE *stream, const char* format, ...);
extern time_t time(time_t *tloc);
#else /* !NeedFunctionPrototypes */
extern int fprintf();
extern time_t time();
#endif /* NeedFunctionPrototypes */

#include "include/score.h"
#include "include/presents.h"
#include "include/special.h"
#include "include/main.h"
#include "include/version.h"
#include "include/error.h"
#include "include/mess.h"
#include "include/ball.h"
#include "include/gun.h"
#include "include/sfx.h"
#include "include/blocks.h"
#include "include/level.h"
#include "include/bonus.h"
#include "include/stage.h"
#include "include/paddle.h"
#include "include/intro.h"
#include "include/inst.h"
#include "include/highscore.h"
#include "include/keys.h"
#include "include/audio.h"
#include "include/misc.h"

#include "include/init.h"

/*
 *  Internal macro definitions:
 */

#define TITLE_FONT 	"-adobe-helvetica-bold-r-*-*-24-*-*-*-*-*-*-*"
#define COPY_FONT   "-adobe-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*"
#define TEXT_FONT  	"-adobe-helvetica-medium-r-*-*-18-*-*-*-*-*-*-*"
#define DATA_FONT  	"-adobe-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*"

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static void ParseCommandLine(char **argv, int argc);
static void InitialiseSettings(void);
static int compareArgument(char *arg1, char *arg2, int minMatch);
static void HandleDisplayErrors(char *displayName);
static void PrintVersion(void);
static void PrintUsage(void);
static void PrintHelp(void);
static void InitialiseGraphics(Display *display, Window window);
static void InitialiseColourNames(Display *display, Colormap colormap);
static void InitialiseFonts(Display *display);
static void ReleaseGraphics(Display *display);
static void ReleaseFonts(Display *display);
static void ExitProgramNow(int value);
static void TurnOnSynchronise(Display *display);
#else
static void ExitProgramNow();
static void ParseCommandLine();
static void InitialiseSettings();
static int compareArgument();
static void HandleDisplayErrors();
static void PrintVersion();
static void PrintUsage();
static void PrintHelp();
static void InitialiseGraphics();
static void InitialiseColourNames();
static void InitialiseFonts();
static void ReleaseGraphics();
static void ReleaseFonts();
static void TurnOnSynchronise();
#endif

/*
 *  Internal variable declarations:
 */

int red, tann, yellow, green, white, black, blue, purple, reds[14], greens[14],
   grey50;
GC gc, gcxor, gcand, gcor, gcsfx;
XFontStruct *titleFont, *copyFont, *textFont, *dataFont;
Colormap 	colormap;
static XVisualInfo	visual_info;
static unsigned long event_mask;
static char *displayName = (char *) NULL;
static int	syncOn;
static int	grabPointer;
static int	useDefaultColourmap;
int	noSound, debug;

#if NeedFunctionPrototypes
static void InitialiseGraphics(Display *display, Window window)
#else
static void InitialiseGraphics(display, window)
	Display *display;
	Window window;
#endif
{
    XGCValues	gcv;

	/* Create a graphics context using copy mode */
	gcv.function   			= GXcopy;
	gcv.fill_style			= FillTiled;
	gcv.graphics_exposures	= False;
	if (!(gc = XCreateGC(display, window, 
		GCFunction | GCFillStyle | GCGraphicsExposures, &gcv)))
		ShutDown(display, 1, "Cannot create GXcopy graphics context.");

	/* Create a graphics context using xor mode */
	gcv.function   			= GXxor;
	gcv.graphics_exposures	= False;
	if (!(gcxor = XCreateGC(display, window, 
		GCFunction | GCGraphicsExposures, &gcv)))
		ShutDown(display, 1, "Cannot create GXxor graphics context.");

	/* Create a graphics context using and mode */
	gcv.function   			= GXand;
	gcv.graphics_exposures	= False;
	if (!(gcand = XCreateGC(display, window, 
		GCFunction | GCGraphicsExposures, &gcv)))
		ShutDown(display, 1, "Cannot create GXand graphics context.");
    XSetForeground(display, gcand, 0);
	XSetBackground(display, gcand, ~0);

	/* Create a graphics context using or mode */
	gcv.function   			= GXor;
	gcv.graphics_exposures	= False;
	if (!(gcor = XCreateGC(display, window, 
		GCFunction | GCGraphicsExposures, &gcv)))
		ShutDown(display, 1, "Cannot create GXor graphics context.");

	/* Create a graphics context for use by sfx */
	gcv.function   			= GXcopy;
	gcv.graphics_exposures	= False;
	if (!(gcsfx = XCreateGC(display, window, 
		GCFunction | GCGraphicsExposures, &gcv)))
		ShutDown(display, 1, "Cannot create GXsfx graphics context.");
}

#if NeedFunctionPrototypes
static void InitialiseColourNames(Display *display, Colormap colormap)
#else
static void InitialiseColourNames(display, colormap)
	Display *display;
	Colormap colormap;
#endif
{
    /* Obtain the colour index of several colours from colourmap */
	red     = ColourNameToPixel(display, colormap, "red");
	tann    = ColourNameToPixel(display, colormap, "tan");
	yellow  = ColourNameToPixel(display, colormap, "yellow");
	green   = ColourNameToPixel(display, colormap, "green");
	white   = ColourNameToPixel(display, colormap, "white");
	black   = ColourNameToPixel(display, colormap, "black");
	purple  = ColourNameToPixel(display, colormap, "purple");
	blue    = ColourNameToPixel(display, colormap, "blue");
	grey50  = ColourNameToPixel(display, colormap, "grey50");
}

#if NeedFunctionPrototypes
static void InitialiseCycleColourNames(Display *display, Colormap colormap)
#else
static void InitialiseCycleColourNames(display, colormap)
	Display *display;
	Colormap colormap;
#endif
{
	reds[0] = ColourNameToPixel(display, colormap, "#f00");
	reds[1] = ColourNameToPixel(display, colormap, "#e00");
	reds[2] = ColourNameToPixel(display, colormap, "#d00");
	reds[3] = ColourNameToPixel(display, colormap, "#c00");
	reds[4] = ColourNameToPixel(display, colormap, "#b00");
	reds[5] = ColourNameToPixel(display, colormap, "#a00");
	reds[6] = ColourNameToPixel(display, colormap, "#900");
	reds[7] = ColourNameToPixel(display, colormap, "#800");
	reds[8] = ColourNameToPixel(display, colormap, "#700");
	reds[9] = ColourNameToPixel(display, colormap, "#600");
	reds[10] = ColourNameToPixel(display, colormap, "#500");
	reds[11] = ColourNameToPixel(display, colormap, "#400");
	reds[12] = ColourNameToPixel(display, colormap, "#300");
	reds[13] = ColourNameToPixel(display, colormap, "#200");

	greens[0] = ColourNameToPixel(display, colormap, "#0f0");
	greens[1] = ColourNameToPixel(display, colormap, "#0e0");
	greens[2] = ColourNameToPixel(display, colormap, "#0d0");
	greens[3] = ColourNameToPixel(display, colormap, "#0c0");
	greens[4] = ColourNameToPixel(display, colormap, "#0b0");
	greens[5] = ColourNameToPixel(display, colormap, "#0a0");
	greens[6] = ColourNameToPixel(display, colormap, "#090");
	greens[7] = ColourNameToPixel(display, colormap, "#080");
	greens[8] = ColourNameToPixel(display, colormap, "#070");
	greens[9] = ColourNameToPixel(display, colormap, "#060");
	greens[10] = ColourNameToPixel(display, colormap, "#050");
	greens[11] = ColourNameToPixel(display, colormap, "#040");
	greens[12] = ColourNameToPixel(display, colormap, "#030");
	greens[13] = ColourNameToPixel(display, colormap, "#020");
}

#if NeedFunctionPrototypes
static void InitialiseFonts(Display *display)
#else
static void InitialiseFonts(display)
	Display *display;
#endif
{
	/* Create all required font structures */

	/* Font used for titles */
    if (!(titleFont = XLoadQueryFont(display, TITLE_FONT)))
	{
		WarningMessage("Cannot open the title font. Using fixed.");
		titleFont = XLoadQueryFont(display, "fixed");
	}

	/* Very small font used for copyright message etc. */
	if (!(copyFont = XLoadQueryFont(display, COPY_FONT)))
	{
		WarningMessage("Cannot open the copyright font. Using fixed.");
		copyFont = XLoadQueryFont(display, "fixed");
	}

	/* Font used for general text everywhere */
	if (!(textFont = XLoadQueryFont(display, TEXT_FONT)))
	{
		WarningMessage("Cannot open the text font. Using fixed.");
		textFont = XLoadQueryFont(display, "fixed");
	}

	/* Font used for data text in intro */
	if (!(dataFont = XLoadQueryFont(display, DATA_FONT)))
	{
		WarningMessage("Cannot open the data font. Using fixed.");
		dataFont = XLoadQueryFont(display, "fixed");
	}
}

#if NeedFunctionPrototypes
static void ReleaseFonts(Display *display)
#else
static void ReleaseFonts(display)
	Display *display;
#endif
{
	/* Free all the fonts used */
	if (titleFont)	XFreeFont(display, titleFont);
	if (copyFont)	XFreeFont(display, copyFont);
	if (textFont)	XFreeFont(display, textFont);
	if (dataFont)	XFreeFont(display, dataFont);
}

#if NeedFunctionPrototypes
static void ExitProgramNow(int value)
#else
static void ExitProgramNow(value)
	int value;
#endif
{
	/* Return to the shell with error code */
	exit(value);
}

#if NeedFunctionPrototypes
static void ReleaseGraphics(Display *display)
#else
static void ReleaseGraphics(display)
	Display *display;
#endif
{
	/* Free the graphics contexts */
	if (gc) 	XFreeGC(display, gc);
	if (gcxor) 	XFreeGC(display, gcxor);
	if (gcand) 	XFreeGC(display, gcand);
	if (gcor) 	XFreeGC(display, gcor);
	if (gcsfx) 	XFreeGC(display, gcsfx);
}

#if NeedFunctionPrototypes
void ShutDown(Display *display, int exit_code, char *message)
#else
void ShutDown(display, exit_code, message)
	Display *display;
	int exit_code;
	char *message;
#endif
{
	/* This is the last function called when exiting */

	/* Remove the colour map */
	if (colormap) XUninstallColormap(display, colormap);

	/* Close the audio device if available and wanted */
	if (noSound == False)
		(void) FreeAudioSystem();

	FreeMisc(display);				/* Free backing store pixmap*/
	FreeKeyControl(display);		/* Free key control         */
	FreeHighScore(display);			/* Free high score memory 	*/
	FreeInstructions(display);		/* Free instructions        */
	FreeBonus(display);				/* Free bonus memory 		*/
	FreeIntroduction(display);		/* Free introduction memory */
	FreeMessageSystem(display);		/* Free message system 		*/
	FreePaddle(display);			/* Free paddle pixmaps 		*/
	FreeLevelInfo(display);			/* Free level pixmaps 		*/
	FreeScoreDigits(display);		/* Free digit pixmaps 		*/
	FreeBlockPixmaps(display);		/* Free all block pixmaps 	*/
	FreeBall(display);				/* Free the ball animation 	*/
	FreeBullet(display);			/* Free the bullet          */
	ReleaseGraphics(display);		/* Free graphics contexts 	*/
	ReleaseFonts(display);			/* Unload fonts used		*/

	/* Output an error message if required */
	if (message[0] != '\0')
	{
		/* If we had an error then use error message */
		if (exit_code == 1)
			ErrorMessage(message);
		else
			NormalMessage(message);
	}

	/* Exit with the error code */
	ExitProgramNow(exit_code);
}

#if NeedFunctionPrototypes
static int ErrorHandler(Display *display, XErrorEvent *err)
#else
static int ErrorHandler(display, err)
	Display *display;
	XErrorEvent *err;
#endif
{
 	char msg[80];
 	char string[256];

	/* Obtain the error message from the server */
	XGetErrorText(display, err->error_code, msg, 80);
	sprintf(string, "Xlib Error: %s", msg);

	/* Close down the system */
	ShutDown(display, 1, string);

	/* Not reached but makes the compiler happy */
	return True;
}

#if NeedFunctionPrototypes
static void HandleDisplayErrors(char *displayName)
#else
static void HandleDisplayErrors(displayName)
	char *displayName;
#endif
{
 	char string[256];

	/* Check if the DISPLAY variable is set and write correct message */
	if (getenv("DISPLAY") == NULL)
		WarningMessage("Your X-Window system display variable is not set.");
	else
	{
		sprintf(string, "Cannot connect to display called <%s>.", displayName);
		WarningMessage(string);
	}
}

#if NeedFunctionPrototypes
static void PrintVersion(void)
#else
static void PrintVersion()
#endif
{
    /* Print version for program to user for command line help */
    fprintf(stdout, 
		"XBoing by Justin Kibell (jck@citri.edu.au)\n"); 
    fprintf(stdout, 
	  "Version %d.%d %s\n", VERSION, REVNUM + buildNum, dateString);
    fprintf(stdout, "(c) Copyright 1993, All Rights Reserved\n");

	/* Exit now */
	ExitProgramNow(0);
}

#if NeedFunctionPrototypes
static void PrintUsage(void)
#else
static void PrintUsage()
#endif
{
    /* Print usage to user for command line help */

    fprintf(stdout, "%s%s",
		"Usage: XBoing [-version] [-usage] [-help] [-sync] ",
		"[-display <displayName>]\n"); 
    fprintf(stdout, "%s%s\n%s%s\n%s\n",
		"              [-speed <1-9>] [-scores] [-keys] [-sound] [-setup]",
		" [-nosfx]",
		"              [-nograb] [-maxvol <1-100>] [-startlevel <1-MAX>]",
		" [-usedefcmap]",
		"              [-nickname <name>]");

	/* Exit now */
	ExitProgramNow(0);
}

#if NeedFunctionPrototypes
static void PrintSetup(void)
#else
static void PrintSetup()
#endif
{
    /* Print setup information about xboing */
	char *str;

    fprintf(stdout, "xboing: Setup Information\n");
    fprintf(stdout, "           Audio: %s\n", 
		AUDIO_AVAILABLE ? "Available" : "Not available");
    fprintf(stdout, "      Audio File: %s\n", AUDIO_FILE);
    fprintf(stdout, "         Version: %d.%d\n", VERSION, REVNUM + buildNum);
    fprintf(stdout, "        Compiled: %s\n", dateString);
    fprintf(stdout, "     Compiled by: %s <%s>\n", 
		getUsersFullName(), whoString);

	/* If the environment variable exists - use it */
	if ((str = getenv("XBOING_SCORE_FILE")) != NULL)
    	fprintf(stdout, "      Score File: %s\n", str);
	else
    	fprintf(stdout, "      Score File: %s\n", HIGH_SCORE_FILE);

	/* If the environment variable exists - use it */
	if ((str = getenv("XBOING_LEVELS_DIR")) != NULL)
    	fprintf(stdout, " Level directory: %s\n", str);
	else
    	fprintf(stdout, " Level directory: %s\n", LEVEL_INSTALL_DIR);

    fprintf(stdout, "Number of levels: %d\n", MAX_NUM_LEVELS);

	/* If the environment variable exists - use it */
	if ((str = getenv("XBOING_SOUND_DIR")) != NULL)
    	fprintf(stdout, "Sounds directory: %s\n", str);
	else
    	fprintf(stdout, "Sounds directory: %s\n", SOUNDS_DIR);

	/* Exit now */
	ExitProgramNow(0);
}


#if NeedFunctionPrototypes
static void PrintHelp(void)
#else
static void PrintHelp()
#endif
{
    /* Print help for program to user for command line help */
    fprintf(stdout, 
		"XBoing by Justin Kibell (jck@citri.edu.au)\n"); 
    fprintf(stdout, "Version %d.%d %s\n", VERSION, REVNUM + buildNum,
    	"(c) Copyright 1993, All Rights Reserved\n");

    fprintf(stdout, "Help: \n");
    fprintf(stdout, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
        "    -speed <n>              - The game speed, 1 - 9. 9 = fast\n",
        "    -maxvol <n>             - The maximum volume as percentage.\n",
        "    -startlevel <n>         - The starting level for game.\n",
        "    -help                   - Produce this help message.\n",
        "    -sync                   - Turn on X synchronisation.\n",
        "    -usage                  - Print a brief help message.\n",
        "    -version                - Print out the current version.\n",
        "    -scores                 - Print out the current highscores.\n",
        "    -keys                   - Use keys instead of mouse control.\n",
        "    -sound                  - Turn audio ON for game.\n",
        "    -setup                  - Print setup information.\n",
        "    -nosfx                  - Do not use special effects.\n",
        "    -nograb                 - Turn off pointer grab.\n",
        "    -usedefcmap             - Use the default colourmap.\n",
		"    -nickname <name>        - Use nickname instead of real name.\n",
        "    -display <display>      - Set the display for the game.\n");

	/* Exit now */
	ExitProgramNow(0);
}

#if NeedFunctionPrototypes
static int compareArgument(char *arg1, char *arg2, int minMatch)
#else
static int compareArgument(arg1, arg2, minMatch)
	char *arg1;
	char *arg2;
	int minMatch;
#endif
{
    if ((strlen(arg1) < minMatch) || (strlen(arg2) < minMatch)) 
        return 1;
    if (strlen(arg1) > strlen(arg2)) 
        return 1;

	/* Return true or false basically */
    return (strncmp(arg1, arg2, strlen(arg1)));
}

#if NeedFunctionPrototypes
static void TurnOnSynchronise(Display *display)
#else
static void TurnOnSynchronise(display)
	Display *display;
#endif
{
	/* Turn the X synchronisation on to flush all calls each frame */
	XSynchronize(display, True);
}

#if NeedFunctionPrototypes
static void InitialiseSettings(void)
#else
static void InitialiseSettings()
#endif
{
	/* Initialise some variables */
	syncOn = False;
	debug = False;
	grabPointer = True;
	useDefaultColourmap = False;
	SetNickName("");

	/* The audio is off by default */
	noSound = True;

	/* So the audio code will use system default */
	SetMaximumVolume(0);	

	/* Always start on level one */
	SetStartingLevel(1);
	SetLevelNumber(1);

	/* Not so fast */
	SetUserSpeed(7);

	useSpecialEffects(True);
	score = 0L;
}

#if NeedFunctionPrototypes
static void ParseCommandLine(char **argv, int argc)
#else
static void ParseCommandLine(argv, argc)
	char **argv;
	int argc;
#endif
{
	/* Parse the command line options */
	int i, l;
	char str[80];

	/* Initialise variables */
	InitialiseSettings();

	for (i = 1; i < argc; i++)
	{
		if (argv[i][0] != '-')
		{
			PrintUsage();
			break;
		}

		if (!compareArgument(argv[i], "-display", 7))
		{
			/* Set the display name for later connection */
			i++;
			if (i < argc)
				displayName = argv[i];
			else PrintUsage();

		} else if (!compareArgument(argv[i], "-help", 4))
		{
			/* Print out the help information and quit */
			PrintHelp();

		} else if (!compareArgument(argv[i], "-debug", 5))
		{
			/* Turn on debugging */
			NormalMessage("Debugging mode activated.");
			debug = True;

		} else if (!compareArgument(argv[i], "-setup", 5))
		{
			/* Print out information on setup */
			PrintSetup();

		} else if (!compareArgument(argv[i], "-sync", 4))
		{
			/* Turn on X protocol synchronisation */
			syncOn = True;

			DEBUG("X Synchronisation turned on.")

		} else if (!compareArgument(argv[i], "-version", 7))
		{
			/* Print out the version information and quit */
			PrintVersion();

		} else if (!compareArgument(argv[i], "-sound", 5))
		{
			/* Enable the sound in the game */
			noSound = False;

			DEBUG("Sound turned on.")

		} else if (!compareArgument(argv[i], "-keys", 4))
		{
			/* Set the paddle control mode to use keys */
			SetPaddleControlMode(CONTROL_KEYS);

			DEBUG("Keyboard mode activated.")

		} else if (!compareArgument(argv[i], "-scores", 6))
		{
			/* List all the highscores */
			CommandlineHighscorePrint();
			ExitProgramNow(0);

		} else if (!compareArgument(argv[i], "-usage", 5))
		{
			/* Print out the usage and quit */
			PrintUsage();

		} else if (!compareArgument(argv[i], "-nosfx", 5))
		{
			/* Turn off special effects then */
			useSpecialEffects(False);

			DEBUG("Special effects off.")
			
		} else if (!compareArgument(argv[i], "-nograb", 6))
		{
			/* Turn off pointer grabbing */
			grabPointer = False;

			DEBUG("Pointer grabbing turned off.")

		} else if (!compareArgument(argv[i], "-nickname", 8))
		{
			/* Use a nick name instead of real name */
			i++;
			if (i < argc)
			{
				/* Dont have too long names */
				if (strlen(argv[i]) > 20)
					WarningMessage(
						"Your nickname is too long. It will be cropped.");

				/* Use this nickname given */
				SetNickName(argv[i]);
			} else PrintUsage();

		} else if (!compareArgument(argv[i], "-usedefcmap", 10))
		{
			/* Try to use the default colourmap */
			useDefaultColourmap = True;

			DEBUG("Using default colourmap please.")

		} else if (!compareArgument(argv[i], "-speed", 5))
		{
			/* Set the speed for the game */
			i++;
			if (i < argc)
			{
				/* Obtain the speed setting */
				l = atoi(argv[i]);
				if ((l > 0) && (l <= 9))
					SetUserSpeed(10 - l);
				else 
				{
					WarningMessage("The speed setting range is [1-9]");
					PrintUsage();
				}
			} else PrintUsage();

		} else if (!compareArgument(argv[i], "-startlevel", 10))
		{
			/* Set the starting level */
			i++;
			if (i < argc)
			{
				/* Obtain the start level setting */
				l = atoi(argv[i]);
				if ((l > 0) && (l <= MAX_NUM_LEVELS))
					SetStartingLevel(l);
				else 
				{
					sprintf(str, "The starting level range is [1-%d]", 
						MAX_NUM_LEVELS);
					WarningMessage(str);
					PrintUsage();
				}
			} else PrintUsage();

		} else if (!compareArgument(argv[i], "-maxvol", 6))
		{
			/* Set the maximum volume to use for the game */
			i++;
			if (i < argc)
			{
				/* Obtain the maximum volume setting */
				l = atoi(argv[i]);
				if ((l >= 1) && (l <= 100))
					SetMaximumVolume(l);
				else 
				{
					WarningMessage("Maximum volume range is [1-100]");
					PrintUsage();
				}
			} else PrintUsage();

		} else PrintUsage();
	}
}

#if NeedFunctionPrototypes
void UnGrabPointer(Display *display)
#else
void UnGrabPointer(display)
	Display *display;
#endif
{
	/* Ungrab the pointer */
	if (grabPointer == True)
		XUngrabPointer(display, CurrentTime);
}

#if NeedFunctionPrototypes
void ChangePointer(Display *display, Window window, int cursorState)
#else
void ChangePointer(display, window, cursorState)
	Display *display;
	Window window;
	int cursorState;
#endif
{
	static Cursor cursor;
	XColor colour;
	Pixmap cursorPixmap;

	/* Create a cursor with nothing inside it */
	colour.pixel = WhitePixel(display, DefaultScreen(display));
	XQueryColor(display, 
		DefaultColormap(display, DefaultScreen(display)), &colour);

	/* Free old cursor */
	if (cursor) XFreeCursor(display, cursor);

	/* Create a cursor based on type needed */
	switch (cursorState)
	{
		case CURSOR_WAIT:
			cursor = XCreateFontCursor(display, XC_watch);
			break;

		case CURSOR_PLUS:
			cursor = XCreateFontCursor(display, XC_plus);
			break;

		case CURSOR_NONE:
			cursorPixmap = XCreatePixmap(display, window, 1, 1, 1);
			cursor = XCreatePixmapCursor(display, 
				cursorPixmap, cursorPixmap, &colour, &colour, 0, 0);
			if (cursorPixmap) XFreePixmap(display, cursorPixmap);
			break;
	}

	XDefineCursor(display, window, cursor);
}

#if NeedFunctionPrototypes
void GrabPointer(Display *display, Window window)
#else
void GrabPointer(display, window)
	Display *display;
	Window window;
#endif
{
	if (grabPointer == True)
	{
		/* Grab the pointer so you cannot move the  mouse out of the main
	 	 * window. Also set the cursor to a new cursor with no shape.
	 	 */
    	if (XGrabPointer(display, window, True,
			ButtonReleaseMask | ButtonPressMask,
			GrabModeAsync, GrabModeAsync, window,
			None, CurrentTime) != GrabSuccess)
		{
			/* Error while grab - not too bad but let user know. */	
			WarningMessage("Pointer grab was unsuccessful.");
		}
	}
}



#if NeedFunctionPrototypes
Display *InitialiseGame(char **argv, int argc)
#else
Display *InitialiseGame(argv, argc)
	char **argv;
	int argc;
#endif
{
	int screen_num;
	static Display *display;
	char str[80];

	/* Setup the default speed for game */
	SetUserSpeed(5);

	/* Set the paddle control mode to mouse */
	SetPaddleControlMode(CONTROL_MOUSE);

	/* Parse all command line arguments - may exit here */
	ParseCommandLine(argv, argc);

	DEBUG("Command line parsed.")

	/* Open a display connection */
	if (!(display = XOpenDisplay(displayName)))
	{
		/* Handle display connection errors */
		HandleDisplayErrors(displayName);
		ExitProgramNow(1);
	}

	/* User wants synchronisation turned on so do it */
	if (syncOn == True) TurnOnSynchronise(display);

	/* Set the error handlers to point to mine */
    XSetErrorHandler(ErrorHandler);

	/* Seed the random number generator */
	srand(time(NULL));

	/* Obtain the screen number for this display */
	screen_num = XDefaultScreen(display);

	/* Make sure that we are using a colour visual */
	if (!XMatchVisualInfo(display, screen_num, 
		DefaultDepth(display, screen_num), PseudoColor, &visual_info))
	{
		if (!XMatchVisualInfo(display, screen_num, 
			DefaultDepth(display, screen_num), DirectColor, &visual_info))
		{
			if (!XMatchVisualInfo(display, screen_num, 
				DefaultDepth(display, screen_num), TrueColor, &visual_info))
			{
				ErrorMessage("Sorry, you must have a colour display. :-(");
				ExitProgramNow(1);
			}
		}
	}

	DEBUG("Display system checked.")

	/* Create our own colour map or use the default one */
	if (useDefaultColourmap == True)
		colormap = XDefaultColormap(display, screen_num);
	else
		colormap = XCreateColormap(display, RootWindow(display, screen_num), 
			visual_info.visual, AllocNone);

	DEBUG("Colourmap created.")

	/* Be polite and let them know we are waiting */
	sprintf(str, "Please wait %s, initialising xboing ...", 
		getUsersFullName());
	NormalMessage(str);

	/* Initialise the audio system if possible */
	if (noSound == False)
	{
		/* Try to turn audio on */
		if (SetUpAudioSystem(display) == False)
		{
			/* Audio failed - let user know */
			noSound = True;
			WarningMessage("Audio unavailable or not supported.");
		}
	}

	DEBUG("Sound system checked.")

	/* Find out some colours */
	InitialiseColourNames(display, colormap);

	DEBUG("Initialised colour names.")

	/* Create all windows */
	CreateAllWindows(display, colormap, argv, argc);

	DEBUG("Created all windows.")

	InitialiseGraphics(display, playWindow);
	InitialiseFonts(display);

	DEBUG("Created GC and fonts.")

	SetBackgrounds(display, colormap);

	DEBUG("Created background pixmaps.")

	/* Initialise all pixmaps and objects and setup special screens etc. */
	InitialiseMessageSystem(display, 	messWindow, 	colormap);
	DEBUG("InitialiseMessageSystem done.")
	InitialiseBlocks(display, 			playWindow, 	colormap);
	DEBUG("InitialiseBlocks done.")
	InitialiseBall(display, 			playWindow, 	colormap);
	DEBUG("InitialiseBall done.")
	InitialiseBullet(display, 			playWindow, 	colormap);
	DEBUG("InitialiseBullet done.")
	InitialiseScoreDigits(display, 		scoreWindow, 	colormap);
	DEBUG("InitialiseScoreDigits done.")
	InitialiseLevelInfo(display, 		levelWindow, 	colormap);
	DEBUG("InitialiseLevelInfo done.")
	InitialisePaddle(display, 			playWindow, 	colormap);
	DEBUG("InitialisePaddle done.")

	SetUpPresents(display, 				mainWindow, 	colormap);
	DEBUG("SetUpPresents done.")
	SetUpKeys(display, 					playWindow, 	colormap);
	DEBUG("SetUpKeys done.")
	SetUpInstructions(display, 			playWindow, 	colormap);
	DEBUG("SetUpInstructions done.")
	SetUpIntroduction(display, 			playWindow, 	colormap);
	DEBUG("SetUpIntroduction done.")
	SetUpBonus(display, 				mainWindow, 	colormap);
	DEBUG("SetUpBonus done.")
	SetUpHighScore(display, 			playWindow, 	colormap);
	DEBUG("SetUpHighScore done.")

	/* Do this after pixmaps all the colourmap */
	InitialiseCycleColourNames(display, colormap);

	DEBUG("Colour cycle indexes created.")

	DisplayLevelInfo(display, levelWindow, level);
	SetLevelTimeBonus(display, timeWindow, 180);
	DrawSpecials(display);

	event_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask |
	   ButtonReleaseMask | ExposureMask | StructureNotifyMask;
	XSelectInput(display, mainWindow, event_mask);

	event_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask |
	   ButtonReleaseMask;
	XSelectInput(display, playWindow, event_mask);

    /* Actually map the main window */
    XMapWindow(display, mainWindow);

	/* Install our new colormap into the server list */
	XInstallColormap(display, colormap);

	DEBUG("Entering main loop.")

	/* Return the new display */
	return display;
}
