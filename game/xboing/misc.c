#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <pwd.h>
#include <netinet/in.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#if NeedFunctionPrototypes
extern int toupper(int c);
#else /* !NeedFunctionPrototypes */
extern char toupper();
#endif /* NeedFunctionPrototypes */

#include "include/init.h"
#include "include/error.h"
#include "include/stage.h"

#include "include/misc.h"

/*
 *  Internal macro definitions:
 */

/*
 *  Internal type declarations:
 */

/*
 *  Internal variable declarations:
 */

#if NeedFunctionPrototypes
void DrawLine(Display *display, Window window, int x, int y, int x2, int y2, 
	int colour, int width)
#else
void DrawLine(display, window, x, y, x2, y2, colour, width)
	Display *display;
	Window window;
	int x;
	int y;
	int x2;
	int y2; 
	int colour;
	int width;
#endif
{
	/* Change the width of the line */
	XSetLineAttributes(display, gcxor, width, LineSolid, CapProjecting, 
		JoinMiter);
	XSetLineAttributes(display, gcand, width, LineSolid, CapProjecting, 
		JoinMiter);

	/* Set to the desired colours */
	XSetBackground(display, gcxor, colour);
	XSetForeground(display, gcxor, colour);

	/* Now draw the line */
	XDrawLine(display, window, gcxor, x, y, x2, y2);
	XDrawLine(display, window, gcand, x, y, x2, y2);
	XDrawLine(display, window, gcxor, x, y, x2, y2);
}

#if NeedFunctionPrototypes
void DrawShadowCentredText(Display *display, Window window, XFontStruct *font,
	char *string, int y, int colour, int width)
#else
void DrawShadowCentredText(display, window, font, string, y, colour, width)
	Display *display;
	Window window;
	XFontStruct *font;
	char *string;
	int y;
	int colour;
	int width;
#endif
{
    int plen, len, x;

	/* String length */
    len = strlen(string);

	/* Length of string in pixels */
    plen = XTextWidth(font, string, len);

	/* Start drawing so the text is centered */
    x = (width / 2) - (plen / 2);

	/* Draw the text with a shadow */
    DrawText(display, window, x+2, y + 2, font, black, string, -1);
    DrawText(display, window, x, y, font, colour, string, -1);
}

#if NeedFunctionPrototypes
void DrawShadowText(Display *display, Window window, XFontStruct *font,
	char *string, int x, int y, int colour)
#else
void DrawShadowText(display, window, font, string, x, y, colour)
	Display *display;
	Window window;
	XFontStruct *font;
	char *string;
	int x;
	int y;
	int colour;
#endif
{
    int len;

	/* String length */
    len = strlen(string);

	/* Draw the text with a shadow */
    DrawText(display, window, x+2, y + 2, font, black, string, -1);
    DrawText(display, window, x, y, font, colour, string, -1);
}

#if NeedFunctionPrototypes
void DrawText(Display *display, Window window, int x, int y, XFontStruct *font, 
	int colour, char *text, int numChar)
#else
void DrawText(display, window, x, y, font, colour, text, numChar)
	Display *display;
	Window window;
	int x;
	int y;
	XFontStruct *font;
	int colour;
	char *text;
	int numChar;
#endif
{
	int len = strlen(text);

	/* If numchar is passed > 0 then only draw numChar characters */
	if (numChar > 0) 
		len = numChar;

	/* Change to the new font */
	XSetFont(display, gcxor, font->fid);
	XSetFont(display, gcand, font->fid);

	/* Change the drawing function */
	XSetBackground(display, gcxor, colour);
	XSetForeground(display, gcxor, colour);

	/* Draw the string into the drawable */
	XDrawString(display, window, gcxor, x, y + font->ascent, text, len);
	XDrawString(display, window, gcand, x, y + font->ascent, text, len);
	XDrawString(display, window, gcxor, x, y + font->ascent, text, len);
}

#if NeedFunctionPrototypes
void RenderShape(Display *display, Window window, Pixmap pixmap, 
	Pixmap mask, int x, int y, int w, int h, int clear)
#else
void RenderShape(display, window, pixmap, mask, x, y, w, h, clear)
	Display *display;
	Window window;
	Pixmap pixmap;
	Pixmap mask;
	int x;
	int y;
	int w;
	int h;
	int clear;
#endif
{
	/* Clear the background first? */
    if (clear) XClearArea(display, window, x, y, w, h, False);

	/* Set to dest x and y clip origin */
    XSetClipOrigin(display, gc, x, y); 

	/* Set the clipping mask */
    XSetClipMask(display, gc, mask);   

    XCopyArea(display, pixmap, window, gc, 0, 0, w, h, x, y);

	/* Unset clip (or add a clip gc) */
    XSetClipMask(display, gc, None);   
}

#if NeedFunctionPrototypes
void FreeMisc(Display *display)
#else
void FreeMisc(display)
    Display *display;
#endif
{
}

#if NeedFunctionPrototypes
int ColourNameToPixel(Display *display, Colormap colormap, char *colourName)
#else
int ColourNameToPixel(display, colormap, colourName)
    Display *display;
    Colormap colormap;
    char *colourName;
#endif
{
    XColor colour;

    /* Obtain the exact colour from the colour name */
    if (XParseColor(display, DefaultColormap(display,
        XDefaultScreen(display)), colourName, &colour) != 0)
    {
        /* Now allocate the colour */
        if (XAllocColor(display, colormap, &colour) != 0)
		{
        	/* Success - return the pixel id */
        	return colour.pixel;
		}
    }

    /* Obviously a problem so barf */
    ShutDown(display, 1, "Error while parsing colours.");

    /* NOT REACHED */
    return 1;
}


#if NeedFunctionPrototypes
char *getUsersFullName(void)
#else
char *getUsersFullName()
#endif
{
    struct passwd *pass;
    char *comma;
    char *cp1, *cp2;
    static char fullname[80];

    /* Get user information from password file */
    if (!(pass = getpwuid(getuid())))
        return("Anonymous?");       /* Unknown user oops. */

    /* find a comma indicating further info after name */
    comma = strchr(pass->pw_gecos, ',');

    /* NULL out the comma */
    if (comma) *comma = '\0';

    /* Use the nickname if not null otherwise password file name */
    cp1 = pass->pw_gecos;
    cp2 = fullname;

    /* Search through the gecos field looking for an '&' which on very
     * old UNIX systems is supposed to be the users user name with the
     * first letter uppercased.
     */
    while(*cp1)
    {
        /* Look for the '&' symbol */
        if(*cp1 != '&')
            *cp2++ = *cp1++;
        else
        {
            /* A ha. Now copy the users name to be in place of '&' */
            strcpy(cp2, pass->pw_name);
       
            /* Convert the first letter to uppercase. */
            if(islower(*cp2))
                *cp2 = toupper(*cp2);

            /* Continue with the remaining string */
            while(*cp2) cp2++;
                cp1++;
        }
    }

    /* Return their name without any trailing stuff */
    return(fullname);
}

#if NeedFunctionPrototypes
char *GetHomeDir(void)
#else
char *GetHomeDir()
#endif
{
    int uid;
    struct passwd *pw;
    register char *ptr;
    static char dest[MAXPATHLEN];

    /* This function will return the home directory of the user
     * by either using the HOME environment variable or constructing
     * it through the USER environment variable.
     */

    if ((ptr = getenv("HOME")) != NULL)
        (void) strcpy(dest, ptr);
    else
    {
        /* HOME variable is not present so get USER var */
        if ((ptr = getenv("USER")) != NULL)
            pw = getpwnam(ptr);
        else
        {
            /* Obtain user id etc. */
            uid = getuid();
            pw = getpwuid(uid);
        }

        if (pw)
            (void) strcpy(dest, pw->pw_dir);
        else
            *dest = '\0';
    }

    /* This will be NULL on error or the actual path */
    return dest;
}

