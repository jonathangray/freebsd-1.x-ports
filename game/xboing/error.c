#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <xpm.h>

#if NeedFunctionPrototypes
extern int fprintf(FILE *stream, const char* format, ...);
extern int fflush(FILE *stream);
#else /* !NeedFunctionPrototypes */
extern int fprintf();
extern int fflush();
#endif /* NeedFunctionPrototypes */

#include "include/init.h"
#include "include/error.h"

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
void NormalMessage(char *message)
#else
void NormalMessage(message)
	char *message;
#endif
{
	/* Print a message to standard out flush it */
	fprintf(stdout, "XBoing - %s\n", message);
	fflush(stdout);
}

#if NeedFunctionPrototypes
void ErrorMessage(char *message)
#else
void ErrorMessage(message)
	char *message;
#endif
{

#ifndef READMEP_FILE
    /* Repeated to generate a message when compiling */
#define READMEP_FILE "README.problems"
#define READMEP_FILE "README.problems"
#endif
	/* Print a standard error message to stdout and flush it */
	fprintf(stdout, "XBoing - Error: %s\n", message);
	fprintf(stdout, "\nPlease read `%s' document for help.\n",
		READMEP_FILE);
	fflush(stdout);
}

#if NeedFunctionPrototypes
void WarningMessage(char *message)
#else
void WarningMessage(message)
	char *message;
#endif
{
	/* Print a standard warning message to stdout and flush it */
	fprintf(stdout, "XBoing - Warning: %s\n", message);
	fflush(stdout);
}

#if NeedFunctionPrototypes
void HandleXPMError(Display *display, int ErrorStatus, char *tag)
#else
void HandleXPMError(display, ErrorStatus, tag)
	Display *display;
	int ErrorStatus;
	char *tag;
#endif
{
    char *error = NULL;
	char *warning = NULL;

	/* Switch on the type of error returned by xpm library */
	switch (ErrorStatus) 
	{
		case XpmSuccess:
			return;

		case XpmColorError:
			/* The colour name passed was bung */
			warning = "Could not parse or alloc requested colour";
			break;

		case XpmNoMemory:
			/* Not enough memory for pixmap */
			error = "Not enough memory for pixmap creation";
			break;

		case XpmColorFailed:
			/* No more entries available in colourmap */
			error = "Colourmap is full - cannot allocate a colour";
			break;

		case XpmOpenFailed:
			/* Xpm could not open the pixmap file */
			error = "Unable to open pixmap file";
			break;

		case XpmFileInvalid:
			/* XPM file contains invalid or corrupt data */
			error = "XPM file contains invalid or corrupt data";
			break;

		default:
			/* Unexpected xpm error code */
			error = "Unexpected xpm error code";
			break;
	}

	/* If there is to be a warning then issue it */
    if (warning)
		fprintf(stdout, "%s - Warning: %s.\n", tag, warning);

	if (error) 
	{
		/* Argg. An error so tell everyone */
		fprintf(stderr, "%s - Error: %s.\n", tag, error);
		ShutDown(display, 1, "Fatal error.");
	}
}
