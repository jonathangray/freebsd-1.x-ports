/* userio.c -- handy user interface functions */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 21-May-86 | Created
*****************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "userio.h"

#ifndef true
#define true 1
#endif
#ifndef false
#define false 0
#endif

/****************************************************************************
*				askbool
* Inputs:
*	char *prompt: string to prompt for user input
*	int deflt: true or false default
* Returns:
*	boolean: true or false as entered by user
* Effect:
*	prompts user for yes or no input, returns result
****************************************************************************/

int askbool(prompt, deflt)
char *prompt;
int deflt;
{
#define undefined -1
    char defchar;		/* the default answer */
    char c;			/* user input */
    int result = -1;		/* the result: -1 = undefined, 0 = false, 1 = true */
    if (deflt)
	defchar = 'y';
    else
	defchar = 'n';
    while (result == undefined) {
	fprintf(stderr, "%s? [%c]: ", prompt, defchar);
	c = getchar();
	if (toupper(c) == 'Y')
	    result = true;
	else if (toupper(c) == 'N')
	    result = false;
	else if (c == '\n')
	    result = deflt;
	else
	    fprintf(stderr, "Please type Y or N.\n");
    }
    while (c != '\n')
	c = getchar();		/* flush the input line */
    return result;
}


/****************************************************************************
*				fileopen
* Inputs:
*	char *deflt: the default file name (e.g. from command line)
*	char *extension: default extension
*	char *mode: read ("r") or write ("w")
*	char *prompt: prompt for user
* Returns:
*	opened file pointer
* Effect:
*	opens file, prompts for user input if necessary and warns about
*	possible confusion.  If deflt is a null string, the user will
*	be prompted for a name.	 The routine loops until a file is opened.
*	If the mode is "r", a check is made to see if the file exists
*	with and without the extension.	 If both exist a warning is given.
*	For mode "w", a check is made to see if the file will be overwritten.
*	The extension is automatically added if the default or user-typed
*	file has no "."	 At the bottom of the loop body, if no file has
*	been opened, the user is prompted for another file name.
****************************************************************************/

FILE *fileopen(deflt, extension, mode, prompt)
char *deflt;
char *extension;		/* default extension */
char *mode;			/* read "r" or write "w" */
char *prompt;			/* prompt for user */
{
    char filename[100];		/* trial name */
    char extname[100];		/* trial name with extension added */
    FILE *fp = NULL;		/* file corresponding to filename */
    FILE *fpext;		/* file corresponding to extname */
    char *problem;		/* tells user why he has to try again */

    problem = "no problem";
    strcpy(filename, deflt);
    while (fp == NULL) {	/* keep trying until a good file is found */
	while (strlen(filename) == 0) {	/* avoid null file names */
	    fprintf(stderr, "%s: ", prompt);
	    fgets(filename, 99, stdin);
	    if (strlen(filename) > 0)
		filename[strlen(filename) - 1] = '\0';
	}
	if (mode[0] == 'r') {
	    strcpy(extname, filename);
	    strcat(extname, ".");
	    strcat(extname, extension);
	    fp = fopen(filename, mode);
	    if (fp != NULL)
		fpext = NULL;	/* added 1/30/93 --gl */
	    else
		fpext = fopen(extname, mode);
	    if (fp != NULL && fpext != NULL) {
		fprintf(stderr,
		    "warning: both %s and %s exist.	 %s will be used.\n",
			filename, extname, filename);
		fclose(fpext);
	    } else if (fpext != NULL) {
		fp = fpext;
	    }
	    if (fp == NULL)
		problem = "Couldn't find %s.";
	} else if (mode[0] == 'w') {
	    /* add the extension if there is no '.' in the file name */
	    if (!strchr(filename, '.')) {
		strcat(filename, ".");
		strcat(filename, extension);
	    }
	    fp = fopen(filename, "r");
	    if (fp != NULL) {
		char question[100];
		fclose(fp);
		strcpy(question, "OK to overwrite ");
		strcat(question, filename);
		if (askbool(question, false)) {
		    fp = fopen(filename, mode);
		} else {
		    fp = NULL;
		    problem = "";
		}
	    } else {
		fp = fopen(filename, mode);
		if (fp == NULL)
		    problem = "Couldn't create %s.";
	    }
	}
	if (fp == NULL) {
	    fprintf(stderr, problem, filename);
	    fprintf(stderr, "  Try again.\n%s: ", prompt);
	    fgets(filename, 99, stdin);
	    if (strlen(filename) > 0)
		filename[strlen(filename) - 1] = '\0';
	}
    }
    strcpy(midi_file_path, filename);
    return fp;
}

/****************************************************************************
*				    readln
* Inputs:
*	FILE * fp: File to read from
* Effect:
*	Reads and discards characters until a newline is seen
****************************************************************************/

void readln(fp)
FILE *fp;
{
    while (getc(fp) != '\n');
}
