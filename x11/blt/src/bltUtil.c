/*
 * bltUtil.c --
 *
 *	This module implements utility procedures for the BLT
 *	toolkit.
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 */

#include "blt.h"


/*
 *----------------------------------------------------------------------
 *
 * Blt_FindCmd --
 *
 *      Given the name of a command, return a pointer to the
 *      clientData field of the command.
 *
 * Results:
 *      A standard TCL result. If the command is found, TCL_OK
 *	is returned and clientDataPtr points to the clientData
 *	field of the command (if the clientDataPtr in not NULL).
 *
 * Side effects:
 *      If the command is found, clientDataPtr is set to the address
 *	of the clientData of the command.  If not found, an error
 *	message is left in interp->result.
 *
 *----------------------------------------------------------------------
 */

int
Blt_FindCmd(interp, cmdName, clientDataPtr)
    Tcl_Interp *interp;		/* Interpreter in which to look. */
    char *cmdName;		/* Name of desired command */
    ClientData *clientDataPtr;
{
    Tcl_CmdInfo info;

    if (!Tcl_GetCommandInfo(interp, cmdName, &info)) {
	return TCL_ERROR;
    }
    if (clientDataPtr != NULL) {
	*clientDataPtr = info.clientData;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_OptionChanged --
 *
 *      Given the specs and option argument names (terminated by a NULL),
 *	search  a Tk_ConfigSpec structure for any matching specs which
 *      has been reset.
 *
 * Results:
 *      Returns 1 if one of the options has changed, 0 otherwise.
 *
 *----------------------------------------------------------------------
 */

int
Blt_OptionChanged(specs, opt0, opt1, opt2, opt3, opt4, opt5, opt6, opt7,
    opt8, opt9, opt10)
    Tk_ConfigSpec specs[];
    char *opt0, *opt1, *opt2, *opt3, *opt4, *opt5, *opt6, *opt7, *opt8;
    char *opt9, *opt10;
{
    register Tk_ConfigSpec *specPtr;
    char *argv[12];
    register char **argPtr;

    argv[0] = opt0;
    argv[1] = opt1;
    argv[2] = opt2;
    argv[3] = opt3;
    argv[4] = opt4;
    argv[5] = opt5;
    argv[6] = opt6;
    argv[7] = opt7;
    argv[8] = opt8;
    argv[9] = opt9;
    argv[10] = opt10;
    argv[11] = NULL;
    for (specPtr = specs; specPtr->type != TK_CONFIG_END; specPtr++) {
	for (argPtr = argv; *argPtr != NULL; argPtr++) {
	    if ((Tcl_StringMatch(specPtr->argvName, *argPtr)) &&
		(specPtr->specFlags & TK_CONFIG_OPTION_SPECIFIED)) {
		return 1;
	    }
	}
    }
    return 0;
}

#ifndef HAVE_STRDUP
/*
 *----------------------------------------------------------------------
 *
 * strdup --
 *
 *      Create a copy of the string from heap storage.
 *
 * Results:
 *      Returns a pointer to the need string copy.
 *
 *----------------------------------------------------------------------
 */
char *
strdup(string)
    char *string;
{
    char *newPtr;

    newPtr = (char *)malloc(sizeof(char) * (strlen(string) + 1));
    strcpy(newPtr, string);
    return (newPtr);
}

#endif /*HAVE_STRDUP*/

#ifndef HAVE_STRCASECMP

static unsigned char lcase[] =
{
    '\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
    '\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
    '\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
    '\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
    '\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
    '\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
    '\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
    '\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
    '\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
    '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
    '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
    '\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
    '\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
    '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
    '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
    '\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
    '\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
    '\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
    '\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
    '\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
    '\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
    '\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
    '\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
    '\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
    '\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
    '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
    '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
    '\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
    '\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
    '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
    '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
    '\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

/*
 *----------------------------------------------------------------------
 *
 * strcasecmp --
 *
 *      Compare two strings, disregarding case.
 *
 * Results:
 *      Returns the following:
 *
 *	zero      - two strings are equal
 *	negative  - first string is less than second
 *	positive  - first string is greater than second
 *
 *----------------------------------------------------------------------
 */
int
strcasecmp(str1, str2)
    CONST char *str1;
    CONST char *str2;
{
    register unsigned char *s = (unsigned char *)str1;
    register unsigned char *t = (unsigned char *)str2;

    for ( /* empty */ ; (lcase[*s] == lcase[*t]); s++, t++) {
	if (*s == '\0') {
	    return 0;
	}
    }
    return (lcase[*s] - lcase[*t]);
}

/*
 *----------------------------------------------------------------------
 *
 * strncasecmp --
 *
 *      Compare two strings, disregarding case, up to a given length.
 *
 * Results:
 *      Returns the following:
 *
 *	zero      - two strings are equal
 *	negative  - first string is less than second
 *	positive  - first string is greater than second
 *
 *----------------------------------------------------------------------
 */
int
strncasecmp(str1, str2, length)
    CONST char *str1;
    CONST char *str2;
    size_t length;
{
    register unsigned char *s = (unsigned char *)str1;
    register unsigned char *t = (unsigned char *)str2;

    for ( /* empty */ ; (length > 0); s++, t++, length--) {
	if (lcase[*s] != lcase[*t]) {
	    return (lcase[*s] - lcase[*t]);
	}
	if (*s == '\0') {
	    return 0;
	}
    }
    return 0;
}

#endif /*HAVE_STRCASECMP*/
