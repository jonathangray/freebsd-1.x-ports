/* These routines report on transitions between word boundaries, both 
 *	in the punctuated vi sense, and in the whitespace/darkspace
 *	sense.  The transition is reported _after_ it has occurred.  You
 *	need to back up to get to the char. before the transition.
 *	Written for vile by Paul Fox, (c)1990
 *
 * $Log: wordmov.c,v $
 * Revision 1.1  1994/02/01 03:29:44  jkh
 * Initial revision
 *
 * Revision 1.6  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.5  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.4  1991/11/08  13:02:46  pgf
 * ifdefed unneeded funcs
 *
 * Revision 1.3  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.2
 * date: 1991/06/25 19:53:49;
 * massive data structure restructure
 * 
 * revision 1.1
 * date: 1990/09/21 10:26:27;
 * initial vile RCS revision
 */

#include "estruct.h"
#include "edef.h"


#define WASSPACE 0
#define ISSPACE 0
#define WASIDENT 1
#define ISIDENT 1
#define WASOTHER 2
#define ISOTHER 2
#define WASNL 3
#define ISNL 3

static int ochartype;

void
setchartype()
{
	ochartype = getchartype();
}

int
getchartype()
{
	register int	c;

	if (is_at_end_of_line(DOT))
		return (ISNL);
	else
		c = char_at(DOT);
	return (isspace(c) ? ISSPACE : 
			( isident(c) ? ISIDENT : ISOTHER ) );
}


int
isnewwordf()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		switch (type) {
		case ISNL:    if (doingopcmd) { ret = SORTOFTRUE; break;}
		case ISSPACE: ret = FALSE;	break;
		case ISIDENT:
		case ISOTHER: ret = TRUE;	break;
		}
		break;
	case WASIDENT:
	case WASOTHER:
		switch (type) {
		case ISNL:    if (doingopcmd) { ret = SORTOFTRUE; break;}
		case ISSPACE: if (doingopcmd && opcmd != OPDEL) 
						{ ret = SORTOFTRUE; break;}
		case ISIDENT:
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}

int
isnewwordb()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		ret = FALSE;	break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE: ret = TRUE;	break;
		case ISIDENT:
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE: ret = TRUE;	break;
		case ISIDENT:
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}

int
isnewviwordf()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		switch (type) {
		case ISNL:    if (doingopcmd) { ret = SORTOFTRUE; break;}
		case ISSPACE: ret = FALSE;	break;
		case ISIDENT:
		case ISOTHER: ret = TRUE;	break;
		}
		break;
	case WASIDENT:
		switch (type) {
		case ISNL:    if (doingopcmd) { ret = SORTOFTRUE; break;}
		case ISSPACE: if (doingopcmd && opcmd != OPDEL) 
						{ ret = SORTOFTRUE; break;}
		case ISIDENT: ret = FALSE;	break;
		case ISOTHER: ret = TRUE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:    if (doingopcmd) { ret = SORTOFTRUE; break;}
		case ISSPACE: if (doingopcmd && opcmd != OPDEL) 
						{ ret = SORTOFTRUE; break;}
		case ISOTHER: ret = FALSE;	break;
		case ISIDENT: ret = TRUE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}

int
isnewviwordb()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		ret = FALSE;	break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISOTHER: ret = TRUE;	break;
		case ISIDENT: ret = FALSE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISIDENT: ret = TRUE;	break;
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}


#ifdef NEEDED
int
isendwordb()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		switch (type) {
		case ISNL:
		case ISSPACE: ret = FALSE;	break;
		case ISIDENT:
		case ISOTHER: ret = TRUE;	break;
		}
		break;
	case WASIDENT:
	case WASOTHER:
		ret = FALSE;	break;
	}
	ochartype = type;
	return (ret);
}

int
isendviwordb()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		switch (type) {
		case ISNL:
		case ISSPACE: ret = FALSE;	break;
		case ISOTHER:
		case ISIDENT: ret = TRUE;	break;
		}
		break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISOTHER: ret = TRUE;	break;
		case ISIDENT: ret = FALSE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISIDENT: ret = TRUE;	break;
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}
#endif /* NEEDED */

int
isendwordf()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		ret = FALSE;	break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE:
			if (doingopcmd) ret = SORTOFTRUE;
			else ret = TRUE;
			break;
		case ISIDENT:
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE:
			if (doingopcmd) ret = SORTOFTRUE;
			else ret = TRUE;
			break;
		case ISIDENT:
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}

int
isendviwordf()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		ret = FALSE;	break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISOTHER:
			if (doingopcmd) ret = SORTOFTRUE;
			else ret = TRUE;
			break;
		case ISIDENT: ret = FALSE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE:
		case ISIDENT:
			if (doingopcmd) ret = SORTOFTRUE;
			else ret = TRUE;
			break;
		case ISOTHER: ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}

#ifdef template
int
isANYTHING()
{
	register int	ret = FALSE;
	register int	type;

	type = getchartype();

	switch (ochartype) {
	case WASNL:
	case WASSPACE:
		switch (type) {
		case ISNL:
		case ISSPACE:
			ret = FALSE;	break;
		case ISIDENT:
			ret = FALSE;	break;
		case ISOTHER:
			ret = FALSE;	break;
		}
		break;
	case WASIDENT:
		switch (type) {
		case ISNL:
		case ISSPACE:
			ret = TRUE;	break;
		case ISIDENT:
			ret = FALSE;	break;
		case ISOTHER:
			ret = TRUE;	break;
		}
		break;
	case WASOTHER:
		switch (type) {
		case ISNL:
		case ISSPACE:
			ret = TRUE;	break;
		case ISIDENT:
			ret = TRUE;	break;
		case ISOTHER:
			ret = FALSE;	break;
		}
		break;
	}
	ochartype = type;
	return (ret);
}
#endif /* template */

