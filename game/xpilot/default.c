/* $Id: default.c,v 1.1 1994/02/23 14:40:05 jkh Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-93 by
 *
 *      Bjørn Stabell        (bjoerns@staff.cs.uit.no)
 *      Ken Ronny Schouten   (kenrsc@stud.cs.uit.no)
 *      Bert Gÿsbers         (bert@mc.bio.uva.nl)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "types.h"
#include <X11/Xos.h>
#include <X11/keysym.h>
#ifdef	__apollo
#    include <X11/ap_keysym.h>
#endif
#include <X11/Intrinsic.h>
#include <sys/types.h>
#ifdef VMS
#include "strcasecmp.h"
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#include <sys/param.h>
#endif
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "version.h"
#include "config.h"
#include "const.h"
#include "client.h"
#include "paint.h"
#include "draw.h"
#include "pack.h"
#include "bit.h"
#include "netclient.h"
#include "xinit.h"
#include "error.h"

#ifndef	lint
static char sourceid[] =
    "@(#)$Id: default.c,v 1.1 1994/02/23 14:40:05 jkh Exp $";
#endif

#ifndef PATH_MAX
#define PATH_MAX	1024
#endif

/*
 * Default fonts
 */
#define GAME_FONT		"-*-times-*-*-*-*-18-*-*-*-*-*-iso8859-1"
#define MESSAGE_FONT		"-*-times-*-*-*-*-14-*-*-*-*-*-iso8859-1"
#define SCORE_LIST_FONT		"-*-fixed-bold-*-*-*-15-*-*-*-c-*-iso8859-1"
#define BUTTON_FONT		"-*-*-bold-o-*-*-14-*-*-*-*-*-iso8859-1"
#define TEXT_FONT		"-*-*-bold-i-*-*-14-*-*-*-p-*-iso8859-1"
#define TALK_FONT		"-*-fixed-bold-*-*-*-15-*-*-*-c-*-iso8859-1"

static struct _keyResources
{
    char	*resource;
    char	*fallback;
    keys_t	key;
    char	*helpLine;
} keyResources[] = {
    { "keyTurnLeft",			"a",
	KEY_TURN_LEFT,			"Turn left (anti-clockwise)" },
    { "keyTurnRight",			"s",
	KEY_TURN_RIGHT,			"Turn right (clockwise)" },
    { "keyThrust",			"Shift_R Shift_L",
	KEY_THRUST,			"Thrust" },
    { "keyShield",			"space Meta_R",
	KEY_SHIELD,			"Raise shield" },
    { "keyFireShot",			"Return Linefeed",
	KEY_FIRE_SHOT,			"Fire shot" },
    { "keyFireMissile",			"backslash",
	KEY_FIRE_MISSILE,		"Fire smart missile" },
    { "keyFireTorpedo",			"quoteright",
	KEY_FIRE_TORPEDO,		"Fire unguided torpedo" },
    { "keyFireHeat",			"semicolon",
	KEY_FIRE_HEAT,			"Fire heat seaking missile" },
    { "keyFireNuke",			"n",
	KEY_FIRE_NUKE,			"Fire nuclear missile" },
    { "keyFireLaser",			"slash",
	KEY_FIRE_LASER,			"Activate laser beam" },
    { "keyDropMine",			"Tab",
	KEY_DROP_MINE,			"Drop a mine (bomb)" },
    { "keyDetachMine",			"bracketright",
	KEY_DETACH_MINE,		"Detach a mine" },
    { "keyLockClose",			"Select Up Down",
	KEY_LOCK_CLOSE,			"Lock on closest player" },
    { "keyLockNext",			"Next Right",
	KEY_LOCK_NEXT,			"Lock on next player" },
    { "keyLockPrev",			"Prior Left",
	KEY_LOCK_PREV,			"Lock on previous player" },
    { "keyRefuel",			"f Control_L Control_R Caps_Lock",
	KEY_REFUEL,			"Refuel" },
    { "keyRepair",			"x",
	KEY_REPAIR,			"Repair target" },
    { "keyCloak",			"Delete BackSpace",
	KEY_CLOAK,			"Toggle cloakdevice" },
    { "keyEcm",				"bracketleft",
	KEY_ECM,			"Use ECM" },
    { "keySelfDestruct",		"q",
	KEY_SELF_DESTRUCT,		"Toggle self destruct" },
    { "keyIdMode",			"i",
	KEY_ID_MODE,			"Toggle ID mode (show realname)" },
    { "keyPause",			"p Pause",
	KEY_PAUSE,			"Toggle pause mode" },
    { "keySwapSettings",		"Escape",
	KEY_SWAP_SETTINGS,		"Swap control settings" },
    { "keyChangeHome",			"Home h",
	KEY_CHANGE_HOME,		"Change home base" },
    { "keyConnector",			"Control_L",
	KEY_CONNECTOR,			"Use connector (pick up ball)" },
    { "keyDropBall",			"d",
	KEY_DROP_BALL,			"Drop a ball" },
    { "keyTankNext",			"e",
	KEY_TANK_NEXT,			"Shift to next tank" },
    { "keyTankPrev",			"w",
	KEY_TANK_PREV,			"Shift to previous tank" },
    { "keyTankDetach",			"r",
	KEY_TANK_DETACH,		"Detach tank" },
    { "keyIncreasePower",		"KP_Multiply KP_F1",
	KEY_INCREASE_POWER,		"Increase power" },
    { "keyDecreasePower",		"KP_Divide KP_F2",
	KEY_DECREASE_POWER,		"Decrease power" },
    { "keyIncreaseTurnspeed",		"KP_Add KP_F3",
	KEY_INCREASE_TURNSPEED,		"Increase turnspeed" },
    { "keyDecreaseTurnspeed",		"KP_Subtract KP_F4",
	KEY_DECREASE_TURNSPEED,		"Decrease turnspeed" },
    { "keyTransporter",			"t",
	KEY_TRANSPORTER,		"Use transporter" },
    { "keyTalk",			"m",
	KEY_TALK,			"Enable/disable talk window" },
    { "keyToggleCompass",		"c",
	KEY_TOGGLE_COMPASS,		"Enable/disable locking on players" }
};


static XrmOptionDescRec opts[] = {
    { "-help",				".help",
    	XrmoptionNoArg,				"True" },
    { "-version",			".version",
    	XrmoptionNoArg,				"True" },
    { "-name",				".name",
    	XrmoptionSepArg,			NULL },
    { "-join",				".join",
	XrmoptionNoArg,				"True" },
    { "-motd",				".motd",
	XrmoptionNoArg,				"True" },
    { "-list",				".list",
	XrmoptionNoArg,				"True" },
    { "-team",				".team",
	XrmoptionSepArg,			NULL },
    { "-display",			".display",
    	XrmoptionSepArg,			NULL },
    { "-geometry",			".geometry",
    	XrmoptionSepArg,			NULL },
    { "-shutdown",			".shutdown",
    	XrmoptionSepArg,			NULL },
    { "-port",				".port",
    	XrmoptionSepArg,			NULL },
#ifdef SOUND
    { "-sounds",			".sounds",
    	XrmoptionSepArg,			NULL },
    { "-maxVolume",			".maxVolume",
    	XrmoptionSepArg,			NULL },
    { "-audioServer",			".audioServer",
    	XrmoptionSepArg,			NULL },
#endif
    { "-power",				".power",
    	XrmoptionSepArg,			NULL },
    { "-turnspeed",			".turnspeed",
    	XrmoptionSepArg,			NULL },
    { "-turnresistance",		".turnresistance",
    	XrmoptionSepArg,			NULL },
    { "-altPower",			".altPower",
    	XrmoptionSepArg,			NULL },
    { "-altTurnSpeed",			".altTurnSpeed",
    	XrmoptionSepArg,			NULL },
    { "-altTurnResistance",		".altTurnResistance",
    	XrmoptionSepArg,			NULL },
    { "-fuelNotify",			".fuelNotify",
    	XrmoptionSepArg,			NULL },
    { "-fuelWarning",			".fuelWarning",
    	XrmoptionSepArg,			NULL },
    { "-fuelCritical",			".fuelCritical",
    	XrmoptionSepArg,			NULL },
    { "-showHUD",			".showHUD",
    	XrmoptionSepArg,			NULL },
    { "-verticalHUDLine",		".verticalHUDLine",
    	XrmoptionSepArg,			NULL },
    { "-horizontalHUDLine",		".horizontalHUDLine",
    	XrmoptionSepArg,			NULL },
    { "-speedFactHUD",			".speedFactHUD",
    	XrmoptionSepArg,			NULL },
    { "-speedFactPTR",			".speedFactPTR",
    	XrmoptionSepArg,			NULL },
    { "-fuelMeter",			".fuelMeter",
    	XrmoptionSepArg,			NULL },
    { "-fuelGauge",			".fuelGauge",
    	XrmoptionSepArg,			NULL },
    { "-turnSpeedMeter",		".turnSpeedMeter",
    	XrmoptionSepArg,			NULL },
    { "-powerMeter",			".powerMeter",
    	XrmoptionSepArg,			NULL },
    { "-packetSizeMeter",		".packetSizeMeter",
    	XrmoptionSepArg,			NULL },
    { "-packetLossMeter",		".packetLossMeter",
    	XrmoptionSepArg,			NULL },
    { "-packetDropMeter",		".packetDropMeter",
    	XrmoptionSepArg,			NULL },
    { "-slidingRadar",			".slidingRadar",
    	XrmoptionSepArg,			NULL },
    { "-outlineWorld",			".outlineWorld",
    	XrmoptionSepArg,			NULL },
    { "-markingLights",			".markingLights",
    	XrmoptionSepArg,			NULL },
    { "-clock",				".clock",
    	XrmoptionSepArg,			NULL },
    { "-gameFont",			".gameFont",
    	XrmoptionSepArg,			NULL },
    { "-scoreListFont",			".scoreListFont",
    	XrmoptionSepArg,			NULL },
    { "-buttonFont",			".buttonFont",
    	XrmoptionSepArg,			NULL },
    { "-textFont",			".textFont",
    	XrmoptionSepArg,			NULL },
    { "-talkFont",			".talkFont",
        XrmoptionSepArg,			NULL },
    { "-backGroundPointDist",		".backGroundPointDist",
	XrmoptionSepArg,			NULL },
    { "-receiveWindowSize",		".receiveWindowSize",
    	XrmoptionSepArg,			NULL },
    { "-visual",			".visual",
    	XrmoptionSepArg,			NULL },
    { "-mono",				".mono",
    	XrmoptionNoArg,				"True" },
    { "-colorSwitch",			".colorSwitch",
    	XrmoptionSepArg,			NULL },
    { "-maxColors",			".maxColors",
    	XrmoptionSepArg,			NULL },
    { "-black",				".black",
    	XrmoptionSepArg,			NULL },
    { "-white",				".white",
    	XrmoptionSepArg,			NULL },
    { "-blue",				".blue",
    	XrmoptionSepArg,			NULL },
    { "-red",				".red",
    	XrmoptionSepArg,			NULL },
    { "-color0",			".color0",
    	XrmoptionSepArg,			NULL },
    { "-color1",			".color1",
    	XrmoptionSepArg,			NULL },
    { "-color2",			".color2",
    	XrmoptionSepArg,			NULL },
    { "-color3",			".color3",
    	XrmoptionSepArg,			NULL },
    { "-color4",			".color4",
    	XrmoptionSepArg,			NULL },
    { "-color5",			".color5",
    	XrmoptionSepArg,			NULL },
    { "-color6",			".color6",
    	XrmoptionSepArg,			NULL },
    { "-color7",			".color7",
    	XrmoptionSepArg,			NULL },
    { "-color8",			".color8",
    	XrmoptionSepArg,			NULL },
    { "-color9",			".color9",
    	XrmoptionSepArg,			NULL },
    { "-color10",			".color10",
    	XrmoptionSepArg,			NULL },
    { "-color11",			".color11",
    	XrmoptionSepArg,			NULL },
    { "-color12",			".color12",
    	XrmoptionSepArg,			NULL },
    { "-color13",			".color13",
    	XrmoptionSepArg,			NULL },
    { "-color14",			".color14",
    	XrmoptionSepArg,			NULL },
    { "-color15",			".color15",
    	XrmoptionSepArg,			NULL },
    { "-keyTurnLeft",			".keyTurnLeft",
	XrmoptionSepArg,			NULL },
    { "-keyTurnRight",			".keyTurnRight",
	XrmoptionSepArg,			NULL },
    { "-keyThrust",			".keyThrust",
	XrmoptionSepArg,			NULL },
    { "-keyShield",			".keyShield",
	XrmoptionSepArg,			NULL },
    { "-keyFireShot",			".keyFireShot",
	XrmoptionSepArg,			NULL },
    { "-keyFireMissile",		".keyFireMissile",
	XrmoptionSepArg,			NULL },
    { "-keyFireTorpedo",		".keyFireTorpedo",
	XrmoptionSepArg,			NULL },
    { "-keyFireHeat",			".keyFireHeat",
	XrmoptionSepArg,			NULL },
    { "-keyFireNuke",			".keyFireNuke",
	XrmoptionSepArg,			NULL },
    { "-keyFireLaser",			".keyFireLaser",
	XrmoptionSepArg,			NULL },
    { "-keyDropMine",			".keyDropMine",
	XrmoptionSepArg,			NULL },
    { "-keyDetachMine",			".keyDetachMine",
	XrmoptionSepArg,			NULL },
    { "-keyLockClose",			".keyLockClose",
	XrmoptionSepArg,			NULL },
    { "-keyLockNext",			".keyLockNext",
	XrmoptionSepArg,			NULL },
    { "-keyLockPrev",			".keyLockPrev",
	XrmoptionSepArg,			NULL },
    { "-keyRefuel",			".keyRefuel",
	XrmoptionSepArg,			NULL },
    { "-keyCloak",			".keyCloak",
	XrmoptionSepArg,			NULL },
    { "-keyEcm",			".keyEcm",
	XrmoptionSepArg,			NULL },
    { "-keySelfDestruct",		".keySelfDestruct",
	XrmoptionSepArg,			NULL },
    { "-keyIdMode",			".keyIdMode",
	XrmoptionSepArg,			NULL },
    { "-keyPause",			".keyPause",
	XrmoptionSepArg,			NULL },
    { "-keySwapSettings",		".keySwapSettings",
	XrmoptionSepArg,			NULL },
    { "-keyChangeHome",			".keyChangeHome",
	XrmoptionSepArg,			NULL },
    { "-keyConnector",			".keyConnector",
	XrmoptionSepArg,			NULL },
    { "-keyDropBall",			".keyDropBall",
	XrmoptionSepArg,			NULL },
    { "-keyTankNext",			".keyTankNext",
	XrmoptionSepArg,			NULL },
    { "-keyTankPrev",			".keyTankPrev",
	XrmoptionSepArg,			NULL },
    { "-keyTankDetach",			".keyTankDetach",
	XrmoptionSepArg,			NULL },
    { "-keyIncreasePower",		".keyIncreasePower",
	XrmoptionSepArg,			NULL },
    { "-keyDecreasePower",		".keyDecreasePower",
	XrmoptionSepArg,			NULL },
    { "-keyIncreaseTurnspeed",		".keyIncreaseTurnspeed",
	XrmoptionSepArg,			NULL },
    { "-keyDecreaseTurnspeed",		".keyDecreaseTurnspeed",
	XrmoptionSepArg,			NULL },
    { "-keyTransporter",		".keyTransporter",
	XrmoptionSepArg,			NULL },
    { "-keyTalk",			".keyTalk",
	XrmoptionSepArg,			NULL },
    { "-keyToggleVelocity",		".keyToggleVelocity",
	XrmoptionSepArg,			NULL },
    { "-keyToggleCompass",		".keyToggleCompass",
	XrmoptionSepArg,			NULL },
    { "-toggleShield",			".toggleShield",
	XrmoptionSepArg,			NULL }
};


static int ON(char *optval)
{
    return (strncasecmp(optval, "true", 4) == 0
	    || strncasecmp(optval, "on", 2) == 0
	    || strncasecmp(optval, "yes", 3) == 0);
}


char* Get_keyhelpstring(keys_t key)
{
    int i;
    char* str = NULL;

    for (i = 0; i < NELEM(keyResources); i++)
	if (keyResources[i].key == key) {
	    str = keyResources[i].helpLine;
	    break;
	}

    return str;
}


char* Get_keyResourceString(keys_t key)
{
    int i;
    char* str = NULL;

    for (i = 0; i < NELEM(keyResources); i++)
	if (keyResources[i].key == key) {
	    str = keyResources[i].resource;
	    break;
	}

    return str;
}


void Usage(void)
{
    int			i;

    printf("Usage: xpilot [-options ...] [server]\n");
    printf("Where options include:\n");
    for (i = 0; i < NELEM(opts); i++) {
	printf("\t%s%s\n", opts[i].option,
	       (opts[i].argKind == XrmoptionSepArg) ?  " <value>" : "");
    }
    printf("If no server is specified then xpilot will search\n");
    printf("for servers on your local network\n");
    printf("Try: `telnet xpilot.cs.uit.no 4400' to see some remote servers\n");

    exit(1);
}


static int Get_resource(XrmDatabase db, char *myName, char *myClass,
			char *resource, char *fallback, char *result,
			unsigned size)
{
    int			i,
			len;
    char		str_name[80],
			str_class[80],
			*str_type[10];
    XrmValue		rmValue;

    sprintf(str_name, "%s.%s", myName, resource);
    sprintf(str_class, "%s.%c%s", myClass,
	    isupper(*resource) ? toupper(*resource) : *resource, resource + 1);

    if (XrmGetResource(db, str_name, str_class, str_type, &rmValue) == True) {
	if (rmValue.addr == NULL) {
	    len = 0;
	} else {
	    len = MIN(rmValue.size, size - 1);
	    strncpy(result, rmValue.addr, len);
	}
	result[len] = '\0';
	for (i = 0; i < NELEM(opts); i++) {
	    if (opts[i].argKind == XrmoptionIsArg
		&& (strcmp(result, opts[i].option) == 0
		    || strcmp(result, opts[i].specifier) == 0)) {
		strncpy(result, "True", size);
		result[size - 1] = '\0';
		break;
	    }
	}
	return 1;
    }
    if (fallback != NULL) {
	strncpy(result, fallback, size - 1);
	result[size - 1] = '\0';
    } else {
	result[0] = '\0';
    }
    return 0;
}


static int Get_string_resource(XrmDatabase db, char *myName, char *myClass,
			       char *resource, char *fallback, char *result,
			       unsigned size)
{
    char		*src, *dst;
    int			val;

    val = Get_resource(db, myName, myClass, resource, fallback, result, size);
    src = dst = result;
    while ((*src & 0x7f) == *src && isgraph(*src) == 0 && *src != '\0') {
	src++;
    }
    while ((*src & 0x7f) != *src || isgraph(*src) != 0) {
	*dst++ = *src++;
    }
    *dst = '\0';

    return val;
}


static void Get_float_resource(XrmDatabase db, char *myName, char *myClass,
			       char *resource, float fallback, float *result)
{
    char		resValue[MAX_CHARS];

    if (Get_resource(db, myName, myClass, resource, NULL, resValue,
		     sizeof resValue) == 0
	|| sscanf(resValue, "%f", result) <= 0) {
	*result = fallback;
    }
}


static void Get_bool_resource(XrmDatabase db, char *myName, char *myClass,
			      char *resource, char *fallback, int *result)
{
    char		resValue[MAX_CHARS];

    Get_resource(db, myName, myClass, resource, fallback, resValue,
		 sizeof resValue);
    *result = (ON(resValue) != 0);
}


static void Get_int_resource(XrmDatabase db, char *myName, char *myClass,
			     char *resource, int fallback, int *result)
{
    char		resValue[MAX_CHARS];

    if (Get_resource(db, myName, myClass, resource, NULL, resValue,
		     sizeof resValue) == 0
	|| sscanf(resValue, "%d", result) <= 0) {
	*result = fallback;
    }
}


static void Get_file_defaults(XrmDatabase *rDBptr,
			      char *myName, char *myClass)
{
#ifdef VMS
#define MAXPATHLEN 1024
#endif
    int			len;
    char		*ptr,
			*lang = getenv("LANG"),
			*home = getenv("HOME"),
			path[MAXPATHLEN];
    XrmDatabase		tmpDB;

    sprintf(path, "%s%s", LIBDIR, myClass);
    *rDBptr = XrmGetFileDatabase(path);

#ifdef VMS
    /*
     * None of the paths generated will be valid VMS file names.
     */
    tmpDB = XrmGetFileDatabase("SYS$LOGIN:decw$xdefaults.dat");
    XrmMergeDatabases(tmpDB, rDBptr);
    tmpDB = XrmGetFileDatabase("DECW$USER_DEFAULTS:xpilot.dat");
    XrmMergeDatabases(tmpDB, rDBptr);
#else
    if (lang != NULL) {
	sprintf(path, "/usr/lib/X11/%s/app-defaults/%s", lang, myClass);
	if (access(path, 0) == -1) {
	    sprintf(path, "/usr/lib/X11/app-defaults/%s", myClass);
	}
    } else {
	sprintf(path, "/usr/lib/X11/app-defaults/%s", myClass);
    }
    tmpDB = XrmGetFileDatabase(path);
    XrmMergeDatabases(tmpDB, rDBptr);

    if ((ptr = getenv("XUSERFILESEARCHPATH")) != NULL) {
	sprintf(path, "%s/%s", ptr, myClass);
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }
    else if ((ptr = getenv("XAPPLRESDIR")) != NULL) {
	if (lang != NULL) {
	    sprintf(path, "%s/%s/%s", ptr, lang, myClass);
	    if (access(path, 0) == -1) {
		sprintf(path, "%s/%s", ptr, myClass);
	    }
	} else {
	    sprintf(path, "%s/%s", ptr, myClass);
	}
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }
    else if (home != NULL) {
	if (lang != NULL) {
	    sprintf(path, "%s/app-defaults/%s/%s", home, lang, myClass);
	    if (access(path, 0) == -1) {
		sprintf(path, "%s/app-defaults/%s", home, myClass);
	    }
	} else {
	    sprintf(path, "%s/app-defaults/%s", home, myClass);
	}
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }

    if ((ptr = XResourceManagerString(dpy)) != NULL) {
	tmpDB = XrmGetStringDatabase(ptr);
	XrmMergeDatabases(tmpDB, rDBptr);
    }
    else if (home != NULL) {
	sprintf(path, "%s/.Xdefaults", home);
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }

    if ((ptr = getenv("XENVIRONMENT")) != NULL) {
	tmpDB = XrmGetFileDatabase(ptr);
	XrmMergeDatabases(tmpDB, rDBptr);
    }
    else if (home != NULL) {
	sprintf(path, "%s/.Xdefaults-", home);
	len = strlen(path);
	gethostname(&path[len], sizeof path - len);
	path[sizeof path - 1] = '\0';
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }

    if (home != NULL) {
	sprintf(path, "%s/.xpilotrc", home);
	tmpDB = XrmGetFileDatabase(path);
	XrmMergeDatabases(tmpDB, rDBptr);
    }
#endif
}


void Parse_options(int *argcp, char **argvp, char *realName, char *host,
		   int *port, int *my_team, int *list, int *join, int *motd,
		   char *nickName, char *dispName, char *shut_msg)
{
    int			i,
			j,
			firstKeyDef,
			num;
    keys_t		key;
    KeySym		ks;
    char		*ptr,
			*str,
			*myName = "xpilot",
			*myClass = "XPilot",
			resValue[MAX_CHARS];
    XrmDatabase		argDB, rDB;

    XrmInitialize();

    argDB = 0;
    XrmParseCommand(&argDB, opts, NELEM(opts), myName, argcp, argvp);

    /*
     * Check for bad arguments.
     */
    for (i = 1; i < *argcp; i++) {
	if (argvp[i][0] == '-') {
	    errno = 0;
	    error("Unknown option '%s'", argvp[i]);
	    error("Type: %s -help to see a list of options", argvp[0]);
	    exit(1);
	}
    }

    if (Get_resource(argDB, myName, myClass, "help", NULL, resValue,
		     sizeof resValue) != 0) {
	Usage();
    }

    if (Get_resource(argDB, myName, myClass, "version", NULL, resValue,
		     sizeof resValue) != 0) {
	puts(TITLE);
	exit(0);
    }

    Get_resource(argDB, myName, myClass, "shutdown", "", shut_msg,
		 MAX_CHARS);

    if (Get_string_resource(argDB, myName, myClass, "display", NULL, dispName,
			    MAX_DISP_LEN) == 0
	|| dispName[0] == '\0') {
#ifdef VMS
	if ((ptr = getenv("DECW$DISPLAY")) != NULL) {
#else
	if ((ptr = getenv("DISPLAY")) != NULL) {
#endif
	    strncpy(dispName, ptr, MAX_DISP_LEN);
	    dispName[MAX_DISP_LEN - 1] = '\0';
	} else {
#ifdef VMS
	    strcpy(dispName, "::0.0");
	    sprintf(dispName, "%s::0.0", host);
#else
	    strcpy(dispName, ":0.0");
#endif
	}
    }
    if ((dpy = XOpenDisplay(dispName)) == NULL) {
	error("Can't open display '%s'", dispName);
	exit(1);
    }
    Get_resource(argDB, myName, myClass, "visual", "",
		 visualName, sizeof visualName);
    if (strncasecmp(visualName, "list", 4) == 0) {
	List_visuals();
	exit(0);
    }

    Get_file_defaults(&rDB, myName, myClass);

    XrmMergeDatabases(argDB, &rDB);

    Get_string_resource(rDB, myName, myClass, "geometry", "", resValue,
			sizeof resValue);
    geometry = strdup(resValue);

    Get_resource(rDB, myName, myClass, "name", realName, nickName,
		 MAX_NAME_LEN);
    CAP_LETTER(nickName[0]);
    if (nickName[0] < 'A' || nickName[0] > 'Z') {
	errno = 0;
	error("Your player name \"%s\" should start with an uppercase letter",
	    nickName);
	exit(1);
    }
    strncpy(realname, realName, sizeof(realname) - 1);
    strncpy(name, nickName, sizeof(name) - 1);

    Get_int_resource(rDB, myName, myClass, "team", TEAM_NOT_SET, my_team);
    if (*my_team < 0 || *my_team > 9) {
	*my_team = TEAM_NOT_SET;
    }
    team = *my_team;
    Get_int_resource(rDB, myName, myClass, "port", SERVER_PORT, port);
    Get_bool_resource(rDB, myName, myClass, "list", "False", list);
    Get_bool_resource(rDB, myName, myClass, "join", "False", join);
    Get_bool_resource(rDB, myName, myClass, "motd", "True", motd);

    Get_float_resource(rDB, myName, myClass, "power", 45.0, &power);
    Get_float_resource(rDB, myName, myClass, "turnSpeed", 35.0, &turnspeed);
    Get_float_resource(rDB, myName, myClass, "turnResistance", 0.12,
		       &turnresistance);
    Get_float_resource(rDB, myName, myClass, "altPower", 35.0, &power_s);
    Get_float_resource(rDB, myName, myClass, "altTurnSpeed", 25.0,
		       &turnspeed_s);
    Get_float_resource(rDB, myName, myClass, "altTurnResistance", 0.12,
		       &turnresistance_s);
    Get_float_resource(rDB, myName, myClass, "sparkProb", 0.50,
		       &spark_prob);
    spark_rand = (int)(spark_prob * MAX_SPARK_RAND + 0.5f);
    Get_int_resource(rDB, myName, myClass, "charsPerSecond", 50,
		     &charsPerSecond);
    Get_bool_resource(rDB, myName, myClass, "markingLights", "True", &i);
    markingLights = (i == false) ? false : true;

    Get_int_resource(rDB, myName, myClass, "backgroundPointDist", 8,
		     &map_point_distance);
    Get_int_resource(rDB, myName, myClass, "backgroundPointSize",
		     DEF_MAP_POINT_SIZE, &map_point_size);
    LIMIT(map_point_size, MIN_MAP_POINT_SIZE, MAX_MAP_POINT_SIZE);
    Get_int_resource(rDB, myName, myClass, "sparkSize",
		     DEF_SPARK_SIZE, &spark_size);
    LIMIT(spark_size, MIN_SPARK_SIZE, MAX_SPARK_SIZE);

    Get_resource(rDB, myName, myClass, "visual", "",
		 visualName, sizeof visualName);
    Get_bool_resource(rDB, myName, myClass, "mono", "False", &i);
    mono = (i != 0) ? true : false;
    Get_bool_resource(rDB, myName, myClass, "colorSwitch", "True", &i);
    colorSwitch = (i != 0) ? true : false;
    Get_int_resource(rDB, myName, myClass, "maxColors", 4,
		     &maxColors);
    Get_string_resource(rDB, myName, myClass, "black", "",
			color_names[BLACK], sizeof(color_names[BLACK]));
    Get_string_resource(rDB, myName, myClass, "white", "",
			color_names[WHITE], sizeof(color_names[WHITE]));
    Get_string_resource(rDB, myName, myClass, "blue", "",
			color_names[BLUE], sizeof(color_names[BLUE]));
    Get_string_resource(rDB, myName, myClass, "red", "",
			color_names[RED], sizeof(color_names[RED]));
    for (i = 0; i < MAX_COLORS; i++) {
	char buf[8], def[MAX_COLOR_LEN];
	sprintf(buf, "color%d", i);
	strcpy(def, (i < NUM_COLORS) ? color_names[i] : "");
	Get_string_resource(rDB, myName, myClass, buf, def,
			    color_names[i], sizeof(color_names[i]));
    }

    instruments = 0;
    Get_bool_resource(rDB, myName, myClass, "showShipName", "True", &i);
    if (i) {
	SET_BIT(instruments, SHOW_SHIP_NAME);
    }
    Get_bool_resource(rDB, myName, myClass, "showHUD", "True", &i);
    if (i) {
	SET_BIT(instruments, SHOW_HUD_INSTRUMENTS);
    }
    Get_bool_resource(rDB, myName, myClass, "verticalHUDLine", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_HUD_VERTICAL);
    }
    Get_bool_resource(rDB, myName, myClass, "horizontalHUDLine", "True", &i);
    if (i) {
	SET_BIT(instruments, SHOW_HUD_HORIZONTAL);
    }
    Get_bool_resource(rDB, myName, myClass, "fuelMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_FUEL_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "fuelGauge", "True", &i);
    if (i) {
	SET_BIT(instruments, SHOW_FUEL_GAUGE);
    }
    Get_bool_resource(rDB, myName, myClass, "turnSpeedMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_TURNSPEED_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "powerMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_POWER_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "packetSizeMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_PACKET_SIZE_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "packetLossMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_PACKET_LOSS_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "packetDropMeter", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_PACKET_DROP_METER);
    }
    Get_bool_resource(rDB, myName, myClass, "slidingRadar", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_SLIDING_RADAR);
    }
    Get_bool_resource(rDB, myName, myClass, "outlineWorld", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_OUTLINE_WORLD);
    }
    Get_bool_resource(rDB, myName, myClass, "clock", "False", &i);
    if (i) {
	SET_BIT(instruments, SHOW_CLOCK);
    }

    Get_float_resource(rDB, myName, myClass, "speedFactHUD", 0.0,
		       &hud_move_fact);
    Get_float_resource(rDB, myName, myClass, "speedFactPTR", 0.0,
		       &ptr_move_fact);
    Get_int_resource(rDB, myName, myClass, "fuelNotify", 500, &fuelLevel3);
    Get_int_resource(rDB, myName, myClass, "fuelWarning", 200, &fuelLevel2);
    Get_int_resource(rDB, myName, myClass, "fuelCritical", 100, &fuelLevel1);

    Get_resource(rDB, myName, myClass, "gameFont", GAME_FONT,
		 gameFontName, sizeof gameFontName);
    Get_resource(rDB, myName, myClass, "messageFont", MESSAGE_FONT,
		 messageFontName, sizeof messageFontName);
    Get_resource(rDB, myName, myClass, "scoreListFont", SCORE_LIST_FONT,
		 scoreListFontName, sizeof scoreListFontName);
    Get_resource(rDB, myName, myClass, "buttonFont", BUTTON_FONT,
		 buttonFontName, sizeof buttonFontName);
    Get_resource(rDB, myName, myClass, "textFont", TEXT_FONT,
		 textFontName, sizeof textFontName);
    Get_resource(rDB, myName, myClass, "talkFont", TALK_FONT,
		 talkFontName, sizeof talkFontName);

    Get_int_resource(rDB, myName, myClass, "receiveWindowSize",
		     DEF_RECEIVE_WINDOW_SIZE, &receive_window_size);
    LIMIT(receive_window_size, MIN_RECEIVE_WINDOW_SIZE,
	  MAX_RECEIVE_WINDOW_SIZE);

#ifdef SOUND
    Get_string_resource(rDB, myName, myClass, "sounds", SOUNDFILE, sounds,
			sizeof sounds);

    Get_int_resource(rDB, myName, myClass, "maxVolume", 100, &maxVolume);

    Get_resource(rDB, myName, myClass, "audioServer", NULL, audioServer,
		 sizeof audioServer);
#endif

    Get_bool_resource(rDB, myName, myClass, "toggleShield", "False",
		      &toggle_shield);

    /*
     * Key bindings
     */
    maxKeyDefs = 2 * NUM_KEYS;
    if ((keyDefs = (keydefs_t *)
	malloc(maxKeyDefs * sizeof(keydefs_t))) == NULL) {
	error("No memory for key bindings");
	exit(1);
    }
    num = 0;
    for (i = 0; i < NELEM(keyResources); i++) {
	key = keyResources[i].key;
	Get_resource(rDB, myName, myClass,
		     keyResources[i].resource, keyResources[i].fallback,
		     resValue, sizeof resValue);

	firstKeyDef = num;
	for (str = strtok(resValue, " \t\r\n");
	    str != NULL;
	    str = strtok(NULL, " \t\r\n")) {

	    if ((ks = XStringToKeysym(str)) == NoSymbol) {
		printf("\"%s\" is not a valid keysym.\n", str);
		continue;
	    }

	    for (j = firstKeyDef; j < num; j++) {
		if (keyDefs[j].keysym == ks
		    && keyDefs[j].key == key) {
		    break;
		}
	    }
	    if (j < num) {
		continue;
	    }
	    if (num >= maxKeyDefs) {
		maxKeyDefs += NUM_KEYS;
		if ((keyDefs = (keydefs_t *)
		    realloc(keyDefs, maxKeyDefs * sizeof(keydefs_t)))
		    == NULL) {
		    error("No memory for key bindings");
		    exit(1);
		}
	    }

	    keyDefs[num].keysym = ks;
	    keyDefs[num].key = key;
	    num++;
	}
    }
    if (num < maxKeyDefs) {
	maxKeyDefs = num;
	if ((keyDefs = (keydefs_t *)
	    realloc(keyDefs, maxKeyDefs * sizeof(keydefs_t))) == NULL) {
	    error("No memory for key bindings");
	    exit(1);
	}
    }

    XrmDestroyDatabase(rDB);

#ifdef SOUND
    audioInit(dispName);
#endif /* SOUND */
}
