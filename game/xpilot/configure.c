/* $Id: configure.c,v 1.1 1994/02/23 14:40:04 jkh Exp $
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

/*
 * Configure.c: real-time option control.
 * To add your own option to the XPilot client do the following:
 * 1: Define storage for the option value in either client.c/paint.c/xinit.c
 *    or use a bit in the instruments option set (using the SHOW_XXX macros).
 * 2: Add a declaration for this storage to either client.h/paint.h/xinit.h
 *    or, in case a bit in instruments is used, add a SHOW_ macro to client.h.
 * 3: Add an X resource record to the XrmOptionDescRec opts[] table in
 *    default.c to have it recognised by the X resource manager routines.
 * 4: Have it set at startup by the Parse_options() routine in default.c.
 * 5: Add the functionality of your option, probably in the same file
 *    as the storage for the option was defined in.
 * 6: Add it to configure.c (this file) as follows:
 *   a) Determine if it needs either a bool/int/float widget
 *      and find a similar option from which you can copy code.
 *   b) Add the Config_create_XXX function prototype at the top of this file.
 *   c) Add the Config_create_XXX function name to the config_creator[] table.
 *      The order in this table determines the order of the options on screen.
 *   d) Define the Config_create_XXX function similar to one of the others.
 *   e) If it needs a callback when the value changes then add a
 *      Config_update_XXX() function after the other update callbacks
 *      and declare a prototype for the callback at the top of this file.
 *      The Config_update_XXX() function should be given as an argument to
 *      the Config_create_bool/int/float() creator in Config_create_XXX().
 *      If the option doesn't need a callback then the calback argument
 *      should be given as NULL.
 *   f) Add one line to the Config_save() routine to have the option saved.
 * 7: Document your option in the manual page for the client.
 * 8: Mail a context diff of your changes to xpilot@cs.uit.no.
 */

#include <X11/Xproto.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xutil.h>

#ifdef VMS
#include <unixio.h>
#include <unixlib.h>
#else
#include <unistd.h>
#include <pwd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#ifdef VMS
#include "strcasecmp.h"
#endif
#include <limits.h>

#include "version.h"
#include "client.h"
#include "paint.h"
#include "draw.h"
#include "xinit.h"
#include "bit.h"
#include "netclient.h"
#include "widget.h"
#include "configure.h"
#include "setup.h"
#include "error.h"

#ifndef PATH_MAX
#define PATH_MAX	1024
#endif

static int Config_create_power(int widget_desc, int *height);
static int Config_create_turnSpeed(int widget_desc, int *height);
static int Config_create_turnResistance(int widget_desc, int *height);
static int Config_create_altPower(int widget_desc, int *height);
static int Config_create_altTurnSpeed(int widget_desc, int *height);
static int Config_create_altTurnResistance(int widget_desc, int *height);
static int Config_create_showHUD(int widget_desc, int *height);
static int Config_create_horizontalHUDLine(int widget_desc, int *height);
static int Config_create_verticalHUDLine(int widget_desc, int *height);
static int Config_create_speedFactHUD(int widget_desc, int *height);
static int Config_create_speedFactPTR(int widget_desc, int *height);
static int Config_create_fuelNotify(int widget_desc, int *height);
static int Config_create_fuelWarning(int widget_desc, int *height);
static int Config_create_fuelCritical(int widget_desc, int *height);
static int Config_create_fuelGauge(int widget_desc, int *height);
static int Config_create_outlineWorld(int widget_desc, int *height);
static int Config_create_slidingRadar(int widget_desc, int *height);
static int Config_create_backgroundPointDist(int widget_desc, int *height);
static int Config_create_backgroundPointSize(int widget_desc, int *height);
static int Config_create_sparkSize(int widget_desc, int *height);
static int Config_create_charsPerSecond(int widget_desc, int *height);
static int Config_create_toggleShield(int widget_desc, int *height);
static int Config_create_sparkProb(int widget_desc, int *height);
#ifdef SOUND
static int Config_create_maxVolume(int widget_desc, int *height);
#endif
static int Config_create_showShipName(int widget_desc, int *height);
static int Config_create_fuelMeter(int widget_desc, int *height);
static int Config_create_powerMeter(int widget_desc, int *height);
static int Config_create_turnSpeedMeter(int widget_desc, int *height);
static int Config_create_packetSizeMeter(int widget_desc, int *height);
static int Config_create_packetLossMeter(int widget_desc, int *height);
static int Config_create_packetDropMeter(int widget_desc, int *height);
static int Config_create_clock(int widget_desc, int *height);
static int Config_create_markingLights(int widget_desc, int *height);
static int Config_create_save(int widget_desc, int *height);

static int Config_update_bool(int widget_desc, void *data, bool *val);
static int Config_update_instruments(int widget_desc, void *data, bool *val);
static int Config_update_dots(int widget_desc, void *data, int *val);
static int Config_update_altPower(int widget_desc, void *data, float *val);
static int Config_update_altTurnResistance(int widget_desc, void *data,
					   float *val);
static int Config_update_altTurnSpeed(int widget_desc, void *data, float *val);
static int Config_update_power(int widget_desc, void *data, float *val);
static int Config_update_turnResistance(int widget_desc, void *data,
					float *val);
static int Config_update_turnSpeed(int widget_desc, void *data, float *val);
static int Config_update_sparkProb(int widget_desc, void *data, float *val);
static int Config_update_charsPerSecond(int widget_desc, void *data, int *val);
static int Config_update_toggleShield(int widget_desc, void *data, bool *val);

static int Config_close(int widget_desc, void *data, char **strptr);
static int Config_next(int widget_desc, void *data, char **strptr);
static int Config_prev(int widget_desc, void *data, char **strptr);
static int Config_save(int widget_desc, void *data, char **strptr);
static int Config_save_confirm_callback(int widget_desc, void *popup_desc,
					char **strptr);

typedef struct xpilotrc {
    char	*line;
    short	size;
} xpilotrc_t;

static xpilotrc_t	*xpilotrc_ptr;
static int		num_xpilotrc, max_xpilotrc;

static bool		config_created = false,
			config_mapped = false;
static int		config_page,
			config_x,
			config_y,
			config_width,
			config_height,
			config_space,
			config_max,
			config_button_space,
			config_text_space,
			config_text_height,
			config_button_height,
			config_entry_height,
			config_bool_width,
			config_bool_height,
			config_int_width,
			config_float_width,
			config_arrow_width,
			config_arrow_height;
static int		*config_widget_desc,
			config_save_confirm_desc = NO_WIDGET;
static int		(*config_creator[])(int widget_desc, int *height) = {
    Config_create_power,
    Config_create_turnSpeed,
    Config_create_turnResistance,
    Config_create_altPower,
    Config_create_altTurnSpeed,
    Config_create_altTurnResistance,
    Config_create_showHUD,
    Config_create_horizontalHUDLine,
    Config_create_verticalHUDLine,
    Config_create_speedFactHUD,
    Config_create_speedFactPTR,
    Config_create_fuelNotify,
    Config_create_fuelWarning,
    Config_create_fuelCritical,
    Config_create_fuelGauge,
    Config_create_outlineWorld,
    Config_create_slidingRadar,
    Config_create_backgroundPointDist,
    Config_create_backgroundPointSize,
    Config_create_sparkSize,
    Config_create_sparkProb,
    Config_create_charsPerSecond,
    Config_create_markingLights,
    Config_create_toggleShield,
#ifdef SOUND
    Config_create_maxVolume,
#endif
    Config_create_showShipName,
    Config_create_fuelMeter,
    Config_create_powerMeter,
    Config_create_turnSpeedMeter,
    Config_create_packetSizeMeter,
    Config_create_packetLossMeter,
    Config_create_packetDropMeter,
    Config_create_clock,
    Config_create_save			/* must be last */
};

static void Create_config(void)
{
    int			i,
			num,
			height,
			offset,
			width,
			widget_desc;
    bool		full;

    /*
     * Window dimensions relative to the top window.
     */
    config_x = 0;
    config_y = RadarHeight + ButtonHeight + 2;
    config_width = 256;
    config_height = top_height - config_y;

    /*
     * Space between label-text and label-border.
     */
    config_text_space = 3;
    /*
     * Height of a label window.
     */
    config_text_height = 2 * 1 + textFont->ascent + textFont->descent;

    /*
     * Space between button-text and button-border.
     */
    config_button_space = 3;
    /*
     * Height of a button window.
     */
    config_button_height = buttonFont->ascent + buttonFont->descent
			    + 2 * 1;

    config_entry_height = MAX(config_text_height, config_button_height);

    /*
     * Space between entries and between an entry and the border.
     */
    config_space = 6;

    /*
     * Sizes of the different widget types.
     */
    config_bool_width = XTextWidth(buttonFont, "Yes", 3)
			+ 2 * config_button_space;
    config_bool_height = config_button_height;
    config_arrow_height = config_text_height;
    config_arrow_width = config_text_height;
    config_int_width = 4 + XTextWidth(buttonFont, "1000", 4);
    config_float_width = 4 + XTextWidth(buttonFont, "0.22", 4);

    config_max = NELEM(config_creator);
    config_widget_desc = (int *) malloc(config_max * sizeof(int));
    if (config_widget_desc == NULL) {
	error("No memory for config");
	return;
    }

    num = -1;
    full = true;
    for (i = 0; i < NELEM(config_creator); i++) {
	if (full == true) {
	    full = false;
	    num++;
	    config_widget_desc[num]
		= Widget_create_form(NO_WIDGET, top,
				     config_x, config_y,
				     config_width, config_height,
				     0);
	    if (config_widget_desc[num] == 0) {
		break;
	    }
	    height = config_height - config_space - config_button_height;
	    width = 2 * config_button_space + XTextWidth(buttonFont,
							  "PREV", 4);
	    offset = config_width - width - config_space;
	    widget_desc =
		Widget_create_activate(config_widget_desc[num],
				       offset, height,
				       width, config_button_height,
				       0, "PREV", Config_prev, (void *)num);
	    if (widget_desc == 0) {
		break;
	    }
	    width = 2 * config_button_space + XTextWidth(buttonFont,
							  "NEXT", 4);
	    offset = (config_width - width) / 2;
	    widget_desc =
		Widget_create_activate(config_widget_desc[num],
				       offset, height,
				       width, config_button_height,
				       0, "NEXT", Config_next, (void *)num);
	    if (widget_desc == 0) {
		break;
	    }
	    width = 2 * config_button_space + XTextWidth(buttonFont,
							  "CLOSE", 5);
	    offset = config_space;
	    widget_desc =
		Widget_create_activate(config_widget_desc[num],
				       offset, height,
				       width, config_button_height,
				       0, "CLOSE", Config_close, (void *)num);
	    if (widget_desc == 0) {
		break;
	    }
	    height = config_space;
	}
	if ((*config_creator[i])(config_widget_desc[num], &height) == 0) {
	    i--;
	    full = true;
	    if (height == config_space) {
		break;
	    }
	    continue;
	}
    }
    if (i < NELEM(config_creator)) {
	for (; num >= 0; num--) {
	    if (config_widget_desc[num] != 0) {
		Widget_destroy(config_widget_desc[num]);
	    }
	}
	config_created = false;
	config_mapped = false;
    } else {
	config_max = num + 1;
	config_widget_desc = (int *)realloc(config_widget_desc,
					    config_max * sizeof(int));
	config_page = 0;
	for (i = 0; i < config_max; i++) {
	    Widget_map_sub(config_widget_desc[i]);
	}
	config_created = true;
	config_mapped = false;
    }
}

static int Config_close(int widget_desc, void *data, char **strptr)
{
    Widget_unmap(config_widget_desc[config_page]);
    config_mapped = false;
    return 0;
}

static int Config_next(int widget_desc, void *data, char **strptr)
{
    int			prev_page = config_page;

    if (config_max > 1) {
	config_page = (config_page + 1) % config_max;
	Widget_raise(config_widget_desc[config_page]);
	Widget_unmap(config_widget_desc[prev_page]);
	config_mapped = true;
    }
    return 0;
}

static int Config_prev(int widget_desc, void *data, char **strptr)
{
    int			prev_page = config_page;

    if (config_max > 1) {
	config_page = (config_page - 1 + config_max) % config_max;
	Widget_raise(config_widget_desc[config_page]);
	Widget_unmap(config_widget_desc[prev_page]);
	config_mapped = true;
    }
    return 0;
}

static int Config_create_bool(int widget_desc, int *height,
			      char *str, bool val,
			      int (*callback)(int, void *, bool *),
			      void *data)
{
    int			offset,
			label_width;

    if (*height + 2*config_entry_height + 2*config_space >= config_height) {
	return 0;
    }
    label_width = XTextWidth(textFont, str, strlen(str))
		  + 2 * config_text_space;
    offset = config_width - (config_space + config_bool_width);
    if (config_space + label_width > offset) {
	if (*height + 3*config_entry_height + 2*config_space
	    >= config_height) {
	    return 0;
	}
    }

    Widget_create_label(widget_desc, config_space, *height
			    + (config_entry_height - config_text_height) / 2,
			label_width, config_text_height,
			0, str);
    if (config_space + label_width > offset) {
	*height += config_entry_height;
    }
    Widget_create_bool(widget_desc,
		       offset, *height
			   + (config_entry_height - config_bool_height) / 2,
		       config_bool_width,
		       config_bool_height,
		       0, val, callback, data);
    *height += config_entry_height + config_space;

    return 1;
}

static int Config_create_int(int widget_desc, int *height,
			     char *str, int *val, int min, int max,
			     int (*callback)(int, void *, int *), void *data)
{
    int			offset,
			label_width,
			intw;

    if (*height + 2*config_entry_height + 2*config_space >= config_height) {
	return 0;
    }
    label_width = XTextWidth(textFont, str, strlen(str))
		  + 2 * config_text_space;
    offset = config_width - (config_space + 2 * config_arrow_width
	    + config_int_width);
    if (config_space + label_width > offset) {
	if (*height + 3*config_entry_height + 2*config_space
	    >= config_height) {
	    return 0;
	}
    }
    Widget_create_label(widget_desc, config_space, *height
			+ (config_entry_height - config_text_height) / 2,
			label_width, config_text_height,
			0, str);
    if (config_space + label_width > offset) {
	*height += config_entry_height;
    }
    intw = Widget_create_int(widget_desc, offset, *height
			      + (config_entry_height - config_text_height) / 2,
			     config_int_width, config_text_height,
			     0, val, min, max, callback, data);
    offset += config_int_width;
    Widget_create_arrow_left(widget_desc, offset, *height
			     + (config_entry_height - config_arrow_height) / 2,
			     config_arrow_width, config_arrow_height,
			     0, intw);
    offset += config_arrow_width;
    Widget_create_arrow_right(widget_desc, offset, *height
			      + (config_entry_height - config_arrow_height) / 2,
			      config_arrow_width, config_arrow_height,
			      0, intw);
    *height += config_entry_height + config_space;

    return 1;
}

static int Config_create_float(int widget_desc, int *height,
			       char *str, float *val, float min, float max,
			       int (*callback)(int, void *, float *),
			       void *data)
{
    int			offset,
			label_width,
			floatw;

    if (*height + 2*config_entry_height + 2*config_space >= config_height) {
	return 0;
    }
    label_width = XTextWidth(textFont, str, strlen(str))
		  + 2 * config_text_space;
    offset = config_width - (config_space + 2 * config_arrow_width
	    + config_float_width);
    if (config_space + label_width > offset) {
	if (*height + 3*config_entry_height + 2*config_space
	    >= config_height) {
	    return 0;
	}
    }
    Widget_create_label(widget_desc, config_space, *height
			+ (config_entry_height - config_text_height) / 2,
			label_width, config_text_height,
			0, str);
    if (config_space + label_width > offset) {
	*height += config_entry_height;
    }
    floatw = Widget_create_float(widget_desc, offset, *height
				 + (config_entry_height
				 - config_text_height) / 2,
				 config_float_width, config_text_height,
				 0, val, min, max, callback, data);
    offset += config_float_width;
    Widget_create_arrow_left(widget_desc, offset, *height
			     + (config_entry_height - config_arrow_height) / 2,
			     config_arrow_width, config_arrow_height,
			     0, floatw);
    offset += config_arrow_width;
    Widget_create_arrow_right(widget_desc, offset, *height
			      + (config_entry_height - config_arrow_height) / 2,
			      config_arrow_width, config_arrow_height,
			      0, floatw);
    *height += config_entry_height + config_space;

    return 1;
}

static int Config_create_power(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "power", &power,
			       MIN_PLAYER_POWER, MAX_PLAYER_POWER,
			       Config_update_power, NULL);
}

static int Config_create_turnSpeed(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "turnSpeed", &turnspeed,
			       MIN_PLAYER_TURNSPEED, MAX_PLAYER_TURNSPEED,
			       Config_update_turnSpeed, NULL);
}

static int Config_create_turnResistance(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "turnResistance", &turnresistance,
			       MIN_PLAYER_TURNRESISTANCE,
			       MAX_PLAYER_TURNRESISTANCE,
			       Config_update_turnResistance, NULL);
}

static int Config_create_altPower(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "altPower", &power_s,
			       MIN_PLAYER_POWER, MAX_PLAYER_POWER,
			       Config_update_altPower, NULL);
}

static int Config_create_altTurnSpeed(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "altTurnSpeed", &turnspeed_s,
			       MIN_PLAYER_TURNSPEED, MAX_PLAYER_TURNSPEED,
			       Config_update_altTurnSpeed, NULL);
}

static int Config_create_altTurnResistance(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "altTurnResistance", &turnresistance_s,
			       MIN_PLAYER_TURNRESISTANCE,
			       MAX_PLAYER_TURNRESISTANCE,
			       Config_update_altTurnResistance, NULL);
}

static int Config_create_showHUD(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "showHUD",
			      BIT(instruments, SHOW_HUD_INSTRUMENTS)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_HUD_INSTRUMENTS);
}

static int Config_create_horizontalHUDLine(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "horizontalHUDLine",
			      BIT(instruments, SHOW_HUD_HORIZONTAL)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_HUD_HORIZONTAL);
}

static int Config_create_verticalHUDLine(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "verticalHUDLine",
			      BIT(instruments, SHOW_HUD_VERTICAL)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_HUD_VERTICAL);
}

static int Config_create_speedFactHUD(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "speedFactHUD", &hud_move_fact, -10.0, 10.0,
			       NULL, NULL);
}

static int Config_create_speedFactPTR(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "speedFactPTR", &ptr_move_fact, -10.0, 10.0,
			       NULL, NULL);
}

static int Config_create_fuelNotify(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "fuelNotify", &fuelLevel3, 0, 1000,
			     NULL, NULL);
}

static int Config_create_fuelWarning(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "fuelWarning", &fuelLevel2, 0, 1000,
			     NULL, NULL);
}

static int Config_create_fuelCritical(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "fuelCritical", &fuelLevel1, 0, 1000,
			     NULL, NULL);
}

static int Config_create_fuelGauge(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "fuelGauge",
			      BIT(instruments, SHOW_FUEL_GAUGE)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_FUEL_GAUGE);
}

static int Config_create_outlineWorld(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "outlineWorld",
			      BIT(instruments, SHOW_OUTLINE_WORLD)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_OUTLINE_WORLD);
}

static int Config_create_slidingRadar(int widget_desc, int *height)
{
    if (Client_wrap_mode() == 0) {
	return 1;
    }
    return Config_create_bool(widget_desc, height, "slidingRadar",
			      BIT(instruments, SHOW_SLIDING_RADAR)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_SLIDING_RADAR);
}

static int Config_create_backgroundPointDist(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "backgroundPointDist", &map_point_distance, 0, 10,
			     Config_update_dots, NULL);
}

static int Config_create_backgroundPointSize(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "backgroundPointSize", &map_point_size,
			     MIN_MAP_POINT_SIZE, MAX_MAP_POINT_SIZE,
			     Config_update_dots, NULL);
}

static int Config_create_sparkSize(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "sparkSize", &spark_size,
			     MIN_SPARK_SIZE, MAX_SPARK_SIZE,
			     NULL, NULL);
}

static int Config_create_sparkProb(int widget_desc, int *height)
{
    return Config_create_float(widget_desc, height,
			       "sparkProb", &spark_prob,
			       0.0, 1.0,
			       Config_update_sparkProb, NULL);
}

static int Config_create_charsPerSecond(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "charsPerSecond", &charsPerSecond,
			     10, 255,
			     Config_update_charsPerSecond, NULL);
}

static int Config_create_toggleShield(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "toggleShield",
			      (toggle_shield) ? true : false,
			      Config_update_toggleShield, NULL);
}

#ifdef SOUND
static int Config_create_maxVolume(int widget_desc, int *height)
{
    return Config_create_int(widget_desc, height,
			     "maxVolume", &maxVolume, 0, 255,
			     NULL, NULL);
}
#endif

static int Config_create_showShipName(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "showShipName",
			      BIT(instruments, SHOW_SHIP_NAME)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_SHIP_NAME);
}

static int Config_create_fuelMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "fuelMeter",
			      BIT(instruments, SHOW_FUEL_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_FUEL_METER);
}

static int Config_create_powerMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "powerMeter",
			      BIT(instruments, SHOW_POWER_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_POWER_METER);
}

static int Config_create_turnSpeedMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "turnSpeedMeter",
			      BIT(instruments, SHOW_TURNSPEED_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_TURNSPEED_METER);
}

static int Config_create_packetSizeMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "packetSizeMeter",
			      BIT(instruments, SHOW_PACKET_SIZE_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_PACKET_SIZE_METER);
}

static int Config_create_packetLossMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "packetLossMeter",
			      BIT(instruments, SHOW_PACKET_LOSS_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_PACKET_LOSS_METER);
}

static int Config_create_packetDropMeter(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "packetDropMeter",
			      BIT(instruments, SHOW_PACKET_DROP_METER)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_PACKET_DROP_METER);
}

static int Config_create_clock(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "clock",
			      BIT(instruments, SHOW_CLOCK)
				  ? true : false,
			      Config_update_instruments,
			      (void *) SHOW_CLOCK);
}

static int Config_create_markingLights(int widget_desc, int *height)
{
    return Config_create_bool(widget_desc, height, "markingLights",
			      markingLights,
			      Config_update_bool, &markingLights);
}
    

static int Config_create_save(int widget_desc, int *height)
{
    static char		save_str[] = "Save Configuration";
    int			space,
			button_desc,
			width = 2 * config_button_space
				+ XTextWidth(buttonFont, save_str,
					     strlen(save_str));

    space = config_height - (*height + 2*config_entry_height + 2*config_space);
    if (space < 0) {
	return 0;
    }
    button_desc =
	Widget_create_activate(widget_desc,
			       (config_width - width) / 2,
			       *height + space / 2,
			       width, config_button_height,
			       0, save_str,
			       Config_save, (void *)save_str);
    if (button_desc == NO_WIDGET) {
	return 0;
    }
    *height += config_entry_height + config_space + space;

    return 1;
}

/* General purpose update callback for booleans.
 * Requires that a pointer to the boolean value has been given as
 * client_data argument, and updates this value to the real value.
 */
static int Config_update_bool(int widget_desc, void *data, bool *val)
{
    bool*	client_data = data;
    *client_data = *val;
    return 0;
}


static int Config_update_instruments(int widget_desc, void *data, bool *val)
{
    if (*val == false) {
	CLR_BIT(instruments, (long)data);
    } else {
	SET_BIT(instruments, (long)data);
    }
    if ((long)data == SHOW_SLIDING_RADAR) {
	Paint_sliding_radar();
    }
    if ((long)data == SHOW_OUTLINE_WORLD) {
	Map_blue();
    }
    if ((long)data == SHOW_PACKET_DROP_METER
	|| (long)data == SHOW_PACKET_LOSS_METER) {
	Net_init_measurement();
    }
    return 0;
}

static int Config_update_dots(int widget_desc, void *data, int *val)
{
    if (val == &map_point_size && map_point_size > 1) {
	return 0;
    }
    Map_dots();
    return 0;
}

static int Config_update_power(int widget_desc, void *data, float *val)
{
    Send_power(*val);
    control_count = CONTROL_DELAY;
    return 0;
}

static int Config_update_turnSpeed(int widget_desc, void *data, float *val)
{
    Send_turnspeed(*val);
    control_count = CONTROL_DELAY;
    return 0;
}

static int Config_update_turnResistance(int widget_desc, void *data, float *val)
{
    Send_turnresistance(*val);
    return 0;
}

static int Config_update_altPower(int widget_desc, void *data, float *val)
{
    Send_power_s(*val);
    return 0;
}

static int Config_update_altTurnSpeed(int widget_desc, void *data, float *val)
{
    Send_turnspeed_s(*val);
    return 0;
}

static int Config_update_altTurnResistance(int widget_desc, void *data, float *val)
{
    Send_turnresistance_s(*val);
    return 0;
}

static int Config_update_sparkProb(int widget_desc, void *data, float *val)
{
    spark_rand = (int)(spark_prob * MAX_SPARK_RAND + 0.5f);
    Send_display();
    return 0;
}

static int Config_update_charsPerSecond(int widget_desc, void *data, int *val)
{
    charsPerTick = (float)charsPerSecond / FPS;
    Send_display();
    return 0;
}

static int Config_update_toggleShield(int widget_desc, void *data, bool *val)
{
    Set_toggle_shield(*val != false);
    return 0;
}

static void Config_save_failed(char *reason, char **strptr)
{
    if (config_save_confirm_desc != NO_WIDGET) {
	Widget_destroy(config_save_confirm_desc);
    }
    config_save_confirm_desc
	= Widget_create_confirm(reason, Config_save_confirm_callback);
    if (config_save_confirm_desc != NO_WIDGET) {
	Widget_raise(config_save_confirm_desc);
    }
    *strptr = "Saving failed...";
}

static int Xpilotrc_add(char *line)
{
    int			size;
    char		*str;

    if (strncmp(line, "XPilot", 6) != 0 && strncmp(line, "xpilot", 6) != 0) {
	return 0;
    }
    if (line[6] != '.' && line[6] != '*') {
	return 0;
    }
    if ((str = strchr(line + 7, ':')) == NULL) {
	return 0;
    }
    size = str - (line + 7);
    if (max_xpilotrc <= 0 || xpilotrc_ptr == NULL) {
	num_xpilotrc = 0;
	max_xpilotrc = 75;
	if ((xpilotrc_ptr = (xpilotrc_t *)
		malloc(max_xpilotrc * sizeof(xpilotrc_t))) == NULL) {
	    max_xpilotrc = 0;
	    return -1;
	}
    }
    if (num_xpilotrc >= max_xpilotrc) {
	max_xpilotrc *= 2;
	if ((xpilotrc_ptr = (xpilotrc_t *) realloc(xpilotrc_ptr,
		max_xpilotrc * sizeof(xpilotrc_t))) == NULL) {
	    max_xpilotrc = 0;
	    return -1;
	}
    }
    if ((str = strdup(line)) == NULL) {
	return -1;
    }
    xpilotrc_ptr[num_xpilotrc].line = str;
    xpilotrc_ptr[num_xpilotrc].size = size;
    num_xpilotrc++;
    return 0;
}

static void Xpilotrc_end(FILE *fp)
{
    int			i;

    if (max_xpilotrc <= 0 || xpilotrc_ptr == NULL) {
	return;
    }
    for (i = 0; i < num_xpilotrc; i++) {
	fprintf(fp, "%s", xpilotrc_ptr[i].line);
	free(xpilotrc_ptr[i].line);
    }
    free(xpilotrc_ptr);
    xpilotrc_ptr = NULL;
    max_xpilotrc = 0;
    num_xpilotrc = 0;
}

static void Xpilotrc_use(char *line)
{
    int			i;

    for (i = 0; i < num_xpilotrc; i++) {
	if (strncmp(xpilotrc_ptr[i].line + 7, line + 7,
		    xpilotrc_ptr[i].size + 1) == 0) {
	    free(xpilotrc_ptr[i].line);
	    xpilotrc_ptr[i--] = xpilotrc_ptr[--num_xpilotrc];
	}
    }
}

static void Config_save_resource(FILE *fp, char *resource, char *value)
{
    char		buf[256];

    sprintf(buf, "xpilot.%s:\t\t%s\n", resource, value);
    Xpilotrc_use(buf);
    fprintf(fp, "%s", buf);
}

static void Config_save_float(FILE *fp, char *resource, float value)
{
    char		buf[40];

    sprintf(buf, "%.3f", value);
    Config_save_resource(fp, resource, buf);
}

static void Config_save_int(FILE *fp, char *resource, int value)
{
    char		buf[20];

    sprintf(buf, "%d", value);
    Config_save_resource(fp, resource, buf);
}

static void Config_save_bool(FILE *fp, char *resource, int value)
{
    char		buf[20];

    sprintf(buf, "%s", (value != 0) ? "True" : "False");
    Config_save_resource(fp, resource, buf);
}

static int Config_save(int widget_desc, void *button_str, char **strptr)
{
#ifdef VMS
    return 1;
#else
    extern char		*Get_keyResourceString(keys_t key);
    int			i;
    KeySym		ks;
    keys_t		key, prev_key;
    struct passwd	*pwent;
    FILE		*fp;
    char		*str,
			*home,
			*res,
			*base = ".xpilotrc",
			buf[512],
			oldfile[PATH_MAX],
			newfile[PATH_MAX];

    *strptr = "Saving...";
    Widget_draw(widget_desc);
    Client_flush();

    if ((home = getenv("HOME")) == NULL
	&& ((pwent = getpwuid(getuid())) == NULL
	|| (home = pwent->pw_dir)[0] == '\0')
	|| access(home, 0) == -1) {
	Config_save_failed("Can't find home directory.", strptr);
	return 1;
    }
    sprintf(oldfile, "%s/%s", home, base);
    if ((fp = fopen(oldfile, "r")) != NULL) {
	while (fgets(buf, sizeof buf, fp)) {
	    buf[sizeof buf - 1] = '\0';
	    Xpilotrc_add(buf);
	}
	fclose(fp);
    }
    sprintf(newfile, "%s/%s.new", home, base);
    unlink(newfile);
    if ((fp = fopen(newfile, "w")) == NULL) {
	Config_save_failed("Can't open file to save to.", strptr);
	return 1;
    }
    Config_save_resource(fp, "name", name);
    Config_save_float(fp, "power", power);
    Config_save_float(fp, "turnSpeed", turnspeed);
    Config_save_float(fp, "turnResistance", turnresistance);
    Config_save_float(fp, "altPower", power_s);
    Config_save_float(fp, "altTurnSpeed", turnspeed_s);
    Config_save_float(fp, "altTurnResistance", turnresistance_s);
    Config_save_float(fp, "speedFactHUD", hud_move_fact);
    Config_save_float(fp, "speedFactPTR", ptr_move_fact);
    Config_save_float(fp, "fuelNotify", fuelLevel3);
    Config_save_float(fp, "fuelWarning", fuelLevel2);
    Config_save_float(fp, "fuelCritical", fuelLevel1);
    Config_save_bool(fp, "showShipName", BIT(instruments, SHOW_SHIP_NAME));
    Config_save_bool(fp, "showHUD", BIT(instruments, SHOW_HUD_INSTRUMENTS));
    Config_save_bool(fp, "verticalHUDLine", BIT(instruments, SHOW_HUD_VERTICAL));
    Config_save_bool(fp, "horizontalHUDLine", BIT(instruments, SHOW_HUD_HORIZONTAL));
    Config_save_bool(fp, "fuelMeter", BIT(instruments, SHOW_FUEL_METER));
    Config_save_bool(fp, "fuelGauge", BIT(instruments, SHOW_FUEL_GAUGE));
    Config_save_bool(fp, "turnSpeedMeter", BIT(instruments, SHOW_TURNSPEED_METER));
    Config_save_bool(fp, "powerMeter", BIT(instruments, SHOW_POWER_METER));
    Config_save_bool(fp, "packetSizeMeter", BIT(instruments, SHOW_PACKET_SIZE_METER));
    Config_save_bool(fp, "packetLossMeter", BIT(instruments, SHOW_PACKET_LOSS_METER));
    Config_save_bool(fp, "packetDropMeter", BIT(instruments, SHOW_PACKET_DROP_METER));
    Config_save_bool(fp, "slidingRadar", BIT(instruments, SHOW_SLIDING_RADAR));
    Config_save_bool(fp, "outlineWorld", BIT(instruments, SHOW_OUTLINE_WORLD));
    Config_save_bool(fp, "clock", BIT(instruments, SHOW_CLOCK));
    Config_save_int(fp, "backgroundPointDist", map_point_distance);
    Config_save_int(fp, "backgroundPointSize", map_point_size);
    Config_save_int(fp, "sparkSize", spark_size);
    Config_save_float(fp, "sparkProb", spark_prob);
    Config_save_int(fp, "receiveWindowSize", receive_window_size);
    Config_save_int(fp, "charsPerSecond", charsPerSecond);
    Config_save_bool(fp, "markingLights", markingLights);
    Config_save_bool(fp, "toggleShield", toggle_shield);
#if SOUND
    Config_save_int(fp, "maxVolume", maxVolume);
#endif
    buf[0] = '\0';
    for (i = 0, prev_key = KEY_DUMMY; i < maxKeyDefs; i++, prev_key = key) {
	ks = keyDefs[i].keysym;
	key = keyDefs[i].key;
	if ((str = XKeysymToString(ks)) == NULL) {
	    continue;
	}
	if (key != prev_key && buf[0] != '\0') {
	    if ((res = Get_keyResourceString(prev_key)) != NULL) {
		Config_save_resource(fp, res, buf);
	    }
	    buf[0] = '\0';
	}
	if (buf[0] != '\0') {
	    strcat(buf, " ");
	}
	strcat(buf, str);
    }
    Xpilotrc_end(fp);
    fclose(fp);
    sprintf(newfile, "%s/%s.bak", home, base);
    rename(oldfile, newfile);
    unlink(oldfile);
    sprintf(newfile, "%s/%s.new", home, base);
    rename(newfile, oldfile);

    if (config_save_confirm_desc != NO_WIDGET) {
	Widget_destroy(config_save_confirm_desc);
	config_save_confirm_desc = NO_WIDGET;
    }

    *strptr = (char *) button_str;
    return 1;
#endif
}

static int Config_save_confirm_callback(int widget_desc, void *popup_desc, char **strptr)
{
    if (config_save_confirm_desc != NO_WIDGET) {
	Widget_destroy((int)popup_desc);
	config_save_confirm_desc = NO_WIDGET;
    }
    return 0;
}

int Config(bool doit)
{
    if (config_created == false) {
	if (doit == false) {
	    return 0;
	}
	Create_config();
	if (config_created == false) {
	    return false;
	}
    }
    if (config_mapped == false) {
	if (doit == true) {
	    Widget_raise(config_widget_desc[config_page]);
	    config_mapped = true;
	}
    } else {
	if (doit == false) {
	    Widget_unmap(config_widget_desc[config_page]);
	    config_mapped = false;
	}
    }
    return (config_mapped == true);
}

void Config_destroy(void)
{
    int			i;

    if (config_created == true) {
	if (config_mapped == true) {
	    Widget_unmap(config_widget_desc[config_page]);
	    config_mapped = false;
	}
	for (i = 0; i < config_max; i++) {
	    Widget_destroy(config_widget_desc[i]);
	}
	config_created = false;
	free(config_widget_desc);
	config_widget_desc = NULL;
	config_max = 0;
	config_page = 0;
    }
}

void Config_resize(void)
{
    bool		mapped = config_created;

    if (config_created == true) {
	Config_destroy();
	if (mapped == true) {
	    Config(mapped);
	}
    }
}
