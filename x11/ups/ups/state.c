/* state.c - set/get ups state variables */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_state_c_sccsid[] = "@(#)state.c	1.9 26/4/92 (UKC)";

#include <local/wn.h>
#include <local/menu3.h>

#include <local/ukcprog.h>

#include <local/obj/fed.h>
#include "src.h"
#include "output.h"
#include "reg.h"
#include "obj_target.h"
#include "state.h"

static struct {
	int st_message_wn;
	int st_display_area_wn;
	target_menu_info_t st_target_menu_info;
	region_id_t st_current_srcwin_region;
	outwin_id_t st_current_outwin;
	region_id_t st_dynamic_menu_region;
	tstate_t st_target_state;
	edesc_t *st_typing_line_edesc;
	int st_tabwidth;
} State;

void
set_tabwidth(tabwidth)
int tabwidth;
{
	State.st_tabwidth = tabwidth;
}

int
get_tabwidth()
{
	return State.st_tabwidth;
}

void
set_target_menu_info(tm)
target_menu_info_t *tm;
{
	State.st_target_menu_info = *tm;
}

target_menu_info_t *
get_target_menu_info()
{
	return &State.st_target_menu_info;
}

void
set_message_wn(wn)
int wn;
{
	State.st_message_wn = wn;
}

int
get_message_wn()
{
	return State.st_message_wn;
}

void
set_display_area_wn(wn)
int wn;
{
	State.st_display_area_wn = wn;
}

int
get_display_area_wn()
{
	return State.st_display_area_wn;
}

void
set_current_outwin(outwin_id)
outwin_id_t outwin_id;
{
	State.st_current_outwin = outwin_id;
}

outwin_id_t
get_current_outwin()
{
	return State.st_current_outwin;
}

void
set_current_srcwin_region(region_id)
region_id_t region_id;
{
	State.st_current_srcwin_region = region_id;
}

region_id_t
get_current_srcwin_region()
{
	return State.st_current_srcwin_region;
}

srcwin_id_t
get_current_srcwin()
{
	return (srcwin_id_t)re_get_data(State.st_current_srcwin_region);
}

void
set_typing_line_edesc(edesc)
edesc_t *edesc;
{
	State.st_typing_line_edesc = edesc;
}

edesc_t *
get_typing_line_edesc()
{
	return State.st_typing_line_edesc;
}

const char *
get_typing_line_string()
{
	return State.st_typing_line_edesc->ed_copy;
}

void
set_dynamic_menu_region(region_id)
region_id_t region_id;
{
	State.st_dynamic_menu_region = region_id;
}

region_id_t
get_dynamic_menu_region()
{
	return State.st_dynamic_menu_region;
}

void
set_target_state(state)
tstate_t state;
{
	State.st_target_state = state;
}

tstate_t
get_target_state()
{
	return State.st_target_state;
}
