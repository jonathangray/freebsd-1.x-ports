/* ui_menu.c - menu display and input handling */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ui_menu_c_sccsid[] = "@(#)ui_menu.c	1.24 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdlib.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/menu3.h>

#include <local/ukcprog.h>
#include "objtypes.h"
#include "ui.h"
#include "ui_priv.h"

#include "exec.h"
#include "obj_target.h"
#include "reg.h"
#include "tdr.h"
#include "state.h"
#include "menudata.h"
#include "cursors.h"

static void set_cur_objtype PROTO((int type));
static void sync_dynamic_menu_with_cur_objtype PROTO((void));

typedef struct newselst {
	objid_t ns_obj;
	struct newselst *ns_next;
} newsel_t;

static newsel_t *New_selection = NULL;

/*  Display the popup menu described by po at x,y in window wn,
 *  do the loop to allow the user to select from it, the remove
 *  the menu.
 *
 *  Return the index of the item selected, or -1 if no item
 *  was selected.
 */
int
select_from_popup(wn, button, po, x, y)
int wn, button;
popup_t *po;
int x, y;
{
	font_t *menufont;
	int md, rv, mask, w, maxw;
	const char **p_str;

	menufont = Mstdfont();
	md = po->po_mdesc;
	if (md == -1) {
		maxw = 0;
		for(p_str = po->po_caps; *p_str != NULL; p_str++)
			if ((w = wn_strwidth(*p_str, menufont)) > maxw)
				maxw = w; 
		md = Mmake(po->po_caps, (int *)NULL, (int *)NULL, MM_DRAG, maxw);
		if (md == -1)
			panic("mmake failed in select_from_popup");
		Msize(md, maxw+10, (p_str - po->po_caps) *
						     (menufont->ft_height + 4));
		Mfmodes(md, MH_GREY, MH_BLACK|MH_BOLD, MH_GREY);
	}
	mask = ~(1 << md);

	Mplace(md, x - 10, y - po->po_last * (menufont->ft_height + 4) -
					         (menufont->ft_height + 2) / 2);
	Mdisplay(md, wn, TRUE); 
	Mselect(x, y, wn, MS_PRESSED, mask);
	while (wn_getpuck(wn, &x, &y) & button)
		(void) Mselect(x, y, wn, MS_CONTINUE, mask);
	rv = MS_rv(Mselect(x, y, wn, MS_RELEASED, mask)) - 1;
	Mremove(md);
	if (rv != -1 && po->po_save_last)
		po->po_last = rv;
	po->po_mdesc = md;
	return rv;
}

/*  Display open menu md in all of window wn.
 */
void
show_menu(md, wn)
int md, wn;
{
	int w, h;

	wn_get_window_size(wn, &w, &h);
	Mplace(md, 0, 0);
	Msize(md, w, h);
	Mdisplay(md, wn, FALSE);
}

#define INITIAL_OBJTYPE	OT_NO_TYPE

static int Cur_objtype = INITIAL_OBJTYPE;

static dmu_state_t Dynamic_menu_updating_state = DMU_ON;

void
updating_callback_func(oldstate, newstate)
int oldstate, newstate;
{
	if (oldstate != newstate) {
		dmu_state_t dmu_state;

		dmu_state = (newstate == OBJ_UPDATING_ON) ? DMU_ON : DMU_OFF;
		set_dynamic_menu_updating_state(dmu_state);
	}
							
}

static void
sync_dynamic_menu_with_cur_objtype()
{
	static int old_cur_objtype = INITIAL_OBJTYPE;

	if (Cur_objtype != old_cur_objtype) {
		if (Cur_objtype == OT_NO_TYPE)
			set_dynamic_menu(-1, (const char *)NULL);
		else
			set_dynamic_menu(Objtab[Cur_objtype].ot_md,
					 Objtab[Cur_objtype].ot_menuname);
		old_cur_objtype = Cur_objtype;
	}
}

void
set_dynamic_menu_updating_state(new_state)
dmu_state_t new_state;
{
	Dynamic_menu_updating_state = new_state;
	if (new_state == DMU_ON)
		sync_dynamic_menu_with_cur_objtype();
}

static void
set_cur_objtype(type)
int type;
{
	Cur_objtype = type;
	if (Dynamic_menu_updating_state == DMU_ON)
		sync_dynamic_menu_with_cur_objtype();
}

int
get_cur_objtype()
{
	return Cur_objtype;
}

/*  Return TRUE if object obj can be selected.
 */
int
can_select(obj)
objid_t obj;
{
	int objtype;

	if (Cur_objtype == OT_NO_TYPE)
		return TRUE;
	
	objtype = ups_get_object_type(obj);
	if (objtype == Cur_objtype)
		return TRUE;
	
	return (objtype == OT_VAR && Cur_objtype == OT_EXPR) ||
	       (objtype == OT_EXPR && Cur_objtype == OT_VAR);
}

/*  Generic selection handling function for all the ups object
 *  types.  Highlighting is simply by inverting foreground and
 *  background colors for the whole object.
 */
void
gen_select(wn, obj, x, y, width, height, flags)
int wn;
objid_t obj;
int x, y, width, height, flags;
{
	int type, nselected;

	if (flags & SEL_VISIBLE)
		wn_invert_area(wn, x, y, width, height);

	if ((flags & SEL_CHANGING) && (nselected = get_num_selected()) <= 1) {
		if (nselected == 0)
			set_cur_objtype(OT_NO_TYPE);
		else if (nselected == 1) {
			type = ups_get_object_type(obj);
			if (type < 0 || type > OT_MAXTYPE)
				panic("bad objtype in gen_select");
			set_cur_objtype(type);
		}
		else
			panic("bad nsel in gs");
	}

	td_record_select(obj, flags);
}

/*  Mousehole caption function for the dynamic menu.  Return 0 (i.e.
 *  no mousehole captions) if there are no objects selected, and hence
 *  no dynamic menu displayed.
 */
int
mfn_dmenu(caps, unused_arg)
int caps;
char *unused_arg;
{
	return (Cur_objtype == OT_NO_TYPE) ? 0 : caps;
}

/*  Input handling function for the dynamic menu.
 */
void
dynamic_menu_func(unused_data, md, rv)
char *unused_data;
int md, rv;
{
	void (*mfunc)PROTO((objid_t obj, int mfrv));
	sel_t *sel;

	sel = get_selection();

	if (Cur_objtype == OT_NO_TYPE || get_num_selected() == 0)
		panic("dmf called with no obj selected");
	if (Cur_objtype < 0 || Cur_objtype > OT_MAXTYPE)
		panic("cur_objtype bad in rd");
	mfunc = Objtab[Cur_objtype].ot_mfunc;

	if (md != -1)
		Mclear(md);
	td_set_obj_updating(OBJ_UPDATING_OFF);

	for (sel = get_selection(); sel != NULL; sel = sel->se_next)
		(*mfunc)(sel->se_code, rv);

	if (New_selection != NULL) {
		newsel_t *ns, *next;

		clear_selection();
		for (ns = New_selection; ns != NULL; ns = next) {
			select_object(ns->ns_obj, TRUE, OBJ_SELF);
			next = ns->ns_next;
			free((char *)ns);
		}
		New_selection = NULL;
	}

	td_set_obj_updating(OBJ_UPDATING_ON);
}

void
add_to_new_selection(obj)
objid_t obj;
{
	newsel_t *ns;

	ns = (newsel_t *)e_malloc(sizeof(newsel_t));
	ns->ns_obj = obj;
	ns->ns_next = New_selection;
	New_selection = ns;
}

/*  Input handling function for the target control menu.
 */
/* ARGSUSED */
void
target_menu_func(unused_data, md, rv)
char *unused_data;
int md, rv;
{
	target_menu_info_t *tm;
	cursor_t old_cursor;

	tm = get_target_menu_info();
	tm->tm_current_md = md;
	set_target_state(TS_RUNNING);
	update_target_menu_state();

	old_cursor = wn_get_window_cursor(WN_STDWIN);
	set_bm_cursor(WN_STDWIN, CU_WAIT);

	do_menu_target_command(rv);

	wn_define_cursor(WN_STDWIN, old_cursor);

	tm->tm_current_md = -1;
	Mclear(md);
	update_target_menu_state();
}

bool
user_wants_stop()
{
	int wn, mask, rv, md;
	tstate_t oldstate;
	target_menu_info_t *tm;
	event_t event;

	tm = get_target_menu_info();
	md = tm->tm_mdtab[(int)TM_STOP].md;
	wn = tm->tm_mdtab[(int)TM_STOP].wn;

	wn_next_event(wn, EVENT_MASK, &event);
	if ((event.ev_buttons & B_LEFT) == 0) {
		if (event.ev_type == EV_WINDOW_EXPOSED ||
						event.ev_type == EV_WINDOW_RESIZED) {
			int i;

			re_redraw_root(event.ev_type, FALSE);

			for (i = 0; i < (int)TM_NTAGS; ++i)
				if (tm->tm_mdtab[i].md == tm->tm_current_md)
					break;
			if (i < (int)TM_NTAGS) {
				int msmask;

				msmask = !(1 << tm->tm_current_md);
				Mselect(1, 1, tm->tm_mdtab[i].wn,
							MS_PRESSED, msmask);
				Mselect(1, 1, tm->tm_mdtab[i].wn,
							MS_RELEASED, msmask);
			}
		}
		return FALSE;
	}
	
	Mclear(md);
	oldstate = get_target_state();
	set_target_state(TS_RUNNING);
	update_target_menu_state();
	set_target_state(oldstate);

	mask = ~(1 << md);
	if (!Mselect(event.ev_x, event.ev_y, wn, MS_PRESSED, mask))
		return FALSE;

	for (;;) {
		wn_next_event(wn, EVENT_MASK, &event);
		if ((event.ev_buttons & B_LEFT) == 0)
			break;
		Mselect(event.ev_x, event.ev_y, wn, MS_CONTINUE, mask);
		wn_show_updates(wn);
	}
	rv = MS_rv(Mselect(event.ev_x, event.ev_y, wn, MS_RELEASED, mask));

	Mclear(md);

	return (rv == 'S');
}


void
update_target_menu_state()
{
	static const char *last_nrel[(int)TM_NTAGS];
	const char *nrel[(int)TM_NTAGS];
	target_menu_info_t *tm;
	int i;

	tm = get_target_menu_info();
	switch(get_target_state()) {
	case TS_RUNNING:
		nrel[(int)TM_START] = "r";
		nrel[(int)TM_NEXT] =  "n";
		nrel[(int)TM_STEP] =  "s";
		nrel[(int)TM_CONT] =  "c";
		nrel[(int)TM_STOP] =  "";
		nrel[(int)TM_EXIT] =  "e";
		break;
	case TS_NOTR:
	case TS_CORE:
		nrel[(int)TM_START] = "";
		nrel[(int)TM_NEXT] =  "n";
		nrel[(int)TM_STEP] =  "s";
		nrel[(int)TM_CONT] =  "c";
		nrel[(int)TM_STOP] =  "S";
		nrel[(int)TM_EXIT] =  "e";
		break;
	case TS_STOPPED_AND_CANT_CONTINUE:
		nrel[(int)TM_START] = "r";
		nrel[(int)TM_NEXT] =  "n";
		nrel[(int)TM_STEP] =  "s";
		nrel[(int)TM_CONT] =  "c";
		nrel[(int)TM_STOP] =  "S";
		nrel[(int)TM_EXIT] =  "";
		break;
	case TS_STOPPED_AND_CAN_CONTINUE:
		nrel[(int)TM_START] = "r";
		nrel[(int)TM_NEXT] =  "";
		nrel[(int)TM_STEP] =  "";
		nrel[(int)TM_CONT] =  "";
		nrel[(int)TM_STOP] =  "S";
		nrel[(int)TM_EXIT] =  "";
		break;
	default:
		panic("unknown target state");
	}

	wn_updating_off(WN_STDWIN);
	for (i = 0; i < (int)TM_NTAGS; ++i) {
		if (tm->tm_mdtab[i].md != tm->tm_current_md &&
							nrel[i] != last_nrel[i]) {
			Mnonsel(tm->tm_mdtab[i].md, nrel[i]);
			last_nrel[i] = nrel[i];
		}
	}
	wn_updating_on(WN_STDWIN);
}

/*  Menu function for the permanent menu (the quit button).
 */
void
permanent_menu_func(unused_data, md, rv)
char *unused_data;
int md, rv;
{
	switch(rv) {
	case MR_QUIT_UPS:
		re_set_exit_event_loop_flag();
		break;
	case MR_DONT_QUIT:
		break;
	default:
		panic("bad cmd in pmf");
	}

	if (md != -1)
		Mclear(md);
}
