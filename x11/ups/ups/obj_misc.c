/* obj_misc.c - miscellaneous object related stuff */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_misc_c_sccsid[] = "@(#)obj_misc.c	1.20 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/menu3.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "objtypes.h"

#include "obj_target.h"
#include "obj_misc.h"
#include "ui.h"
#include "va.h"
#include "obj_stack.h"
#include "menudata.h"
#include "tdr.h"

static int cblock_cmp PROTO((objid_t obj1, objid_t obj2));
static void set_max_src_width PROTO((fil_t *fil));

const char Srchead_format[] = "Source files\n";

const char Sfile_ex_format[] = "\n%[-]s\n";
#define FN_SFILE_NAME	0

const char Globals_format[] = "Untyped variables\n";

const char Fhead_format[] = "Functions\n";

int
header_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	const char *name;
	int len;

	name = (char *)code;
	len = strlen(name) - 1;

	if (len <= 0 || name[len] != '\n')
		panic("name botch in hd");

	return td_outf(level, "%.*s", len, name);
}

/*  Add a header object for the global variables.
 */
void
add_globals_header(par)
objid_t par;
{
	new_object(GLOBALS_OBJCODE, OT_GLOBALS, par, OBJ_CHILD);
}

/*  Process the return from the globals menu. just display and conceal
 *  variables.
 */
void
do_globals(obj, command)
objid_t obj;
int command;
{
	switch(command) {
	case MR_SHOW_UNTYPED_VARS:
		if (can_get_target_vars())
			addvars(obj);
		else
			errf("Target not running");
		break;
	case MR_HIDE_UNTYPED_VARS:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	default:
		panic("bad cmd in dg");
	}
}

const char Cblock_format[] = "%[-]16cs %[-]60cs\n";

#define FN_CBLOCK_NAME	0
#define FN_CBLOCK_DFUNC	1

const char Cbhead_format[] = "Common blocks\n";

void
add_common_block_object_if_necessary(cblock)
common_block_id_t cblock;
{
	char *buf, *fnbuf;
	const char *name, *dfunc_name;
	int len;

	if (obj_child_exists(CBHEAD_OBJCODE, (objid_t)cblock))
		return;

	name = get_cblock_name(cblock);
	dfunc_name = get_cblock_funcname(cblock);

	if (strcmp(name, "_BLNK__") == 0)
		buf = strsave("Blank common");
	else {
		len = strlen(name);
		if (name[len - 1] == '_')
			--len;
		buf = strf("common /%.*s/", len, name);
	}

	if (dfunc_name == NULL)
		fnbuf = strsave("");
	else
		fnbuf = strf("(as defined in %s)", dfunc_name);

	new_object((objid_t)cblock, OT_CBLOCK, CBHEAD_OBJCODE, OBJ_CHILD);
	set_field_value((objid_t)cblock, FN_CBLOCK_NAME, (fval_t)buf);
	set_field_value((objid_t)cblock, FN_CBLOCK_DFUNC, (fval_t)fnbuf);
}

const char *
cblock_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	const char *name, *dfunc_name;

	name = get_cblock_name((common_block_id_t)obj);
	dfunc_name = get_cblock_funcname((common_block_id_t)obj);

	if (last != NULL)
		free(last);

	if (dfunc_name == NULL) {
		last = NULL;
		return name;
	}

	last = strf("%s[%s]", name, dfunc_name);
	return last;
}


void
free_common_block_object(obj)
objid_t obj;
{
	free((char *)get_field_value(obj, FN_CBLOCK_NAME));
	free((char *)get_field_value(obj, FN_CBLOCK_DFUNC));
}

static int
cblock_cmp(obj1, obj2)
objid_t obj1, obj2;
{
	return strcmp(get_cblock_name((common_block_id_t)obj1),
		      get_cblock_name((common_block_id_t)obj2));
}

void
do_cbhead(obj, command)
objid_t obj;
int command;
{
	switch(command) {
	case MR_SHOW_COMMON_BLOCKS:
		iterate_over_common_blocks(add_common_block_object_if_necessary);
		sort_children(obj, cblock_cmp);
		break;
	case MR_HIDE_ALL_COMMON_BLOCKS:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	default:
		panic("bad cmd in dch");
	}
}

int
cblock_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%s %s",
				get_field_value(code, FN_CBLOCK_NAME),
				get_field_value(code, FN_CBLOCK_DFUNC));
}

void
do_cblock(obj, command)
objid_t obj;
int command;
{
	switch(command) {
	case MR_EXPAND_COMMON_BLOCK:
		if (can_get_target_vars())
			addvars(obj);
		else
			errf("Target not running");
		break;
	case MR_COLLAPSE_COMMON_BLOCK:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	case MR_HIDE_COMMON_BLOCK:
		remove_object(obj, OBJ_DESCENDENTS);
		remove_object(obj, OBJ_SELF);
		break;
	default:
		panic("bad cmd in dcb");
	}
}

int
src_cmp(obj1, obj2)
objid_t obj1, obj2;
{
	int ch1, ch2;

	ch1 = get_code(obj1, OBJ_CHILD) != NULL;
	ch2 = get_code(obj2, OBJ_CHILD) != NULL;
	if (ch1 != ch2)
		return ch1 ? 1 : -1;
	return strcmp(((fil_t *)obj1)->fi_name, ((fil_t *)obj2)->fi_name);
}

/*  Width in pixels of the longest source file name.
 */
static int Max_src_width = 0;

/*  Horizontal gap in pixels between source file objects.
 */
#define SRC_HGAP	10

/*  Process the return from the source header menu.
 *  Add or remove source files from the display.
 */
void
do_srchead(obj, command)
objid_t obj;
int command;
{
	switch(command) {
	case MR_SHOW_SOURCE_FILES:
		iterate_over_source_files(add_source_file_object_if_necessary);
		sort_children(obj, src_cmp);
		break;
	case MR_HIDE_SOURCE_FILES:
		remove_object(obj, OBJ_DESCENDENTS);
		break;
	}
}

void
add_source_file_object_if_necessary(fil)
fil_t *fil;
{
	if (obj_child_exists(SRCHEAD_OBJCODE, (objid_t)fil))
		return;

	/*  The compilers emit an N_SO symbol for libg.s - don't
	 *  show this to the user.
	 */
	if (strcmp(fil->fi_name, "libg.s") == 0)
		return;

	new_object((objid_t)fil, OT_SFILE, SRCHEAD_OBJCODE, OBJ_CHILD);
	set_field_value((objid_t)fil, FN_SFILE_NAME, (fval_t)fil->fi_name);
}

static void
set_max_src_width(fil)
fil_t *fil;
{
	int width;

	width = wn_strwidth(fil->fi_name, (font_t *)NULL);
	if (width > Max_src_width)
		Max_src_width = width;
}

/*  Add the variables to the display.
 */
int
addvars(par)
objid_t par;
{
	fil_t *fi;
	var_t *v;

	/*  Add the variables to the list
	 */
	switch(ups_get_object_type(par)) {
	case OT_SFILE:
		fi = (fil_t *)par;
		if (FI_VARS(fi) == NULL) {
			errf("No global variables in source file %s",
									fi->fi_name);
			return -1;
		}
		v = FI_VARS(fi);
		break;
	case OT_CBLOCK:
		v = get_cblock_vars((common_block_id_t)par);
		break;
	default:
		panic("unknown object type in addvars");
		v = NULL; /* to satisfy gcc */
	}

	for (; v != NULL; v = v->va_next)
		if (find_var(par, v) == NULL)
			(void) add_var_object(par, v, OBJ_FIRST_CHILD);
	return 0;
}

/*  Return the width in pixels of a source file object.
 *
 *  A pointer to this function is passed to the obj library.
 */
/* ARGSUSED */
void
srcfile_getsize(unused_obj, unused_par, sz)
objid_t unused_obj, unused_par;
struct szst *sz;
{
	static bool first_call = TRUE;

	if (first_call) {
		iterate_over_source_files(set_max_src_width);
		first_call = FALSE;
	}

	sz->sz_depth = wn_get_sysfont()->ft_height + 1;
	sz->sz_width = Max_src_width + SRC_HGAP;
}


const char *
srcfile_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	const char *name;
	objid_t obj2;
	int count;

	name = ((fil_t *)obj)->fi_name;

	obj2 = get_code(SRCHEAD_OBJCODE, OBJ_CHILD);

	count = 1;
	for (; obj2 != obj && obj2 != NULL; obj2 = get_code(obj2, OBJ_NEXT)) {
		if (strcmp(((fil_t *)obj2)->fi_name, name) == 0)
			++count;
	}
	
	if (last != NULL)
		free(last);

	if (count == 1) {
		last = NULL;
		return name;
	}
	else {
		last = strf("%d-%s", count, name);
		return last;
	}
}

int
file_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%s", ((fil_t *)code)->fi_name);
}

/*  Process the return from a source file menu. Either show the variables
 *  or call the editor.
 */
void
do_file(par, command)
objid_t par;
int command;
{
	fil_t *fil;

	fil = (fil_t *)par;

	switch(command) {
	case MR_ADD_EXPRESSION:
		if (!can_get_target_vars()) {
			errf("Target not running");
			return;
		}

		change_type(par, OT_SFILE_EX);
		sort_children(SRCHEAD_OBJCODE, src_cmp);
		ensure_visible(par);

		get_fi_vars(fil);
		add_expr_object(par, fil->fi_block, fil->fi_language);

		if (get_code(par, OBJ_CHILD) == NULL) {
			change_type(par, OT_SFILE);
			sort_children(SRCHEAD_OBJCODE, src_cmp);
			ensure_visible(par);
		}
		break;
	case MR_ADD_VARS:
		if (!can_get_target_vars()) {
			errf("Target not running");
			return;
		}


		addvars(par);
		if (get_code(par, OBJ_CHILD) != NULL)
			change_type(par, OT_SFILE_EX);

		sort_children(SRCHEAD_OBJCODE, src_cmp);
		ensure_visible(par);

		break;
	case MR_HIDE_VARS:
		remove_object(par, OBJ_DESCENDENTS);
		change_type(par, OT_SFILE);
		sort_children(SRCHEAD_OBJCODE, src_cmp);
		ensure_visible(par);
		break;
	case MR_DISPLAY_SOURCE:
		show_source(fil, 1, FALSE);
		break;
	default:
		panic("bad cmd in df");
	}
}

void
hide_source_vars()
{
	objid_t obj;

	obj = get_code(SRCHEAD_OBJCODE, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT)) {
		remove_object(obj, OBJ_DESCENDENTS);
		change_type(obj, OT_SFILE);
	}
	sort_children(SRCHEAD_OBJCODE, src_cmp);
}

objid_t
find_or_add_object(par, wanted, add_object)
objid_t par, wanted;
void (*add_object)PROTO((objid_t wobj));
{
	objid_t obj;

	obj = get_code(par, OBJ_CHILD);
	for (; obj != NULL; obj = get_code(obj, OBJ_NEXT))
		if (obj == wanted)
			break;
	if (obj == NULL)
		(*add_object)(wanted);
	return wanted;
}
