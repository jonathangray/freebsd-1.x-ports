/* expr.c - expression evaluation */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_expr_c_sccsid[] = "@(#)expr.c	1.31 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/ukcprog.h>

#include "objtypes.h"
#include "ups.h"
#include "symtab.h"

#include "ui.h"
#include "va.h"
#include "obj_stack.h"
#include "obj_misc.h"
#include "obj_target.h"
#include "src.h"
#include "state.h"
#include "expr.h"
#include "tdr.h"

typedef struct argst {
	char *arg_name;
	char **arg_names;
	struct argst *arg_next;
} arg_t;

/*  Arguments for find_func.
 */
typedef struct find_func_argsst {
	func_t *ffa_func;	/* argument */
	objid_t ffa_stack_obj;	/* result */
} find_func_args_t;

static int show_member PROTO((struct selst *sel, char **names, int *p_found_a_member));
static int skip_sref PROTO((const char *name, const char **p_next));
static int skip_varname PROTO((const char *name, const char *seps, const char **p_next));
static char **parse_dot_expr PROTO((const char *name_start));
static void free_names PROTO((char **names));
static arg_t *split_line PROTO((char *line));
static void free_arglist PROTO((arg_t *list));
static bool show_local PROTO((srcwin_id_t srcwin_id, func_t *f, int lnum,
							const char *name));
static bool show_global PROTO((srcwin_id_t srcwin_id, fil_t *srcfil, func_t *srcfunc,
			       const char *name,
			       bool want_errmesg, bool from_typing_line));
static int display_local PROTO((objid_t par, func_t *f, int lnum, const char *name));
static bool find_func PROTO((objid_t stack_obj, func_t *f, char *args));
static var_t *find_local_at_lnum PROTO((func_t *f, int lnum, const char *name,
								block_t **blocks));
static bool find_local PROTO((objid_t par, func_t *f, char *args));
static int find_all_locals PROTO((func_t *f, const char *name,
						       objid_t par, block_t *block));
static bool search_stack PROTO((
			bool (*func)(objid_t stack_obj, func_t *f, char *args),
			char *args));
static void show_func PROTO((srcwin_id_t srcwin_id, func_t *f));
static void show_single_member PROTO((const char *name));

/*  Versions of isalpha() and isalnum() which count '_' and '$' as letters.
 */
#define isvaralpha(c)	(isalpha(c) || (c) == '_' || (c) == '$')
#define isvaralnum(c)	(isalnum(c) || (c) == '_' || (c) == '$')

int
get_varname(fil, line, pos, bufsize, p_start, p_end)
fil_t *fil;
const char *line, *pos;
int bufsize;
const char **p_start, **p_end;
{
	const char *end;
	bool seen_alpha;
	int startpos;

	startpos = pos - line;
	seen_alpha = FALSE;
	for (startpos = pos - line; startpos >= 0; --startpos) {
		if (!isvaralnum(line[startpos]))
			break;
		if (isvaralpha(line[startpos]))
			seen_alpha = TRUE;
	}
	if (!seen_alpha)
		return FALSE;

	if (fil->fi_language == LANG_C) {
		const char *start;

		/*  While the variable is preceded by `var.' or `var->',
		 *  back over that as well.
		 */
		for (;;) {
			start = &line[startpos];

			while (startpos >= 0 && isspace(line[startpos]))
				--startpos;
			if (line[startpos] != '.' && (startpos < 1 ||
			      line[startpos-1] != '-' || line[startpos] != '>'))
				break;

			if (line[startpos] == '.')
				--startpos;
			else
				startpos -= 2;
			while (startpos >= 0 && isspace(line[startpos]))
				--startpos;

			seen_alpha = FALSE;
			for (; startpos >= 0; --startpos) {
				if (!isvaralnum(line[startpos]))
					break;
				if (isvaralpha(line[startpos]))
					seen_alpha = TRUE;
			}
			if (!seen_alpha)
				break;
		}

		startpos = start - line;
	}

	while (!isvaralpha(line[startpos]))
		++startpos;
	
	for (end = pos + 1; *end != '\0' && isvaralnum(*end); end++)
		;
	
	*p_start = &line[startpos];
	*p_end = end;
	return end - &line[startpos] < bufsize;
}

/*  Return the function that a given line number is in.
 *  We want the declarations and stuff before the first executable
 *  line of a given function to count as part of that function,
 *  so we say that all the lines from the end of the previous
 *  function to the end of the one we are looking at count as
 *  part of the function.
 */
func_t *
lnum_to_func(fil, lnum)
fil_t *fil;
int lnum;
{
	funclist_t *fl, *prevfl;

	/*  The fact that we go backwards through the list here is
	 *  a historical accident - the fl_next links used not to
	 *  be there.
	 */
	for (fl = fil->fi_funclist_tail; fl != NULL; fl = prevfl) {

		/*  Work backwards through the functions from this one,
		 *  stopping at the first function we find that has
		 *  line number information.
		 */
		for (prevfl = fl->fl_prev; prevfl != NULL; prevfl = prevfl->fl_prev)
			if (FU_LNOS(prevfl->fl_func) != NULL)
				break;

		/*  We have the right function if we have line numbers,
		 *  lnum is before the end of this function, and either
		 *  we are the first function in the file with line numbers or
		 *  lnum is after the end of the nearest function before us
		 *  that has line numbers.
		 */
		if (FU_LNOS(fl->fl_func) != NULL &&
		    lnum <= fl->fl_func->fu_max_lnum &&
		    (prevfl == NULL || lnum > prevfl->fl_func->fu_max_lnum))
			return fl->fl_func;
	}

	return NULL;
}

void
show_var(srcwin_id, fil, lnum, wholename)
srcwin_id_t srcwin_id;
fil_t *fil;
int lnum;
const char *wholename;
{
	func_t *f;
	char **names, **nptr;
	bool have_var, oldval;
	int oldstate;

#ifdef ARCH_MIPS
	if (fil->fi_language != LANG_C) {
		errf("Sorry, can only show variables in C code");
		return;
	}
#endif

	names = ssplit(wholename, ".-> \t");

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	oldval = td_set_select_recording(FALSE);

	f = lnum_to_func(fil, lnum);
	have_var = TRUE;
	if (can_get_target_vars()) {
		if (f == NULL || !show_local(srcwin_id, f, lnum, names[0]))
			have_var = show_global(srcwin_id, fil, f, names[0],
								TRUE, FALSE);
	}
	else {
		have_var = show_global(srcwin_id, fil, f, names[0], TRUE, FALSE);
	}
	
	for (nptr = names + 1; *nptr != NULL; ++nptr)
		show_single_member(*nptr);
	
	td_set_select_recording(oldval);
	td_set_obj_updating(oldstate);

	free((char *)names);
}

/*  If there is a single struct/union variable selected,
 *  and name is the name of a member of the struct/union,
 *  then add that member (if necessary) and select it.
 *
 *  This is called by show_var when the user has selected
 *  a struct/union member in the source window.
 */
static void
show_single_member(name)
const char *name;
{
	sel_t *sel;
	objid_t par, var_obj;
	int objtype;
	var_t *v;
	taddr_t addr;

	/*  Check that we have a single variable selected.
	 *  This might be false as the show_local/show_global
	 *  above could have displayed a function and not
	 *  affected the selection.
	 */
	sel = get_selection();
	if (sel == NULL || sel->se_next != NULL)
		return;
	par = sel->se_code;
	objtype = get_object_type(par);
	if (objtype != OT_VAR)
		return;

	if (!get_member_of_aggr(par, name, &v, &addr))
		return;
	if ((var_obj = find_var(par, v)) == NULL)
		var_obj = add_var_object(par, v, OBJ_FIRST_CHILD);

	clear_selection();
	select_object(var_obj, TRUE, OBJ_SELF);
	ensure_visible(var_obj);
}

static int
show_member(sel, names, p_found_a_member)
struct selst *sel;
char **names;
int *p_found_a_member;
{
	char **nptr;
	objid_t par, var_obj;
	var_t *v;
	taddr_t addr;
	int selected_one, found_a_member;

	selected_one = found_a_member = FALSE;
	for (; sel != NULL; sel = sel->se_next) {
		par = sel->se_code;
		var_obj = NULL;
		for (nptr = names; *nptr != NULL; nptr++) {
			if (!get_member_of_aggr(par, *nptr + 1, &v, &addr))
				break;
			found_a_member = TRUE;
			if (addr == 0)
				break;
			if ((var_obj = find_var(par, v)) == NULL)
				var_obj = add_var_object(par, v, OBJ_FIRST_CHILD);
			set_no_indent(var_obj, **nptr == '@');
			par = var_obj;
		}
		if (var_obj != NULL) {
			select_object(var_obj, TRUE, OBJ_SELF);
			selected_one = TRUE;
		}
		sel->se_user = TRUE;
	}
	if (found_a_member)
		*p_found_a_member = TRUE;
	return selected_one;
}

static int
skip_sref(name, p_next)
const char *name, **p_next;
{
	if (*name == '.' || *name == '@')
		*p_next = name + 1;
	else if (*name == '-' && name[1] == '>')
		*p_next = name + 2;
	else {
		errf("Expected . or -> but got %s in expression", name);
		return FALSE;
	}
	return TRUE;
}

static int
skip_varname(name, seps, p_next)
const char *name, *seps, **p_next;
{
	const char *cptr;

	for (cptr = name; *cptr != '\0' && isvaralnum(*cptr); cptr++)
		;
	if (!isvaralpha(*name) || (*cptr != '\0' && strchr(seps, *cptr) == NULL)) {
		errf("\"%s\" is not a correctly formed variable name", name);
		return FALSE;
	}
	*p_next = cptr;
	return TRUE;
}

static char **
parse_dot_expr(name_start)
const char *name_start;
{
	const char *name, *next;
	char *cptr, **names, **nptr;
	int memc, len;

	memc = 0;
	name = name_start;
	do {
		if (!skip_sref(name, &name))
			return NULL;
		memc++;
		if (!skip_varname(name, "@-. ", &next))
			return NULL;
		name = next;
	} while (*name != ' ' && *name != '\0');

	nptr = names = (char **) e_malloc((memc + 1) * sizeof(char *));

	name = name_start;
	do {
		if (!skip_sref(name, &name) || !skip_varname(name, "@-. ", &next))
			panic("bad name in parse_dot_expr");
		len = next - name;
		cptr = strncpy(e_malloc(len + 2), name - 1, len + 1);
		cptr[len + 1] = '\0';
		*nptr++ = cptr;
		name = next;
	} while (*name != ' ' && *name != '\0');
	*nptr = NULL;
	return names;
}

static void
free_names(names)
char **names;
{
	char **nptr;

	for (nptr = names; *nptr != NULL; nptr++)
		free(*nptr);
	free((char *)names);
}

static arg_t *
split_line(line)
char *line;
{
	arg_t *list, *arg;
	char *cptr;

	cptr = line;
	list = NULL;
	for (;;) {
		while (isspace(*cptr))
			cptr++;
		if (*cptr == '\0')
			break;

		arg = (arg_t *) e_malloc(sizeof(arg_t));
		arg->arg_name = cptr;
		arg->arg_names = NULL;	/* for safety */
		arg->arg_next = list;
		list = arg;

		while (*cptr != '\0' && !isspace(*cptr))
			cptr++;
		if (*cptr == '\0')
			break;
		*cptr++ = '\0';
	}
	return list;
}

static void
free_arglist(list)
arg_t *list;
{
	arg_t *next;

	for (; list != NULL; list = next) {
		next = list->arg_next;
		free((char *)list);
	}
}

void
show_var_from_typing_line(srcwin_id, name)
srcwin_id_t srcwin_id;
const char *name;
{
	arg_t *start, *list, *arg;
	struct selst *head_sel, *sel;
	char *cptr;
	int oldstate, selected_one, found_a_member, ntimes;
	bool oldval;

	oldval = td_set_select_recording(FALSE);

	if (*name != '.' && *name != '@' && (*name != '-' || name[1] != '>')) {
		if (!show_local(srcwin_id, (func_t *)NULL, 0, name))
			show_global(srcwin_id, get_displayed_fil(),
				    (func_t *)NULL, name, TRUE, TRUE);

		td_set_select_recording(TRUE);
		return;
	}

	if ((head_sel = get_unordered_selection()) == NULL) {
		errf("No objects selected");
		td_set_select_recording(TRUE);
		return;
	}
	if (get_object_type(head_sel->se_code) != OT_VAR) {
		errf("No variables selected");
		td_set_select_recording(TRUE);
		return;
	}

	for (sel = head_sel; sel != NULL; sel = sel->se_next)
		sel->se_user = FALSE;

	cptr = strsave(name);
	start = list = split_line(cptr);
	selected_one = found_a_member = FALSE;

	if (*list->arg_name == '#') {
		ntimes = atoi(list->arg_name + 1);
		list = list->arg_next;
	}
	else
		ntimes = 1;
	
	for (arg = list; arg != NULL; arg = arg->arg_next) {
		arg->arg_names = parse_dot_expr(arg->arg_name);
		if (arg->arg_names == NULL) 
			break;
	}

	if (arg != NULL) {
		arg_t *arg2;

		for (arg2 = list; arg2 != arg; arg2 = arg2->arg_next)
			free_names(arg2->arg_names);

		free(cptr);
		free_arglist(list);
		td_set_select_recording(TRUE);
		return;
	}

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	for (;;) {
		for (arg = list; arg != NULL; arg = arg->arg_next) {
			if (show_member(head_sel, arg->arg_names,
							     &found_a_member))
				selected_one = TRUE;
		}
		if (!selected_one)
			break;

		for (sel = head_sel; sel != NULL; sel = sel->se_next)
			if (sel->se_user)
				select_object(sel->se_code, FALSE, OBJ_SELF);

		if ((head_sel = get_unordered_selection()) == NULL)
			break;
		
		if (ntimes != 0 && --ntimes == 0)
			break;
	}
	
	if (!found_a_member) {
		if (list != NULL && list->arg_next != NULL)
			errf("None of the elements given are members of any selected structures");
		else
			errf("\"%s\" is not a member of any selected structure",
								list->arg_names[0]);
	}

	for (arg = list; arg != NULL; arg = arg->arg_next)
		free_names(arg->arg_names);

	td_set_obj_updating(oldstate);
	free(cptr);
	free_arglist(start);
	td_set_select_recording(TRUE);
}

static var_t *
find_local_at_lnum(f, lnum, name, blocks)
func_t *f;
int lnum;
const char *name;
block_t **blocks;
{
	int level, match_level;
	var_t *v, *match_var;
	block_t *bl;
	block_t *blocktab[MAX_BLOCK_LEVEL + 1];

	if (blocks == NULL)
		blocks = blocktab;

	bl = FU_BLOCKS(f);
	match_var = NULL;
	match_level = -1;
	level = 0;

	while (bl != NULL) {
		for (v = bl->bl_vars; v != NULL; v = v->va_next) {
			if (strcmp(v->va_name, name) == 0) {
				match_level = level;
				match_var = v;
			}
		}

		blocks[level++] = bl;
		for (bl = bl->bl_blocks; bl != NULL; bl = bl->bl_next)
			if (bl->bl_start_lnum <= lnum && lnum <= bl->bl_end_lnum)
				break;
	}
	
	blocks[match_level + 1] = NULL;
	return match_var;
}

static int
display_local(par, f, lnum, name)
objid_t par;
func_t *f;
int lnum;
const char *name;
{
	block_t *blocks[MAX_BLOCK_LEVEL + 1];
	var_t *v;
	objid_t var_obj, child;
	int oldstate, level, nmatches_at_this_level;

	/*  We don't store the found variable, as we rescan the list
	 *  looking for multiple matches if we find one.
	 */
	if (find_local_at_lnum(f, lnum, name, blocks) == NULL)
		return find_local(par, f, (char *)name);
	
	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);

	/*  Note that we start level from one here, because the "block" at
	 *  level zero is the function entry in the stack, which is always
	 *  present.
	 */
	for (level = 1; blocks[level] != NULL; ++level) {
		if ((child = find_block(par, blocks[level])) == 0)
			child = add_block_object(par, blocks[level]);
		par = child;
	}
	--level;

	/*  You can get multiple declarations for a variable at one level
	 *  when a formal parameter is redeclared as a local variable, so
	 *  we check for this.
	 */
	clear_selection();
	nmatches_at_this_level = 0;
	for (v = blocks[level]->bl_vars; v != NULL; v = v->va_next) {
		if (strcmp(v->va_name, name) == 0) {
			if ((var_obj = find_var(par, v)) == NULL)
				var_obj = add_var_object(par, v, OBJ_LAST_CHILD);
			select_object(var_obj, TRUE, OBJ_SELF);
			ensure_visible(var_obj);
			++nmatches_at_this_level;
		}
	}

	td_set_obj_updating(oldstate);

	if (nmatches_at_this_level > 1)
		errf("Warning: %s declared %d times at the same level",
						name, nmatches_at_this_level);

	return TRUE;
}

static bool
find_local(par, f, name)
objid_t par;
func_t *f;
char *name;
{
	int nvars;

#ifdef ARCH_MIPS
	if (f->fu_language != LANG_C)
		return FALSE;
#endif
	if (FU_BLOCKS(f) == NULL)
		return FALSE;

	clear_selection();
	nvars = find_all_locals(f, name, par, FU_BLOCKS(f));

	if (nvars > 1)
		errf("Warning: there are %d declarations of %s in function %s",
							nvars, name, f->fu_name);
	
	return nvars != 0;
}

static int
find_all_locals(f, name, par, block)
func_t *f;
const char *name;
objid_t par;
block_t *block;
{
	int total_nvars;
	block_t *bl;
	objid_t var_obj;
	var_t *v;

	total_nvars = 0;

	for (bl = block->bl_blocks; bl != NULL; bl = bl->bl_next) {
		objid_t child, new_child;
		int nvars;

		child = find_block(par, bl);
		new_child = (child != 0) ? child : add_block_object(par, bl);

		if ((nvars = find_all_locals(f, name, new_child, bl)) != 0)
			total_nvars += nvars;
		else if (child == 0)
			remove_object(new_child, OBJ_SELF);
	}

	var_obj = NULL;
	for (v = block->bl_vars; v != NULL; v = v->va_next) {
		if (strcmp(v->va_name, name) == 0) {
			if ((var_obj = find_var(par, v)) == NULL)
				var_obj = add_var_object(par, v, OBJ_LAST_CHILD);
			select_object(var_obj, TRUE, OBJ_SELF);
			ensure_visible(var_obj);
			++total_nvars;
		}
	}
				
	return total_nvars;
}

static bool
find_func(stack_obj, f, args)
objid_t stack_obj;
func_t *f;
char *args;
{
	find_func_args_t *ffa;

	ffa = (find_func_args_t *)args;
	if (f == ffa->ffa_func) {
		ffa->ffa_stack_obj = stack_obj;
		return TRUE;
	}
	return FALSE;
}

static bool
search_stack(func, args)
bool (*func)PROTO((objid_t stack_obj, func_t *f, char *a_args));
char *args;
{
	if (iterate_over_stack_funcs(func, args, STK_SELECTED, STK_OUTER))
		return TRUE;
	return iterate_over_stack_funcs(func, args, STK_INNER, STK_SELECTED);
}

static bool
show_local(srcwin_id, f, lnum, name)
srcwin_id_t srcwin_id;
func_t *f;
int lnum;
const char *name;
{
	int oldstate;
	bool found;

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	if (f == NULL)
		found = search_stack(find_local, (char *)name);
	else {
		find_func_args_t ffargs;

		ffargs.ffa_func = f;
		ffargs.ffa_stack_obj = NULL;
		if (search_stack(find_func, (char *)&ffargs))
			found = display_local(ffargs.ffa_stack_obj, f, lnum, name);
		else {
			if (!show_global(srcwin_id, f->fu_fil, f, name,
								 FALSE, FALSE))
				errf("Function %s is not currently active",
									f->fu_name);
			found = TRUE;
		}
	}
	td_set_obj_updating(oldstate);
	
	return found;
}

static void
show_func(srcwin_id, f)
srcwin_id_t srcwin_id;
func_t *f;
{
	if (f->fu_fil == NULL)
		errf("Can't find source for function %s", f->fu_name);
	else if (FU_LNOS(f) == NULL)
		errf("No line number information for function %s", f->fu_name);
	else {
		if (srcwin_id != NULL)
			src_push_current_pos(srcwin_id);
		show_source(f->fu_fil, FU_LNOS(f)->ln_num, TRUE);
	}
}

static bool
show_global(srcwin_id, srcfil, srcfunc, name, want_errmesg, from_typing_line)
srcwin_id_t srcwin_id;
fil_t *srcfil;
func_t *srcfunc;
const char *name;
bool want_errmesg, from_typing_line;
{
	fil_t *fil;
	func_t *f;
	var_t *v;
	objid_t var_obj;
	objid_t obj;
	common_block_id_t cblock;
	int oldstate;

	if (find_global_by_name(name, srcfil, srcfunc, !from_typing_line,
				&f, &v, &cblock, &fil) != 0)
		return FALSE;

	if (f != NULL) {
		show_func(srcwin_id, f);
		return TRUE;
	}

	if (!can_get_target_vars()) {
		errf("Can't show variables as the target isn't running");
		return FALSE;
	}

	oldstate = td_set_obj_updating(OBJ_UPDATING_OFF);
	if (cblock != NULL) {
		add_common_block_object_if_necessary(cblock);
		obj = (objid_t)cblock;
	}
	else if (fil != NULL) {
		add_source_file_object_if_necessary(fil);
		obj = (objid_t)fil;
	}
	else
		obj = GLOBALS_OBJCODE;
	
	clear_selection();
	if ((var_obj = find_var(obj, v)) == NULL) {
		var_obj = add_var_object(obj, v, OBJ_LAST_CHILD);
		if (get_object_type(obj) == OT_SFILE) {
			change_type(obj, OT_SFILE_EX);
			sort_children(SRCHEAD_OBJCODE, src_cmp);
		}
	}
	select_object(var_obj, TRUE, OBJ_SELF);
	ensure_visible(var_obj);
	td_set_obj_updating(oldstate);

	return TRUE;
}
