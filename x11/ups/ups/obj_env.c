/* obj_env.c - target environment handling routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_obj_env_c_sccsid[] = "@(#)obj_env.c	1.9 26/7/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <local/ukcprog.h>
#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/obj/fed.h>

#include "objtypes.h"
#include "ui.h"
#include "obj_env.h"
#include "menudata.h"
#include "tdr.h"

extern const char **environ;

/*  An element in the circular doubly linked list of environment entries.
 */
typedef struct enventryst {
	struct enventryst *ee_prev;
	struct enventryst *ee_next;
	char *ee_value;
	bool ee_hidden;
} enventry_t;

static void make_envtab PROTO((void));
static void add_env_object PROTO((enventry_t *ee));
static void free_envtab PROTO((void));
static int env_quitfunc PROTO((edesc_t *edesc, int unused_n_tries));
static void env_edit PROTO((draw_t fdets));
static enventry_t *search_envtab PROTO((const char *s, int len,
							enventry_t *orig_ee));
static void add_env_entry PROTO((enventry_t *orig));

/*  Maximum length of an individual environment string.
 */
#define MAX_ENV_LEN	4096

const char Env_format[] = "%[-]120cE\n";

fdef_t Env_fdefs[] = {
	'E', s_draw, env_edit, NULL,
	'\0', NULL, NULL, NULL,
};

static enventry_t *Envtab = NULL;

#define FN_ENV_VALUE		0

fnamemap_t Env_fnamemap[] = {
	"env-var-value",	FN_ENV_VALUE,
	NULL,			0,
};

const char Envhead_format[] = "Environment\n";

#define ENVHEAD_OBJCODE	((objid_t)Envhead_format)

const char *
env_getobjname(obj)
objid_t obj;
{
	static char *last = NULL;
	enventry_t *ee, *ee2;
	int len, count;
	char *name;
	const char *s;

	ee = (enventry_t *)obj;

	for (s = ee->ee_value; *s != '\0' && *s != '='; ++s)
		;
	if (*s == '=')
		++s;
	len = s - ee->ee_value;
	
	name = e_malloc(len + 1);
	memcpy(name, ee->ee_value, len);
	name[len] = '\0';

	count = 1;
	for (ee2 = Envtab->ee_next; ee2 != Envtab; ee2 = ee2->ee_next) {
		if (ee2 == ee)
			break;
		if (strncmp(ee2->ee_value, name, len) == 0)
			++count;
	}

	if (last == NULL)
		free(last);

	if (count == 1) {
		last = name;
	}
	else {
		last = strf("%d-%s", count, name);
		free(name);
	}

	return last;
}

/*  Add the signal list header to the form.
 */
void
add_env_header(par)
objid_t par;
{
	new_object(ENVHEAD_OBJCODE, OT_ENVHEAD, par, OBJ_CHILD);
}

void
free_env(obj)
objid_t obj;
{
	enventry_t *ee;

	ee = (enventry_t *)obj;

	if (!ee->ee_hidden) {
		ee->ee_prev->ee_next = ee->ee_next;
		ee->ee_next->ee_prev = ee->ee_prev;
		free(ee->ee_value);
		free((char *)ee);
	}
}

void
env_getsize(obj, unused_par, sz)
objid_t obj, unused_par;
struct szst *sz;
{
	font_t *font;
	const char *s;

	font = wn_get_sysfont();

	sz->sz_depth = font->ft_height;

	/*  We don't want a zero width object for an empty environment string.
	 */
	s = ((enventry_t *)obj)->ee_value;
	sz->sz_width = *s == '\0' ? font->ft_width / 2
				  : font->ft_width * strlen(s);
}

/*  Process the return from the signal header menu. Expand or collapse
 *  the signal list
 */
void
do_envhead(obj, command)
objid_t obj;
int command;
{
	enventry_t *ee;

	switch(command) {
	case MR_ADD_ENV_ENTRY:
		if (Envtab == NULL)
			make_envtab();
		add_env_entry(Envtab);
		break;
	case MR_RESET_ENVTAB:
		do_envhead(obj, MR_HIDE_ALL_ENV_ENTRIES);
		if (Envtab != NULL)
			free_envtab();
		do_envhead(obj, MR_SHOW_ALL_ENV_ENTRIES);
		break;
	case MR_SHOW_ALL_ENV_ENTRIES:
		if (Envtab == NULL)
			make_envtab();
		for (ee = Envtab->ee_next; ee != Envtab; ee = ee->ee_next) {
			if (ee->ee_hidden)
				add_env_object(ee);
		}
		break;
	case MR_HIDE_ALL_ENV_ENTRIES:
		if (Envtab != NULL) {
			for (ee = Envtab->ee_next; ee != Envtab; ee = ee->ee_next)
				ee->ee_hidden = TRUE;
			remove_object(obj, OBJ_DESCENDENTS);
		}
		break;
	default:
		panic("bad cmd in dsh");
	}
}

const char **
get_environment()
{
	static const char **etab = NULL;
	static int etab_size = 0;
	enventry_t *ee;
	int nenv, i;

	if (Envtab == NULL)
		return environ;
	
	nenv = 1;	/* For the final NULL */
	for (ee = Envtab->ee_next; ee != Envtab; ee = ee->ee_next)
		++nenv;
	
	if (nenv > etab_size) {
		if (etab != NULL)
			free((char *)etab);
		etab_size = nenv + 10;		/* add a little slop */
		etab = (const char **)e_malloc(etab_size * sizeof(char *));
	}

	i = 0;
	for (ee = Envtab->ee_next; ee != Envtab; ee = ee->ee_next)
		etab[i++] = ee->ee_value;
	etab[i] = NULL;

	return etab;
}

/*  Build an initial environment list from the environ supplied
 *  environment.
 */
static void
make_envtab()
{
	const char **envp;

	if (Envtab != NULL)
		panic("et botch in me");
	
	Envtab = (enventry_t *)e_malloc(sizeof(enventry_t));
	Envtab->ee_next = Envtab->ee_prev = Envtab;

	for (envp = environ; *envp != NULL; ++envp) {
		enventry_t *ee;

		ee = (enventry_t *)e_malloc(sizeof(enventry_t));
		ee->ee_value = strsave(*envp);
		ee->ee_hidden = TRUE;

		/*  Link the new entry in at the tail of the list.
		 *  This is so we end up with the list in the same order
		 *  as the entries in environ.
		 */
		ee->ee_next = Envtab;
		ee->ee_prev = Envtab->ee_prev;
		ee->ee_prev->ee_next = ee;
		Envtab->ee_prev = ee;
	}
}

static void
add_env_entry(orig)
enventry_t *orig;
{
	enventry_t *ee;
	int oldstate;

	if (Envtab == NULL)
		panic("et botch in mkee");

	ee = (enventry_t *)e_malloc(sizeof(enventry_t));
	ee->ee_value = strsave("");
	ee->ee_hidden = TRUE;

	ee->ee_prev = orig;
	ee->ee_next = orig->ee_next;
	ee->ee_next->ee_prev = ee;
	orig->ee_next = ee;

	add_env_object(ee);
	ensure_visible((objid_t)ee);

	oldstate = td_set_obj_updating(OBJ_UPDATING_ON);
	td_obj_edit_field((objid_t)ee, FN_ENV_VALUE, 0, 0);
	if (ee->ee_hidden) {
		ee->ee_hidden = FALSE;
		remove_object((objid_t)ee, OBJ_SELF);
	}
	td_set_obj_updating(oldstate);
}

static void
free_envtab()
{
	enventry_t *ee, *next;

	for (ee = Envtab->ee_next; ee != Envtab; ee = next) {
		next = ee->ee_next;
		free(ee->ee_value);
		free((char *)ee);
	}

	free((char *)Envtab);
	Envtab = NULL;
}

static void
add_env_object(ee)
enventry_t *ee;
{
	if (ee->ee_prev == Envtab)
		new_object((objid_t)ee, OT_ENV, ENVHEAD_OBJCODE, OBJ_FIRST_CHILD);
	else
		new_object((objid_t)ee, OT_ENV, (objid_t)ee->ee_prev, OBJ_AFTER);

	set_field_value((objid_t)ee, FN_ENV_VALUE, (fval_t)ee->ee_value);
	ee->ee_hidden = FALSE;
}

static int
env_quitfunc(edesc, unused_n_tries)
edesc_t *edesc;
int unused_n_tries;
{
	enventry_t *ee;
	const char *pos;

	ee = (enventry_t *)edesc->ed_user;

	free(ee->ee_value);
	ee->ee_value = strsave(edesc->ed_copy);
	set_field_value((objid_t)edesc->ed_user, FN_ENV_VALUE, (fval_t)ee->ee_value);

	if (*ee->ee_value == '\0')
		errf("Warning: zero length enviroment entry");
	else if ((pos = strchr(ee->ee_value, '=')) == NULL || pos == ee->ee_value)
		errf("Warning: entry not in name=value format");
	else if (search_envtab(ee->ee_value, pos - ee->ee_value, ee) != NULL)
		errf("Warning: duplicate entry for %.*s",
						pos - ee->ee_value, ee->ee_value);

	return EDR_CONFIRM_CHANGE;
}

static enventry_t *
search_envtab(s, len, orig_ee)
const char *s;
int len;
enventry_t *orig_ee;
{
	enventry_t *ee;
	const char *pos;

	for (ee = Envtab->ee_next; ee != Envtab; ee = ee->ee_next) {
		if (ee != orig_ee &&
		    (pos = strchr(ee->ee_value, '=')) != NULL &&
		    pos - ee->ee_value == len &&
		    memcmp(s, ee->ee_value, len) == 0)
			return ee;
	}

	return NULL;
}

static void
env_edit(fdets)
draw_t fdets;
{
	edesc_t edescbuf;
	int res;

	clear_selection();
	make_edesc(&edescbuf, fdets.dr_wn, (char *)fdets.dr_fval, MAX_ENV_LEN,
						      fdets.dr_fg, fdets.dr_bg);
	edescbuf.ed_user = (int)fdets.dr_code;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)env_quitfunc;

	/*  BUG: must think of a better way to do this.
	 */
	if (fdets.dr_fval[0] == '\0') {
		edescbuf.ed_meaning = EDM_SETCURSOR;
		edescbuf.ed_newpos = 0;
		do_edit(&edescbuf, EDM_SETCURSOR);
	}

	res = suppress_ta_cursor_then_edit_field(&edescbuf, "env-var-value");
	if (res != EDR_CONFIRM_CHANGE)
		((enventry_t *)fdets.dr_code)->ee_hidden = TRUE;
}

int
env_dumpobj(arg, code, level)
char *arg;
objid_t code;
int level;
{
	return td_outf(level, "%s", ((enventry_t *)code)->ee_value);
}

/*  Process the return from a signal menu. Switching attributes on and off.
 */
void
do_env(obj, command)
objid_t obj;
int command;
{
	switch(command) {
	case MR_APPEND_ENV_ENTRY:
		add_env_entry((enventry_t *)obj);
		break;
	case MR_DELETE_ENV_ENTRY:
		remove_object(obj, OBJ_SELF);
		break;
	case MR_HIDE_ENV_ENTRY:
		((enventry_t *)obj)->ee_hidden = TRUE;
		remove_object(obj, OBJ_SELF);
		break;
	default:
		panic("bad cmd in de");
	}
}
