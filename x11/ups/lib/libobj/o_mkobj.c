/* o_mkobj.c - creation and destruction of objects and objcode hashing */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char obj_mkobj_sccsid[] = "@(#)o_mkobj.c	1.9 26/4/92 (UKC)";

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <stdio.h>

#include <local/wn.h>

#include <stdlib.h>

#include <local/ukcprog.h>
#include <mtrprog/alloc.h>

#include "obj.h"
#include "o_priv.h"

#include "o_mkobj.h"
#include "o_sel.h"
#include "o_lptrs.h"

/*  We use a chained hash table to convert from codes to objs (in
 *  code_to_obj.
 */
struct hashst {
	struct objst *ha_obj;
	struct hashst *ha_next;
};

/*  Size of the hash table
 */
#define HTABSIZE 497

/*  The hash table
 */
static struct hashst *Hashtab[HTABSIZE];

/*  Hash function - probably not very good. At the moment we simply
 *  divide the code by 8 and use the result mod the hash table size.
 *  We divide by 8 because the code is most likely a pointer to a
 *  structure, and will be at least 0 mod 4, and probably 0 mod 8.
 */
#define HASH(n) ((((int)n)>>3)%HTABSIZE)

static struct objst *new_obj PROTO((void));
static void free_obj PROTO((struct objst *obj));
static struct hashst *new_hash PROTO((void));
static void free_hash PROTO((struct hashst *hash));
static void unenter_obj PROTO((struct objst *obj));

#ifdef NO_VPRINTF
int _doprnt PROTO((const fmt, va_list arg, FILE *stream));
#endif

/*  Free list managers for objst, hashst and fvalst structures.
 *  See alloc.h
 */
ALLOC_NEW_FREE(static,struct objst,obj,ob_next)
ALLOC_NEW_FREE(static,struct hashst,hash,ha_next)
ALLOC_NEW_FREELIST(extern,struct fvalst,fval,fv_next)

/*  Return a pointer to a linked list of fvals, of length nfields
 */
struct fvalst *
new_fval_list(nfields)
int nfields;
{
	struct fvalst *head, *fptr;
	
	head = fptr = new_fval();
	while (--nfields > 0) {
		fptr->fv_val = NULL;
		fptr->fv_next = new_fval();
		fptr = fptr->fv_next;
	}
	fptr->fv_val = NULL;
	fptr->fv_next = NULL;
	return head;
}

/*  Enter a new object obj, with code code, in the hash table.
 *  Multiple objects with the same code is an panic, but this
 *  is not detected.
 */
void
enter_obj(obj, code)
struct objst *obj;
objid_t code;
{
	int i;
	struct hashst *new;
	
	i = HASH(code);
	new = new_hash();
	new->ha_obj = obj;
	new->ha_next = Hashtab[i];
	Hashtab[i] = new;
	set_likely(obj);
}

/*  Two objects that we think are likely to be needed soon (see set_likely()
 *  below.
 */
static struct objst *Likely1 = &Rootobj, *Likely2 = &Rootobj;

#ifdef STATS
/*  Variable for monitoring how successful the set_likely() heuristic
 *  has been.
 */
static int N_hits = 0, N_misses = 0, N_sets = 0;
#endif /* STATS */

/*  In an attempt to speed up hashing, we keep to objs that we think
 *  the user will refer to soon. This function sets them, but they
 *  are also set directly in some places.
 */
void
set_likely(obj)
struct objst *obj;
{
	Likely2 = Likely1;
	Likely1 = obj;
#ifdef STATS
	N_sets++;
#endif
}

/*  Look up code in the hash table, and return the object that points
 *  to it. Before trying the hash, we chack code against Likely{1,2}.
 *  Treat it as a fatal panic if the code is not found.
 */
struct objst *
code_to_obj(code)
register objid_t code;
{
	register struct hashst *p_hash;
	
	if (code == Likely1->ob_code) {
#ifdef STATS
		N_hits++;
#endif
		return Likely1;
	}
	if (code == Likely2->ob_code) {
#ifdef STATS
		N_hits++;
#endif
		return Likely2;
	}
	for (p_hash = Hashtab[HASH(code)]; p_hash != NULL &&
		    p_hash->ha_obj->ob_code != code; p_hash = p_hash->ha_next)
		;
	if (p_hash == NULL)
		panic("attempt to reference nonexistent object");
#ifdef STATS
	N_misses++;
#endif
	set_likely(p_hash->ha_obj);
	return p_hash->ha_obj;
}

/*  Remove an object from the hash table.
 *  Fatal error if the object is not found.
 */
static void
unenter_obj(obj)
struct objst *obj;
{
	int i;
	register struct hashst *p_hash, *prev_p_hash;
	
	i = HASH(obj->ob_code);
	p_hash = Hashtab[i];
	if (p_hash == NULL)
		panic("p_hash NULL in unenter_obj");
	prev_p_hash = NULL;
	while (p_hash->ha_obj != obj) {
		prev_p_hash = p_hash;
		p_hash = p_hash->ha_next;
		if (p_hash == NULL)
			panic("p_hash NULL in unenter_obj");
	}
	if (prev_p_hash == NULL)
		Hashtab[i] = p_hash->ha_next;
	else {
		prev_p_hash->ha_next = p_hash->ha_next;
		free_hash(p_hash);
	}
	if (obj == Likely1)
		Likely1 = &Rootobj;
	else if (obj == Likely2)
		Likely2 = &Rootobj;
}

/*  Link an object into the tree. Connected with the parent, next and
 *  prev pointers given. Note that if prev is NULL, this is the first
 *  object in the list, so we have to move the parents child pointer.
 *  We mark the parent as modified.
 */
void
link_object(obj, par, next, prev)
register struct objst *obj, *par, *next, *prev;
{
	obj->ob_parent = par;
	obj->ob_next = next;
	obj->ob_prev = prev;
	if (next != NULL)
		next->ob_prev = obj;
	if (prev != NULL)
		prev->ob_next = obj;
	else
		par->ob_child = obj;
	set_update_lptrs(par, FALSE);
}

/*  Remove object obj from the tree. Chop it out of the list,
 *  and if it is the first child of a parent, move the parent's
 *  child pointer on.
 *  Mark the parent as modified.
 */
void
unlink_object(obj)
register struct objst *obj;
{
	if (obj->ob_prev != NULL)
		obj->ob_prev->ob_next = obj->ob_next;
	else
		obj->ob_parent->ob_child = obj->ob_next;
	if (obj->ob_next != NULL)
		obj->ob_next->ob_prev = obj->ob_prev;
	set_update_lptrs(obj->ob_parent, FALSE);
}

/*  Make a new object with the given field values and type.
 *  Involves allocating space, entering the object in the hash table,
 *  setting up the fval list, and linking the object into the tree.
 */
void
make_obj(par, next, prev, code, type)
struct objst *par, *next, *prev;
objid_t code;
int type;
{
	register struct objst *obj;
	
	obj = new_obj();
	enter_obj(obj, code);
	obj->ob_child = NULL;
	obj->ob_code = code;
	obj->ob_type = type;
	if (Odesc[type].od_nfields > 1)
		obj->ob_fval = new_fval_list(Odesc[type].od_nfields);
	else
		obj->ob_fval = NULL;
	obj->ob_flags = Odesc[type].od_flags;
	link_object(obj, par, next, prev);
}

/*  Recursively remove a tree of objects. Removes the object,
 *  its children and/or its descendents depending on the arguments.
 *  Must deselect selected objects.
 */
void
rm_obj_tree(obj, kill_children, kill_parent, kill_descendents)
struct objst *obj;
int kill_children, kill_parent, kill_descendents;
{
	struct objst *child, *next;
	
	if (kill_children) {
		for (child = obj->ob_child; child != NULL; child = next) {
			next = child->ob_next;
			if (kill_descendents || child->ob_child == NULL)
				rm_obj_tree(child, TRUE, TRUE, TRUE);
		}
	}
	if (kill_parent) {
		if (obj->ob_flags & OB_IN_SEL_LIST)
			deselect(obj);
		unlink_object(obj);
		if (Odesc[obj->ob_type].od_free_obj != NULL)
			(*(Odesc[obj->ob_type].od_free_obj))(obj->ob_code);
		if ((obj->ob_flags & OB_ONE_FVAL) == 0)
			free_fval_list(obj->ob_fval);
		unenter_obj(obj);
		free_obj(obj);
	}	
}

#ifdef STATS

#define MAXLIST 50

/*  Print statistics on the size of the hash table, and the distribution
 *  of chain lengths. Also report how successful the set_likely() stuff
 *  has been.
 */
void
hash_stats()
{
	int table[MAXLIST+1];
	int i, list_len;
	struct hashst *p_hash;
	
	for (i = 0; i <= MAXLIST; i++)
		table[i] = 0;
	for (i = 0; i < HTABSIZE; i++) {
		list_len = 0;
		for(p_hash=Hashtab[i]; p_hash!=NULL; p_hash=p_hash->ha_next)
			list_len++;
		table[(list_len > MAXLIST) ? MAXLIST : list_len]++;
	}
	printf("length number\n");
	for (i = 0; i < MAXLIST; i++)
		if (table[i] > 0)
			printf("%6d %-6d\n", i, table[i]);
	if (table[MAXLIST] > 0)
		printf("  > %2d %-6d\n", MAXLIST, table[MAXLIST]);
	printf("hits:%d misses:%d sets:%d\n", N_hits, N_misses, N_sets);
	
}
#endif /* STATS */
