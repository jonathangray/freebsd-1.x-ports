#include <stdio.h>
#include "constants.h"
#include "globals.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)cvar.c	8.1	12/31/84)

/*
**  CVAR -- routines to manipulate the c variable trees
**
**	C variable trees are binary trees of cvar structs,
**	with the c_left < c_right with respect to c_id.
*/

/*
**  DEC_VAR -- declare a C var or field.
**
**	Parameters:
**		same as decl_cvar() & decl_field plus
**		the local and global tree variables.
**
**	Returns:
**		0 -- successful
**		other -- cvar node pointer that couldn't be entered
**			to tree
*/

cvar_t *
dec_var(char *name, int type, int indir_level, int block_level, cvar_t **local_tree, cvar_t **global_tree)
{
	register cvar_t		*cvarp;
	register			i;

	i = 0;
	cvarp = (cvar_t *)nalloc(sizeof(*cvarp));
	if (!cvarp) {
		yysemerr("unable to allocate space for a variable", name);
		return (0);
	}
	if (!(cvarp->c_id = salloc(name))) {
		yysemerr("no space for variable name", name);
		xfree(cvarp);
		return (0);
	}
	cvarp->c_type = type;
	cvarp->c_indir = indir_level;
	cvarp->c_left = cvarp->c_right = 0;
	i = c_enter(cvarp, block_level > 0 ? local_tree : global_tree);
	return (i ? 0 : cvarp);
}

/*
**   C_ENTER -- Enter a cvar node in a cvar tree
**
**	Parameters:
**		node -- the cvar node to insert
**		root -- a pointer to the root pointer
**
**	Returns:
**		1 -- if successful
**		0 -- otherwise (node of same name existed
**
**	Side Effects:
**		If a node of that name didn't exist one is inserted
**
**	Called By:
**		dec_var()
*/
int
c_enter(cvar_t *node, cvar_t **root)
{
	register char		*name;
	register cvar_t	*n, *r;

	r = *root;
	n = node;
	name = n->c_id;
	if (!r) {
		*root = n;
		return (1);
	}
	for (;;) {
		switch (scompare(name, 0, r->c_id, 0)) {
			
		  case -1 :
			if (!r->c_left) {
				r->c_left = n;
				return (1);
			}
			r = r->c_left;
			break;

		  case 0 :
			yysemerr("identifier re-declared", name);
			xfree(name);
			xfree(n);
			return (0);

		  case 1 :
			if (!r->c_right) {
				r->c_right = n;
				return (1);
			}
			r = r->c_right;
			break;
		}
	}
}

/*
**  DECL_CVAR -- Declare a C variable
**
**	Parameters:
**		name -- identifier string (makes its own copy for the tree)
**		type 
**		indir_level -- level of indirection of declaration
**			(- 1 if string)
**		block_level -- 0 - global, else local var
**
**	Returns:
**		none
**
**	Side Effects:
**		allocates a cvar node, and a copy of name, may put a node
**		in a cvar tree (if not previously declared).
**
**	Called By:
**		the c_variable productions of the parser [grammar.y]
*/
void
decl_cvar(char *name, int type, int indir_level, int block_level)
{
	register cvar_t		*bad_node;

	if ((bad_node = dec_var(name, type, indir_level, block_level,
				&C_locals, &C_globals)) != 0) {
		yysemerr("re-declared identifier", bad_node->c_id);
		xfree(bad_node->c_id);
		xfree(bad_node);
	}
}

/*
**  DECL_FIELD -- Declare a structures field
**
**	Same as decl_cvar() for fields within C records (structs).
**	NOTE : if a !0 is returned from dec_var() (i.e. the field
**	was already declared) the storage for that node is freed
**	but no error has been comitted, as fields may be re-declared.
*/
void
decl_field(char *name, int type, int indir_level, int block_level)
{
	register cvar_t		*bad_node;

	if ((bad_node = dec_var(name, type, indir_level, block_level,
				&F_locals, &F_globals)) != 0) {
		xfree(bad_node->c_id);
		xfree(bad_node);
	}
}

/*
**  GET_VAR -- get a cvar node from a local_tree, global_tree pair
**	searching first through the local then the global.
**
**	Parameters:
**		id -- c_id key
**		local_tree -- first tree
**		global_tree -- secomd tree to search
**
**	Returns:
**		0 -- if no node by that name
**		otherwise -- pointer to the node
*/


cvar_t *
get_var(char *id, cvar_t *local_tree, cvar_t *global_tree)
{
	register char		*name;
	register cvar_t	*tree, *node;
	char			flag;

	flag = 0;
	name = id;
	tree = local_tree;
	for (;;) {
		for (node = tree; node; ) {
			switch (scompare(name, 0, node->c_id, 0)) {
			  
			  case -1 :
				if (!node->c_left)
					break;
				else
					node = node->c_left;
				continue;

			  case 0 :
				return (node);

			  case 1 :
				if (!node->c_right)
					break;
				else
					node = node->c_right;
				continue;
			}
			break;
		}
		if (!flag) {
			flag += 1;
			tree = global_tree;
		}
		else
			return (0);
	}
}

/*
**  GETCVAR -- get the cvar node for a given identifier
**	Looks first in C_locals, then in C_globals.
**
**	Parameters:
**		id -- name of cvar to look for
**
**	Returns:
**		adress of cvar node if found
**		0 -- otherwise
**
**	Requires:
**		C_locals & C_globals -- to search them
*/

cvar_t *
getcvar(char *id)
{
	return (get_var(id, C_locals, C_globals));
}

/*
**  GETFIELD -- Same as getcvar() for structure fields
*/

cvar_t *
getfield(char *id)
{
	return (get_var(id, F_locals, F_globals));
}


/*
**  FREECVAR & F_CVAR -- Free up storage in a cvar tree
**
**	Freecvar calls f_cvar to free storage for a tree, then
**	0's out the root pointer passed it.
*/
void
f_cvar(cvar_t *root)
{
	if (root) {
		f_cvar(root->c_left);
		f_cvar(root->c_right);
		xfree(root->c_id);
		xfree(root);
	}
}

void
freecvar(cvar_t **rootp)
{
	f_cvar(*rootp);
	*rootp = 0;
}
