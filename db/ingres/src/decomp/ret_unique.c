#include <ingres.h>
#include <symbol.h>
#include <aux.h>
#include <tree.h>
#include "globs.h"
#include <errors.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)ret_unique.c	8.2	2/8/85)



/*
**	create a result relation for a ret_unique
*/
void
mk_unique(qtree_t *root)
{
	register int	i, domcnt;
	register qtree_t	*r;

	r = root;

	/* verify that target list is within range */
	domcnt = r->left->sym.type != TREE ? r->left->sym.value.sym_resdom.resno : 0;
	if (findwid(r) > MAX_TUP_SIZE || domcnt > MAX_DOMAINS)
		derror(RETUTOBIG);
	i = MAX_RANGES - 1;
	De.de_rangev[i].relnum = mak_t_rel(r, "u", -1);
	De.de_resultvar = i;

	/* don't count retrieve into portion as a user query */
	r->sym.value.sym_root.rootuser = 0;
}
/*
**	Retrieve all domains of the variable "var".
**	This routine is used for ret_unique to retrieve
**	the result relation. First duplicates are removed
**	then the original tree is converted to be a
**	retrieve of all domains of "var", and then
**	ovqp is called to retrieve the relation.
*/
void
pr_unique(qtree_t *root1, int var1)
{
	register qtree_t	*root, *r;
	register int	var;

	root = root1;
	var = var1;

	/* remove duplicates from the unopened relation */
	removedups(var);

	/* remove the qual from the tree */
	root->right = De.de_qle;

	/* make all resdoms refer to the result relation */
	for (r = root->left; r->sym.type != TREE; r = r->left)
		r->right = makavar(r, var, r->sym.value.sym_resdom.resno);

	/* count as a user query */
	root->sym.value.sym_root.rootuser = TRUE;

	/* run the retrieve */
	De.de_sourcevar = var;
	De.de_newq = De.de_newr = TRUE;
	if ( root->sym.value.sym_root.lvarc == 0 )
		root->sym.value.sym_root.lvarc = 1;
	call_ovqp(root, mdRETR, NORESULT);
}
