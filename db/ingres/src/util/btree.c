#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <stdio.h>

#include <ingres.h>
#include <catalog.h>
#include <aux.h>
#include <btree.h>
#include <batch.h>
#include <symbol.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)btree_util.c	8.3	5/30/88)

/*	Trace up to the root of the BTree, incrementing (or decrementing) all
**	key values.
**
**	Parameters :
**		tree - BTree filename (I)
**		block - starting node from which path is traced (I)
**		inc - either +1 or -1 (I)
*/
void
tracepath(long rootpage, register locator_t *block, register int inc)
{
	locator_t 	p, child;

	if (block->pageno != rootpage) {
		/* if at root, no need for adjustment */
		bmove(block, &child, sizeof(locator_t));

		/* trace up to root node */
		do {
			p.pageno = child.page.parent;
			parentkey(child.pageno, &p);
			p.page.node.intnode.key[p.offset] += inc;
			write_node(p.pageno, &p.page);
			bmove(&p, &child, sizeof(locator_t));
		} while (p.pageno != rootpage);
	}
}

/*	Determines which key/ptr pair is the parent of the given page
**
**	Parameters :
**		tree - BTree filename (I)
**		block - page number of child (I)
**		prt - information about the parent of the child (I, O)
**
*/
int
parentkey(long block, register locator_t *prt)
{
	register int	i;

	get_node(prt->pageno, &prt->page);
	/* find pointer in parent which references desired block */
	for (i = 0; prt->page.node.intnode.ptr[i] != block && i < prt->page.nelmts; ++i)
		continue;

	if (i == prt->page.nelmts)
		syserr("parentkey: child = %ld", block);
	prt->offset = i;
	return(0);
}

/*	Retrieve a node from the BTree
**
**	Parameters :
**		tree - BTree filename (I)
**		pagenum - page number of page to be retrieved (I)
**		buffer - buffer into which page is retrieved (O)
*/
void
get_node(long pagenum, register bt_node_t *buffer)
{
	int	Btree_fd;

	Btree_fd = getglobalint(BTREE_FD_NAME);
	if ( lseek(Btree_fd, (off_t) (pagenum * PGSIZE), 0) == -1 ) {
		syserr("GET_NODE: seek to %ld failed",pagenum);
	}
	if (read(Btree_fd, buffer, sizeof(*buffer)) != sizeof(*buffer)) {
		syserr("GET_NODE: read btree, page %ld", pagenum);
	}
}

/*	Write a node into the BTree file name 
**
**	Parameters :
**		tree - BTree filename (I)
**		pagenum - page number of page to be written (I)
**		buffer - buffer containing page to be written (I)
*/
void
write_node(long pagenum, register bt_node_t *buffer)
{
	int	Btree_fd;

	Btree_fd = getglobalint(BTREE_FD_NAME);
	(void) lseek(Btree_fd, (off_t) (pagenum * PGSIZE), 0);
	if (write(Btree_fd, buffer, PGSIZE) != PGSIZE) {
		syserr("WRITE_NODE: write btree, page %ld", pagenum);
	}
}

/*	Retrieves a new page from the BTree file.  If there is an empty page
**	in the file, that page is used.  Otherwise, a new page is added to
**	the end of the file.
**
**	Parameters :
**		tree - BTree filename (I)
**
**	Returns :
**		page number of new page
*/
long
new_page(char *tree)
{
	long			freepage, newpage;
	bt_node_t 	page, root, garbage;
	struct stat		fileinfo;

	get_node(RT, &root);
	freepage = root.parent;
	if (freepage == 0) {
		/* no free pages in file, add a new page to the end */
		/* determine page number of page at very end of file */
		stat(tree, &fileinfo);
		newpage = fileinfo.st_size / PGSIZE;
		write_node(newpage, &page);
	} else {
		/* use first free page, adjusting free page chain */
		newpage = freepage;
		get_node(newpage, &garbage);
		root.parent = garbage.parent;
		write_node(RT, &root);
	}
	return(newpage);
}

/*	Returns an empty file to the empty file list.  The head of the list
**	is indicated through the parent of the root and linked together
**	through the parents of the empty pages.
**
**	Parameters :
**		tree - BTree filename (I)
**		pagenum - page number of empty page (I)
*/
void
return_page(long pagenum)
{
	bt_node_t	root, freepage;	

	/* indicate that page is free by inserting its page number at the
	** head of the free page list */
	get_node(RT, &root);
	get_node(pagenum, &freepage);
	freepage.parent = root.parent;
	freepage.depth = 0;
	write_node(pagenum, &freepage);
	root.parent = pagenum;
	write_node(RT, &root);
}

/*
**	Determine the lid that corresponds to a given position in the B-Tree.
**
**	Parameters :
**		tree - BTree name (I)
**		tidloc - location in BTree (I)
**
**	Returns :
**		lid value
*/
void
get_lid(tid_t *tidloc, long *lid)
{
	static locator_t	tid;
	register int		i;
	long			l, child;

	do {
		/* determine where in BTree to search */
		pluck_page(tidloc, &tid.pageno);
	
		get_node(tid.pageno, (bt_node_t *) &tid.page);
		tid.offset = tid.page.node.leafnode.back_ptr[tidloc->line_id & I1MASK];

		if (tid.offset > tid.page.nelmts - 1)
			syserr("get_lid: bad tid location %ld, tid.offset=%d, tid.page.nelmts=%d", *tidloc, tid.offset,tid.page.nelmts);
	
		/* scan through leaf */
		for (l = 1, i = tid.offset; i > 0; ++l, --i)
			continue;
		/* trace up to root, adding keys */
		while (!tid.page.depth) {
			child = tid.pageno;
			tid.pageno = tid.page.parent;
			parentkey(child, &tid);
			for (i = tid.offset - 1; i >= 0; l += tid.page.node.intnode.key[i--])
				continue;
		}
		lid[tid.page.depth - 1] = l;
#ifdef xATR1
			if (tTf(24,0))
				printf("lid=%ld\n", lid[tid.page.depth - 1]);
#endif
		bmove(&tid.page.prttree, tidloc, LIDSIZE);
	} while (tid.page.depth > 1);
}

/*
**	Uses the secondary btree relation to find the btree tid corresponding 
**	to a given main relation tid
*/
void
search_btree(tid_t mtid, register tid_t *tidloc)
{
	char		key[2 * LIDSIZE], tup[2 * LIDSIZE];
	tid_t		tid;
	extern desc_t	Btreesec;

#ifdef xATR1
		if (tTf(24,0)) {
			printf("In Search_btree: searching for tid %ld\n", tid2int(mtid));
			printdesc(&Btreesec);
		}
#endif
	clearkeys(&Btreesec);
	ingres_setkey(&Btreesec, key, &mtid, 1);
	if (!getequal(&Btreesec, key, tup, &tid)) {
		bmove(tup + LIDSIZE, tidloc, LIDSIZE);
	} else {
		syserr("search_btree:can't find mtid %ld in btree", mtid);
	}
#ifdef xATR1
		if (tTf(24,0))
			printup(&Btreesec, tup);
#endif
}

/*
**	Linearly searches the leaves of the BTree for a desired tid
**
**	Parameters :
**		tree - BTree file name (I)
**		tid - desired tid value (I)
**		tidloc - location of the desired tid (O)
*/
void
lin_search(int level, long tid, tid_t *tidloc, long *lid, long tupcnt)
{
	locator_t	block;
	register int	i;
	long		page, t, next;
	int		found;
	long		nextpage, count;
	int		forward, first;

	page = RT;
	for (i = 0; i < level - 1; ++i) {
		t = get_tid(page, lid[i], &block);
		page = t;
	}

	found = 0;
	forward  = 0;
	first = 1;
	do {
		if (!forward) {
			get_node(page, &block.page);
			next = block.page.nexttree;
		}
		count = 0;

		/* start at leftmost leaf */
		do {
			get_node(page, &block.page);
			if (forward) {
				nextpage = block.page.nexttree;
			} else {
				nextpage = block.page.prevtree;
			}
			get_tid(page, 1, &block);
			for (;;) {
				/* search through leaf */
				for (i = 0; i < block.page.nelmts && block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[i]] != tid; ++i) {
					if (!first) {
						++count;
					}
				}
				if (i < block.page.nelmts && block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[i]] == tid) {
					found = 1;
					break;	/* tid found */
				}
				first = 0;
				if (!(block.pageno = block.page.node.leafnode.nextleaf) || count >= tupcnt) {
					break;	/* all leaves exhausted */
				} else {
					/* try leaf to the right */
					get_node(block.pageno, &block.page);
				}
			}
			if (found || count >= tupcnt) {
				break;
			}
		} while ((page = nextpage) != 0);
		nextpage = next;
		forward = !forward;
	} while (forward != 0 && !found);
	if (!found) {
		syserr("search_btree: tid not found %ld", tid);
	} else {
		stuff_page(tidloc, &block.pageno);
		tidloc->line_id = block.page.node.leafnode.tid_loc[i];
	}
}

/*
**	Determines the value corresponding to the very last lid in the
**	BTree.
**
**	Parameters :
**		tree - BTree file name (I)
**
**	Returns :
**		value of last lid
*/
long
last_lid(long rootpage)
{
	register int 		i;
	long			lid;
	bt_node_t	root;

	lid = 0;
	get_node(rootpage, &root);
	if (root.nodetype == 'L') {
		lid = root.nelmts;
	} else {
		for (i = 0; i < root.nelmts; ++i) {
			lid += root.node.intnode.key[i];
		}
	}
	++lid;
	return(lid);
}

/*
**	Changes the tid value stored in the leaf of a BTree node
**
**	Parameters :
**		tree - BTree file name (I)
**		tid - new tid value (I)
**		tidloc - location of tid to be replaced (I)
*/
void
replace_btree(tid_t tid, register tid_t *tidloc)
{
	long			pageno;
	bt_node_t	p;

	pluck_page(tidloc, &pageno);
	get_node(pageno, &p);
	bmove(&tid, &p.node.leafnode.tid_pos[(int)tidloc->line_id], sizeof(tid));
	write_node(pageno, &p);
}

void
btreename(char *relname, char *outname)
{
	char	*temp;

	temp = outname;
	bmove(relname, temp, MAX_NAME_SIZE + 2);
	smove("B\0", temp + MAX_NAME_SIZE + 2);
}

/*
**	Btreerange finds the smallest and largest tids corresponding to
**	the lids found between two given lids.
*/
void
btreerange(desc_t *d, long *low_lid, long *high_lid, tid_t *lotid, tid_t *hitid)
{
	register int 	i;
	long		l, h, temp;
	tid_t		tid;
	locator_t  	block;
	long		page, t, last, hbtid;
	int		done, first;
	long		start, next;
	long		lvar;

	/* find tid corresponding to high lid */
	page = RT;
	for (i = 0; i < d->d_r.r_dim; ++i) {
		if ((t = get_tid(page, high_lid[i], &block)) < 0)
			syserr("get_tid error in btreerange, lid = %ld\n", high_lid[i]);
		page = t;
	}
	hbtid = page;
	/* find starting point of scan */
	page = RT;
	for (i = 0; i < d->d_r.r_dim - 1; ++i) {
		last = last_lid(page) - 1;
		if (low_lid[i] > last)
			low_lid[i] = last;
		if ((t = get_tid(page, low_lid[i], &block)) < 0)
			syserr("get_tid error in btreerange, lid = %ld\n", low_lid[i]);
		page = t;
	}
	first = 1;
	last = last_lid(page) - 1;
	if (low_lid[d->d_r.r_dim - 1] > last)
		low_lid[d->d_r.r_dim - 1] = last;
	start = low_lid[d->d_r.r_dim - 1];
	do {
		get_node(page, &block.page);
		next = block.page.nexttree;
		if ((lvar = get_tid(page, start, &block)) < 0) {
			syserr("get_tid error in btreerange, lid = %ld\n",
					low_lid[d->d_r.r_dim - 1]);
		}
		(void) memcpy(&tid, &lvar, sizeof(tid));
		/* set high and low to intial value */
		if (first) {
			first = 0;
			pluck_page(&tid, &l);
			pluck_page(&tid, &h);
		}
		page = block.pageno;
		done = 0;
		while (done == 0) {
			for (i = 0; i < block.page.nelmts && done == 0; ++i) {
				(void) memcpy(&tid,
					&block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[i]],
					sizeof(tid));
				pluck_page(&tid, &temp);
				if (temp > h)
					h = temp;
				else if (temp < l)
					l = temp;
				if (memcmp(&tid, &hbtid, sizeof(tid)) == 0) {
					done = 1;
				}
			}
			page = block.page.node.leafnode.nextleaf;
			if (page == 0) {
				done = 1;
			} else {
				get_node(page, &block.page);
			}
		}
		start = 1;
	} while ((page = next) != 0 && !done);
	stuff_page(lotid, &l);
	stuff_page(hitid, &h);
	lotid->line_id = hitid->line_id = -1;
}

/*	CREATE_BTREE -- creates a new B-Tree
**
**	Creates an empty B-Tree whose root is an empty leaf.  The B-Tree
** 	filename is the relation name with "TREE" concatenated to the end
**	of the name.
**
**	Parameters:
**		relname - relation name (I)
*/
void
create_btree(char *relname)
{
	bt_node_t	root;
	register int		i;

	root.depth = 1;
	root.prevtree = root.nexttree = 0l;
	bmove(&root.prevtree, &root.prttree, LIDSIZE);
	/* the root is initially an empty leaf node */
	root.nodetype = 'L';
	root.nelmts = 0;
	root.parent = 0;	/* '0' indicates no other empty pages in file */

	root.node.leafnode.prevleaf = 0;
	root.node.leafnode.nextleaf = 0;

	for (i = 0; i< MAXLEAVES; ++i)
		root.node.leafnode.tid_loc[i] = root.node.leafnode.back_ptr[i] = i;

	close(open(relname, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE));
	setglobalint(BTREE_FD_NAME, open(relname, O_RDWR));
	if (getglobalint(BTREE_FD_NAME) < 0) {
		syserr("create_btree: can't open %s", relname);
	}
	write_node(RT, &root);

#ifdef xATR1
	if (tTf(24, 0))
		printf("creating btree %s", relname);
#endif

}

/*	Divides key/ptr pairs between the left and right nodes, so both will
**	have the proper number of elements.
**
**	Parameters :
**		tree - BTree filename (I)
**		left - left node (I, O)
**		right - "left's" right sibling (I, O)
**		lkey - key value of the parent of left node (O)
**		rkey - key value of the parent of right node (O)
*/
void
distribute(register locator_t *left, register locator_t *right, long *lkey, long *rkey)
{
	register int		i, move;
	bt_node_t	temp;

	if (right->page.nelmts > left->page.nelmts) {
		/* move elements from the right node to the left node */
		move = MAXPTRS / 2 - left->page.nelmts;

		for (i = 1; i <= move; ++i) {
			/* move first part of right node into the end of the left node */
			left->page.node.intnode.ptr[i + left->page.nelmts] =
				right->page.node.intnode.ptr[i - 1];
			left->page.node.intnode.key[i + left->page.nelmts] =
				right->page.node.intnode.key[i - 1];
			/* adjust parents of children whose parents have been
			** moved */
			get_node(left->page.node.intnode.ptr[i + left->page.nelmts], &temp);
			temp.parent = left->pageno;
			write_node(left->page.node.intnode.ptr[i + left->page.nelmts], &temp);
		}
		left->page.nelmts += move;

		for (i = move; i < right->page.nelmts; ++i) {
			/* flush right node values to the left */
			right->page.node.intnode.ptr[i - move] =
				right->page.node.intnode.ptr[i];
			right->page.node.intnode.key[i - move] =
				right->page.node.intnode.key[i];
		}
		right->page.nelmts -= move;
	} else {
		/*  move left node values to the right node */
		move = MAXPTRS / 2 - right->page.nelmts + 1;

		/* move right node values to the right to make room for left
		** node values */
		for (i = right->page.nelmts - 1; i >= 0; --i) {
			right->page.node.intnode.ptr[i + move] =
				right->page.node.intnode.ptr[i];
			right->page.node.intnode.key[i + move] =
				right->page.node.intnode.key[i];
		}

		/* move latter part of left node into the now free space at the
		** beginning of the right node */
		for (i = 0; i < move; ++i) {
			right->page.node.intnode.ptr[i] = 
				left->page.node.intnode.ptr[left->page.nelmts - move + i];
			right->page.node.intnode.key[i] =
				left->page.node.intnode.key[left->page.nelmts - move + i];
			/* adjust parents of children whose parents have been
			** moved */
			get_node(right->page.node.intnode.ptr[i], &temp);
			temp.parent = right->pageno;
			write_node(right->page.node.intnode.ptr[i], &temp);
		}
		left->page.nelmts -= move;
		right->page.nelmts += move;
	}

	/* calculate key values for parents of the left and right nodes */
	*lkey = 0;
	for (i = 0; i < left->page.nelmts; ++i)
		*lkey += left->page.node.intnode.key[i];
	*rkey = 0;
	for (i = 0; i < right->page.nelmts; ++i)
		*rkey += right->page.node.intnode.key[i];
	write_node(left->pageno, &left->page);
	write_node(right->pageno, &right->page);
}

/*	Combines left and right nodes together to form one node.
**
**	Parameters :
**		tree - BTree filename (I)
**		left - left node (I, O)
**		right - "left's" right sibling (I)
*/
void
combine(register locator_t *left, register locator_t *right)
{
	register int		i;
	bt_node_t	temp;

	/* move all ptr/key values in the right node to the end of the left node */
	for (i = 0; i < right->page.nelmts; ++i) {
		left->page.node.intnode.ptr[left->page.nelmts + i] =
			right->page.node.intnode.ptr[i];
		left->page.node.intnode.key[left->page.nelmts + i] =
			right->page.node.intnode.key[i];
		/* adjust parents of children whose parents have been moved */
		get_node(left->page.node.intnode.ptr[left->page.nelmts + i], &temp);
		temp.parent = left->pageno;
		write_node(left->page.node.intnode.ptr[left->page.nelmts + i], &temp);
	}
	left->page.nelmts += right->page.nelmts;
	write_node(left->pageno, &left->page);
}

/*	Returns an empty page to the empty file space list.  Removes key/ptr
** 	pair corresponding to empty node from tree.  Combines and distributes
**	pairs if a node has less than the minimum number of values.  Adjusts
**	all values up the tree.
**
**	Parameters :
**		tree - BTree filename (I)
**		empty - empty node (I)
*/
void
delete_key(long rootpage, register locator_t *empty)
{
	locator_t		prt, gprt, sibling;
	register int		i;
	bt_node_t	new, temp;
	long			pkey, skey;
	extern char		*Fileset;
	char			replbatch[MAX_NAME_SIZE + 4];
	FILE			*fp;
	long			count, page;
	tid_t			oldtid, newtid;

	/* find parent corresponding to empty node, and remove ptr/key pair
	** from parent */
	prt.pageno = empty->page.parent;
	parentkey(empty->pageno, &prt);
	if (prt.offset != prt.page.nelmts - 1) {
		for (i = prt.offset; i < prt.page.nelmts; ++i) {
			prt.page.node.intnode.ptr[i] = 
				prt.page.node.intnode.ptr[i + 1];
			prt.page.node.intnode.key[i] =
				prt.page.node.intnode.key[i + 1];
		}
	}
	--prt.page.nelmts;

	if (empty->page.nodetype == 'L') {
		/* adjust previous and next fields of leaves */
		if (empty->page.node.leafnode.nextleaf != 0) {
			get_node(empty->page.node.leafnode.nextleaf, &temp);
			temp.node.leafnode.prevleaf = empty->page.node.leafnode.prevleaf;
			write_node(empty->page.node.leafnode.nextleaf, &temp);
		}
		if (empty->page.node.leafnode.prevleaf != 0) {
			get_node(empty->page.node.leafnode.prevleaf, &temp);
			temp.node.leafnode.nextleaf = empty->page.node.leafnode.nextleaf;
			write_node(empty->page.node.leafnode.prevleaf, &temp);
		}
	}

	/* return empty node to empty file space */
	return_page(empty->pageno);

	if (prt.page.nelmts >= (int) ((float) MAXPTRS / 2 + 0.5)) {
		write_node(prt.pageno, &prt.page);
		/* node still has proper number of elements, decrement key  
		** values up the tree */
		tracepath(rootpage, &prt, -1);
	} else if (prt.pageno == rootpage && prt.page.nelmts < 2) {
		/* root has only one child, a leaf; root becomes leaf node */
		/* move leaf values into root; root's position always remains
		** the same */
		get_node(prt.page.node.intnode.ptr[0], &new);
		new.parent = prt.page.parent;
		write_node(rootpage, &new);
		return_page(prt.page.node.intnode.ptr[0]);
		if (new.nodetype  == 'I') {
			for (i = 0; i < new.nelmts; ++i) {
				get_node(new.node.intnode.ptr[i], &temp);
				temp.parent = rootpage;
				write_node(new.node.intnode.ptr[i], &temp);
			}
		} else {
			/* btree tid is being changed, must be reflected in
			** secondary btree relation
			*/
			concat(REPL_IN, Fileset, replbatch);
			if ((fp = fopen(replbatch, "w")) == NULL)
				syserr("can't open replbatch in delete_btree");
			count = 0;
			page = 0l;
			stuff_page(&newtid, &page);
			stuff_page(&oldtid, &prt.page.node.intnode.ptr[0]);
			for (i = 0; i < new.nelmts; ++i) {
				oldtid.line_id = newtid.line_id = new.node.leafnode.tid_loc[i];
				if (fwrite(&oldtid, 1, LIDSIZE, fp) != LIDSIZE)
					syserr("write error in delete_btree");
				if (fwrite(&newtid, 1, LIDSIZE, fp) != LIDSIZE)
					syserr("write error in delete_btree");
				++count;
			}
			fclose(fp);
			repl_btree(replbatch, count);
			unlink(replbatch);
			unlink(ztack(REPL_OUT, Fileset));
		}
	} else if (prt.pageno != rootpage) {
		/* find the grandparent of the empty node */
		gprt.pageno = prt.page.parent;
		parentkey(prt.pageno, &gprt);
		if (gprt.page.nelmts - 1 != gprt.offset) {
			/* determine the right sibling of the parent node */
			sibling.pageno = gprt.page.node.intnode.ptr[gprt.offset + 1];
			get_node(sibling.pageno, &sibling.page);

			if (sibling.page.nelmts > MAXPTRS / 2 + 1) {
			/* distribute key/ptr pairs of parent and right sibling
			** between the two nodes (since there are sufficient
			** pairs to guarantee that both will have at the least
			** the minimum number of required children) */
				distribute(&prt, &sibling, &pkey, &skey);
				/* adjust key values in grandparent */
				gprt.page.node.intnode.key[gprt.offset] = pkey;
				gprt.page.node.intnode.key[gprt.offset + 1] = skey;
				write_node(gprt.pageno, &gprt.page);
				tracepath(rootpage, &gprt, -1);
				return;
			}
		}
		if (gprt.offset != 0) {
			/* determine the left sibling of the parent node */
			sibling.pageno = gprt.page.node.intnode.ptr[gprt.offset - 1];
			get_node(sibling.pageno, &sibling.page);

			if (sibling.page.nelmts > MAXPTRS / 2 + 1) {
			/* distribute key/ptr pairs of parent and left sibling
			** between the two nodes */
				distribute(&sibling, &prt, &skey, &pkey);
				gprt.page.node.intnode.key[gprt.offset - 1] = skey;
				gprt.page.node.intnode.key[gprt.offset] = pkey;
				write_node(gprt.pageno, &gprt.page);
				tracepath(rootpage, &gprt, -1);
				return;
			}
		}

		if (gprt.page.nelmts - 1 != gprt.offset) {
		/* combine parent node with its right sibling */
			sibling.pageno = gprt.page.node.intnode.ptr[gprt.offset + 1];
			get_node(sibling.pageno, &sibling.page);
			combine(&prt, &sibling);
		} else {
		/* combine parent node with its left sibling */
			sibling.pageno = gprt.page.node.intnode.ptr[gprt.offset - 1];
			get_node(sibling.pageno, &sibling.page);
			combine(&sibling, &prt);
			/* grandparent should point to newly combined block,
			** the left sibling */
			--gprt.offset;
		}

		get_node(gprt.page.node.intnode.ptr[gprt.offset], &new);
		if (gprt.pageno == rootpage && gprt.page.nelmts == 2) {
		/* root has only one child, reduce B-Tree level */
			/* only child becomes root */
			new.parent = gprt.page.parent;
			write_node(rootpage, &new);

			/* make sure "new's" children's parent is the root */
			for (i = 0; i < new.nelmts; ++i) {
				get_node(new.node.intnode.ptr[i], &temp);
				temp.parent = rootpage;
				write_node(new.node.intnode.ptr[i], &temp);
			}
			/* both of root's children empty */
			return_page(gprt.page.node.intnode.ptr[gprt.offset + 1]);
			return_page(gprt.page.node.intnode.ptr[gprt.offset]);
		} else {
			/* adjust key value in grandparent node */
			pkey = 0;
			for (i = 0; i < new.nelmts; ++i)
				pkey += new.node.intnode.key[i];
			gprt.page.node.intnode.key[gprt.offset] = pkey;
			write_node(gprt.pageno, &gprt.page);

			/* remove ptr/key pair corresponding to empty node */
			gprt.pageno = gprt.page.node.intnode.ptr[gprt.offset + 1];
			get_node(gprt.pageno, &gprt.page);
			delete_key(rootpage, &gprt);
		}
	
	}
}

/*	DELETE_BTREE -- BTree deletion routine
**
**	Deletes a tid from a leaf node and adjusts all values up the tree
**
**	Parameters :
**		tree - BTree filename (I)
**		lid - lid to be deleted (I)
**
**	Returns :
**		> 0 	success
**		< 0 	bad lid
*/
int
delete_btree(long *lid, register int level)
{
	register int		i, j;
	locator_t		tid[MAXLID];
	register int		save;
	int			num[MAXLID];
	int			del[MAXLID];
	long			page[MAXLID + 1], t;
	bt_node_t	temp;

#ifdef xATR1
	if(tTf(24,0))
		printf("deleting lid %ld from tree ", *lid);
#endif

	page[0] = RT;
	for (i = 0; i < level; ++i) {
		if ((t = get_tid(page[i], lid[i], &tid[i])) < 0)
			return(-1);
		del[i] = 0;
		num[i] = tid[i].page.nelmts; 
		page[i+1] = t;
	}

	del[level-1] = 1;
	for (i = level - 2; i >= 0; --i) {
		if (num[i + 1] == 1 && del[i + 1] == 1)
			del[i] = 1;
	}

	for (i = 0; i < level; ++i) {
		if (del[i]) {
			++Repl_cnt[i];
			for (j = i - 1; j >= 0; --j)
				Prev_lid[j] = lid[j];
			/* remove tid from leaf */
			if (tid[i].offset != tid[i].page.nelmts - 1) {
				save = tid[i].page.node.leafnode.tid_loc[tid[i].offset];
				for (j = tid[i].offset; j < tid[i].page.nelmts; ++j) {
					tid[i].page.node.leafnode.tid_loc[j] =
						tid[i].page.node.leafnode.tid_loc[j + 1];
					tid[i].page.node.leafnode.back_ptr[tid[i].page.node.leafnode.tid_loc[j]] = j;
				}
				tid[i].page.node.leafnode.tid_loc[tid[i].page.nelmts - 1] = save;
				tid[i].page.node.leafnode.back_ptr[save] = tid[i].page.nelmts - 1;
			}
			--tid[i].page.nelmts;

			if (tid[i].page.nelmts != 0) {
				write_node(tid[i].pageno, &tid[i].page);
				/* leaf is now empty as a result of deletion, decrement keys
				** up tree */
				tracepath(page[i], &tid[i], -1);
			} else if (tid[i].pageno != page[i]) {
				/* key/ptr pair corresponding to empty leaf must be removed */
				delete_key(page[i], &tid[i]);
			} else if (lid[i] == 1 && page[i] != RT) {
				if (tid[i].page.prevtree) {
					get_node(tid[i].page.prevtree, &temp);
					temp.nexttree = tid[i].page.nexttree;
					write_node(tid[i].page.prevtree, &temp);
				}
				if (tid[i].page.nexttree) {
					get_node(tid[i].page.nexttree, &temp);
					temp.prevtree = tid[i].page.prevtree;
					write_node(tid[i].page.nexttree, &temp);
				}
				return_page(page[i]);
			} else
				write_node(page[i], &tid[i].page);
		}
	}
	return(0);
}

/*	Form a new root with two children since the old root was split.
**
**	Parameters :
**		tree - BTree filename (I)
**		oldroot - split root (I, O)
**		rpageno - page number of new root's right child (I)
**		rkey - key of new root's right child (I)
*/
void
rootsplit(char *tree, long rootpage, register locator_t *oldroot, long rpageno, long lkey, long rkey)
{
	bt_node_t	newroot, temp;
	long			i;

	/* get a new page for the former root */
	oldroot->pageno = new_page(tree);

	newroot.depth = oldroot->page.depth;
	newroot.prevtree = oldroot->page.prevtree;
	newroot.nexttree = oldroot->page.nexttree;
	bmove(&oldroot->page.prttree, &newroot.prttree, LIDSIZE);
	newroot.nodetype = 'I';
	newroot.nelmts = 2;
	newroot.parent = oldroot->page.parent;
	oldroot->page.parent = rootpage;
	oldroot->page.depth = 0;
	newroot.node.intnode.key[0] = lkey;
	newroot.node.intnode.ptr[0] = oldroot->pageno;
	newroot.node.intnode.key[1] = rkey;
	newroot.node.intnode.ptr[1] = rpageno;

	write_node(rootpage, &newroot);

	if (oldroot->page.nodetype == 'I')
		/* make sure the children of the oldroot have the correct parent */
		for (i = 0; i < oldroot->page.nelmts; ++i) {
			get_node(oldroot->page.node.intnode.ptr[i], &temp);
			temp.parent = oldroot->pageno;
			write_node(oldroot->page.node.intnode.ptr[i], &temp);
		}
}

/*	Insert a key/ptr pair into a node, splitting nodes if necessary and
**	adjusting values up the tree.
**
**	Parameters :
**		tree - BTree filename (I)
**		p - page number of newly formed node (I)
**		k - key value of newly formed node (I)
**		pkey - information about the node whose ptr/key pair is to
**		       be inserted (I, O)
*/
void
insert_key(char *tree, long rootpage, long p, long k, register locator_t *pkey)
{
	register int		i, j, start;
	bt_node_t	newblock, temp;
	long			newpage, oldkey, newkey;
	locator_t		prt;
	short			rblock;

	if (pkey->page.nelmts != MAXPTRS) {
	/* insert pointer/key pair into proper position in node by moving
	** other pairs down node */
		for (i = pkey->page.nelmts - 1; i >= pkey->offset + 1; --i) {
			pkey->page.node.intnode.ptr[i + 1] =
				pkey->page.node.intnode.ptr[i];
			pkey->page.node.intnode.key[i + 1] =
				pkey->page.node.intnode.key[i];
		}
		++pkey->page.nelmts;
		pkey->page.node.intnode.ptr[pkey->offset + 1] = p;
		pkey->page.node.intnode.key[pkey->offset + 1] = k;

		write_node(pkey->pageno, &pkey->page);

		/* trace up B-Tree incrementing keys */ 
		tracepath(rootpage, pkey, 1);
	} else {
	/* node is full, must be split */
		/* determine where split will be made */
		start = MAXPTRS / 2;
		rblock = 0;

		if (pkey->offset + 1 > start) {
		/* ptr/key pair will be inserted in new node */
			++start;
			rblock = 1;
		}

		/* readjust old node values and create new node values */
		pkey->page.nelmts = start;
		newpage = new_page(tree);
		newblock.nodetype = 'I';
		newblock.nelmts = MAXPTRS + 1 - start;
		newblock.parent = pkey->page.parent;
		newblock.depth = 0;

		/* copy right side of old node into new node */

		for (i = 0, j = start; j < MAXPTRS || rblock == 1; ++i) {
			if (j == pkey->offset + 1 && rblock == 1) {
			/* node position corresponds to that of new ptr/key pair */
				newblock.node.intnode.ptr[i] = p;
				newblock.node.intnode.key[i] = k;
				/* make sure children of newblock have proper
				** parent */
				get_node(p, &temp);
				temp.parent = newpage;
				write_node(p, &temp);

				rblock = -1;
			} else {
				newblock.node.intnode.ptr[i] =
					pkey->page.node.intnode.ptr[j];
				newblock.node.intnode.key[i] =
					pkey->page.node.intnode.key[j];
				/* make sure children of newblock have proper
				** parent */
				get_node(newblock.node.intnode.ptr[i], &temp);
				temp.parent = newpage;
				write_node(newblock.node.intnode.ptr[i], &temp);
				++j;
			}
		}

		if (!rblock) {
		/* insert new ptr/key pair into proper position in old node */
			for (i = pkey->page.nelmts - 1; i >= pkey->offset + 1; --i) {
				pkey->page.node.intnode.ptr[i + 1] =
					pkey->page.node.intnode.ptr[i];
				pkey->page.node.intnode.key[i + 1] =
					pkey->page.node.intnode.key[i];
			}
			pkey->page.node.intnode.ptr[pkey->offset + 1] = p;
			pkey->page.node.intnode.key[pkey->offset + 1] = k;
			++pkey->page.nelmts;
			--newblock.nelmts;
		}

		/* calculate the key values of the old and new nodes */
		oldkey = 0;
		for (i = 0; i < pkey->page.nelmts; ++i) {
			oldkey += pkey->page.node.intnode.key[i];
		}
		newkey = 0;
		for (i = 0; i < newblock.nelmts; ++i) {
			newkey += newblock.node.intnode.key[i];
		}

		if (pkey->pageno == rootpage) {
			/* split node was root, add a new level to B-Tree */
			rootsplit(tree, rootpage, pkey, newpage, oldkey, newkey);
			newblock.parent = rootpage;
			write_node(pkey->pageno, &pkey->page);
			write_node(newpage, &newblock);
		} else {
		/* recursively add ptr/key pair corresponding to new node into
		** the parent of the old node */
			write_node(pkey->pageno, &pkey->page);
			write_node(newpage, &newblock);
			prt.pageno = pkey->page.parent;
			parentkey(pkey->pageno, &prt);
			prt.page.node.intnode.key[prt.offset] = oldkey;
			insert_key(tree, rootpage, newpage, newkey, &prt);
		}
	}
}

/*	INSERT_BTREE -- BTree insertion routine
**
**	Insert a tid into the BTree at the position corresponding to its lid.
**	Split the leaf if necessary and adjust all values up the tree.
**
**	Parameters :
**		tree - BTree filename (I)
**		lid - given lid (I)
**		tid - tid to be inserted into BTree leaf (I)
**		tidpos - location where tid was inserted (O)
**
**	Returns :
**		-1  	lid position does not exist
**		0	successful insertion
*/
void
insert_btree(char *tree, long rootpage, long lid, long *tid, register tid_t *tidpos, char create)
{
	register int		i, j, start;
	locator_t		block, p;
	bt_node_t	newblock, temp, newroot;
	short			rblock, tline;
	long			newpage, tpage;
	long			get_tid();
	int			save;
	char			replbatch[MAX_NAME_SIZE + 4];
	FILE 			*fp;
	tid_t			oldtid, newtid;
	long			count, page, childpage;
	extern char		*Fileset;

#ifdef xATR1
	if (tTf(24,0))
		printf("inserting lid %ld into btree at rootpage %d", lid, rootpage);
#endif
	tline = 0;
	fp = (FILE *) NULL;
	count = 0;

	/* find location of desired lid in B-Tree */
	if (get_tid(rootpage, lid, &block) < -1)
		return;

	if ((newroot.depth = create) != 0) {
		if ((save = block.offset) != 0)
			newroot.prevtree = block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[save -1]];
		else {
			if (!block.page.prevtree)
				newroot.prevtree = 0;
			else {
				get_node(block.page.prevtree, &temp);
				newroot.prevtree = temp.node.leafnode.tid_pos[temp.node.leafnode.tid_loc[temp.nelmts - 1]];
			}
		}
		if  (save <= block.page.nelmts - 1 && block.page.nelmts)
			newroot.nexttree = block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[save]];
		else {
			if (!block.page.nexttree)
				newroot.nexttree = 0;
			else {
				get_node(block.page.nexttree, &temp);
				newroot.nexttree = temp.node.leafnode.tid_pos[temp.node.leafnode.tid_loc[0]];
			}
		}
		*tid = new_page(tree);
		if (block.pageno == RT)
			get_node(RT, &block.page);
		if (newroot.prevtree) {
			get_node(newroot.prevtree, &temp);
			temp.nexttree = *tid;
			write_node(newroot.prevtree, &temp);
		}
		if (newroot.nexttree) {
			get_node(newroot.nexttree, &temp);
			temp.prevtree = *tid;
			write_node(newroot.nexttree, &temp);
		}
		stuff_page(&newroot.prttree, &block.pageno);
		newroot.nodetype = 'L';
		newroot.nelmts = 0;
		newroot.parent = 0;
		newroot.node.leafnode.prevleaf = 0;
		newroot.node.leafnode.nextleaf = 0;
		for (i = 0; i < MAXLEAVES; ++i)
			newroot.node.leafnode.tid_loc[i] = newroot.node.leafnode.back_ptr[i] = i;
	}

	if (block.page.nelmts != MAXLEAVES) {
	/* insert tid into its proper position in leaf */
		save = block.page.node.leafnode.tid_loc[block.page.nelmts];
		/* move other tids down to make room for new tid */
		for (i = block.page.nelmts - 1; i >= block.offset; --i) {
			block.page.node.leafnode.tid_loc[i + 1] =
				block.page.node.leafnode.tid_loc[i];
			block.page.node.leafnode.back_ptr[block.page.node.leafnode.tid_loc[i]] = i + 1;
		}
		block.page.node.leafnode.tid_loc[block.offset] = save;
		block.page.node.leafnode.tid_pos[save] = *tid;
		block.page.node.leafnode.back_ptr[save] = block.offset;
		++block.page.nelmts;
		write_node(block.pageno, &block.page);
		if (create)
			newroot.prttree.line_id = save;

		/* trace up B-Tree, incrementing key values */
		tracepath(rootpage, &block, 1);

		tpage = block.pageno;
		tline = save;
	} else {
	/* leaf is full, must be split */
		/* determine where to split leaf */
		start = MAXLEAVES / 2;
		rblock = 0;

		if (block.offset > start) {		
		/* new tid will be inserted in the new leaf */
			++start;
			rblock = 1;
		}

		/* readjust all values in the old leaf and assign them for
		** the new leaf */

		block.page.nelmts = start;	/* assume new tid will be
						** inserted into new leaf */
		newpage = new_page(tree);
		newblock.nodetype = 'L';
		for (i = 0; i < MAXLEAVES; ++i)
			newblock.node.leafnode.tid_loc[i] = newblock.node.leafnode.back_ptr[i] = i;
		newblock.nelmts = MAXLEAVES + 1 - start;
		newblock.parent = block.page.parent;
		newblock.depth = 0;

		if ((newblock.node.leafnode.nextleaf = block.page.node.leafnode.nextleaf) != 0) {
			get_node(newblock.node.leafnode.nextleaf, &temp);
			temp.node.leafnode.prevleaf = newpage;
			write_node(newblock.node.leafnode.nextleaf, &temp);
		}
		block.page.node.leafnode.nextleaf = newpage;
		newblock.node.leafnode.prevleaf = block.pageno;

		/* create file for storing tids that must be replaced in btree
		** secondary relation
		*/
		if (!bequal("_SYS", tree, 4) && !create) {
			concat(REPL_IN, Fileset, replbatch);
			if ((fp = fopen(replbatch, "w")) == NULL)
				syserr("can't open batch file in insert_btree");
			count = 0;
			stuff_page(&oldtid, &block.pageno);
			stuff_page(&newtid, &newpage);
		}

		/* copy the right half of the old leaf onto the new leaf */
		for (i = 0, j = start; j < MAXLEAVES || rblock == 1; ++i) {
			if (j == block.offset && rblock == 1) {
			/* current position in new leaf corresponds to position
			** of new tid */
				newblock.node.leafnode.tid_pos[newblock.node.leafnode.tid_loc[i]] = *tid;
				tline = newblock.node.leafnode.tid_loc[i];
				/* indicate that tid has been inserted */
				rblock = -1;
			} else {
				childpage = newblock.node.leafnode.tid_pos[newblock.node.leafnode.tid_loc[i]] =
					block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[j]];
				if (create) {
					get_node(childpage, &temp);
					stuff_page(&temp.prttree, &newpage);
					temp.prttree.line_id = newblock.node.leafnode.tid_loc[i];
					write_node(childpage, &temp);
				}
				if (!bequal("_SYS", tree, 4) && !create) {
					oldtid.line_id = block.page.node.leafnode.tid_loc[j];
					newtid.line_id = newblock.node.leafnode.tid_loc[i];
					if (fwrite(&oldtid, 1, LIDSIZE, fp) != LIDSIZE)
						syserr("write error in insert_btree");
					if (fwrite(&newtid, 1, LIDSIZE, fp) != LIDSIZE)
						syserr("write error in insert_btree");
					++count;
				}
				++j;
			}
		}
		if (!bequal("_SYS", tree, 4) && !create) {
			fclose(fp);
			repl_btree(replbatch, count);
		}

		if (!rblock) {
		/* new tid belongs in old leaf, insert it into its proper 
		** position */
			save = block.page.node.leafnode.tid_loc[block.page.nelmts];
			for (i = block.page.nelmts - 1; i >= block.offset; --i) {
				block.page.node.leafnode.tid_loc[i + 1] =
					block.page.node.leafnode.tid_loc[i];
				block.page.node.leafnode.back_ptr[block.page.node.leafnode.tid_loc[i]] = i + 1;
			}
			block.page.node.leafnode.tid_loc[block.offset] = save;
			block.page.node.leafnode.tid_pos[save] = *tid;
			block.page.node.leafnode.back_ptr[save] = block.offset;
			tline = save;
			/* readjust element counts to reflect that tid has been
			** placed in the old leaf */
			++block.page.nelmts;
			--newblock.nelmts;
		}

		if (block.pageno == rootpage) {
			/* split leaf was the root, a new level to the B-Tree
			** must be added */
			rootsplit(tree, rootpage, &block, newpage, block.page.nelmts, newblock.nelmts);
			newblock.parent = rootpage;
			write_node(block.pageno, &block.page);
			newblock.node.leafnode.prevleaf = block.pageno;
			write_node(newpage, &newblock);

			if (create) {
				for (i = 0; i < block.page.nelmts; ++ i) {
					get_node(block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[i]], &temp);
					stuff_page(&temp.prttree, &block.pageno);
					write_node(block.page.node.leafnode.tid_pos[block.page.node.leafnode.tid_loc[i]], &temp);
				}
			}
			/* btree page has changed */
			if (!bequal("_SYS", tree, 4) && !create) {
				concat(REPL_IN, Fileset, replbatch);
				if ((fp = fopen(replbatch, "w")) == NULL)
					syserr("can't open batch file in insert_btree");
				count = 0;
				page = 0l;
				stuff_page(&oldtid, &page);
				stuff_page(&newtid, &block.pageno);
				for (i = 0; i < block.page.nelmts; ++i) {
					if (rblock || block.page.node.leafnode.tid_loc[i] != tline) {
						oldtid.line_id = newtid.line_id = block.page.node.leafnode.tid_loc[i];
						if (fwrite(&oldtid, 1, LIDSIZE, fp) != LIDSIZE)
							syserr("write error in insert_btree");
						if (fwrite(&newtid, 1, LIDSIZE, fp) != LIDSIZE)
							syserr("write error in insert_btree");
						++count;
					}
				}
				fclose(fp);
				repl_btree(replbatch, count);
			}
		} else {
		/* insert the pointer and key associated with new leaf into the
		** parent of the old leaf */
			write_node(block.pageno, &block.page);
			write_node(newpage, &newblock);
			p.pageno = block.page.parent;
			parentkey(block.pageno, &p);
			p.page.node.intnode.key[p.offset] = block.page.nelmts;
			insert_key(tree, rootpage, newpage, newblock.nelmts, &p);
		}
		tpage = (rblock) ? newpage : block.pageno;
	}
	stuff_page(tidpos, &tpage);
	tidpos->line_id = tline;

	if (create)
		write_node(*tid, &newroot);

}

/*
**	Takes a pair of tids from a file and sequentially replaces the
**	old tid with the new tid in the secondary btree relation
*/
void
repl_btree(register char *replbatch, long count)
{
	register int	i, j;
	tid_t		uptid;
	desc_t		d;
	char		tids[2 * LIDSIZE], key[2 * LIDSIZE], newkey[2 * LIDSIZE];
	extern desc_t	Btreesec;

	if (count > 0) {
		/* set up descriptor for sort */
		d.d_off[1] = 0;
		d.d_off[2] = LIDSIZE;
		d.d_fmt[1] = d.d_fmt[2] = INT_CONST;
		d.d_len[1] = d.d_len[2] = LIDSIZE;
		d.d_given[1] = 1;
		d.d_given[2] = 2;
		d.d_r.r_spec = M_ORDER;
		d.d_r.r_attrc = 2;
		d.d_r.r_width = 2 * LIDSIZE;
		sortfile(replbatch, &d, FALSE);
		if ((Repl_outfp = fopen(ztack(REPL_OUT, Fileset), "r")) == NULL)
			syserr("can't open replace file after sort in insertbtreee\n");
		clearkeys(&Btreesec);
		for (i = 0; i < count; ++i) {
			if (fread(tids, 1, 2 * LIDSIZE, Repl_outfp) != 2 * LIDSIZE)
				syserr("read error in insert_btree");
			ingres_setkey(&Btreesec, key, tids, 2);
			if (getequal(&Btreesec, key, newkey, &uptid)) {
				printup(&Btreesec, key);
				syserr("getequal error in insert_btree");
			}
			/* place new tid in place */
			ingres_setkey(&Btreesec, newkey, tids + LIDSIZE, 2);
			if ((j = replace(&Btreesec, &uptid, newkey, TRUE)) != 0)
				if (j == 1)
					continue;
				else
					syserr("bad replace in insert_btree");
		}
		fclose(Repl_outfp);
		unlink(replbatch);
		unlink(ztack(REPL_OUT, Fileset));
	}
}

int
insert_mbtree(desc_t *d, char *btree, long *lid, long *tid, tid_t *tidpos)
{
	register int 	i, j;
	long		page;
	long		l;
	locator_t	tidloc;
	long		ntid;
	long		t;

	page = RT;
	t = 0;
	for (i = 0; i < d->d_r.r_dim - 1; ++i) {
		if (lid[i] && lid[i+1]) {
			if ((t = get_tid(page, lid[i], &tidloc)) < -1)
				return(-1);
			if (t != -1)
				page = t;
		}
		if (!lid[i] || t == -1 || !lid[i+1]) {
			for (j = i; j < d->d_r.r_dim - 1; ++j) {
				if (j != i)
					lid[j] = 1;
				else if (t == -1 || !lid[i])
					lid[j] = last_lid(page);
				insert_btree(btree, page, lid[j], &ntid, tidpos, j + 2);
				page = ntid;
			}
			lid[d->d_r.r_dim - 1] = 1;
			break;
		}
	}
	l = last_lid(page);
	if (lid[d->d_r.r_dim - 1] < 0 || lid[d->d_r.r_dim - 1] >  l)
		return(-1);
	if (!lid[d->d_r.r_dim - 1])
		lid[d->d_r.r_dim - 1] = l;
	insert_btree(btree, page, lid[d->d_r.r_dim - 1], tid, tidpos, 0);

	return (0);
}
