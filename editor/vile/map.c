/*	MAP:	These routines provide a simple interface for the map command
 *		written by Otto Lind
 *		6/3/93
 *
 * $Log: map.c,v $
 * Revision 1.1  1994/02/01 03:29:32  jkh
 * Initial revision
 *
 * Revision 1.9  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.8  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.7  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.6  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.5  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.4  1993/06/30  17:42:50  pgf
 * cleaned up, made SPEC bindings work
 *
 * Revision 1.3  1993/06/28  20:04:14  pgf
 * string_to_key replaced with prc2kcod
 *
 * Revision 1.2  1993/06/10  16:13:52  pgf
 * moved the recursion check from map_proc to map_check.  this makes
 * nesting possible, but loops are not detected....
 *
 * Revision 1.1  1993/06/10  14:58:47  pgf
 * ansification, and a little reformatting (sorry, otto...  :-)
 *
 * otto's Revision 1.2  93/06/06  21:27:33  otto
 * Added unmap, cleaned up.
 * 
 * otto's Revision 1.1  93/06/04  23:42:11  otto
 * Initial revision
 * 
 */

#include "estruct.h"
#include "edef.h"

#define DEFAULT_REG     -1

#define OPT_MAP_DISPLAY	!SMALLER
#define	MAPPED_LIST_NAME	ScratchName(Mapped Characters)

typedef struct _mapping {
    int			key;		/* key that is mapped */
    char		*kbdseq;	/* keyboard sequnce to replace */
    CMDFUNC		*oldfunc;	/* used in unmap operation */
    struct _mapping	*next;		/* used to form linked list */
} Mapping;

static Mapping	*mhead;			/* ptr to head of linked list */
static Mapping	*keymapped = NULL;	/* current mapped key seq used */
static TBUFF	*MapMacro;

static Mapping * search_map P(( int ));
static int install_map P(( int, char * ));
static int remove_map P(( int ));
#if OPT_MAP_DISPLAY
static void makecharslist P(( int, char * ));
static int show_mapped_chars P(( void ));
static int update_mapped_list P(( void ));
#endif

/*
** search for key in mapped linked list
*/
static Mapping *
search_map(key)
int	key;
{
	register Mapping	*m;

	for (m = mhead; m; m = m->next)
		if (m->key == key)
			return m;
	return NULL;
}

/*
** install key sequence into appropriate table
*/
static int
install_map(key, v)
int		key;
char	*v;
{
	extern	CMDFUNC	f_map_proc;
	register Mapping	*m;
	char	temp[NSTRING];
	int	test;

	/* check for attempted recursion
	 * patch: prc2kcod assumes only a single key-sequence
	 */
	test = prc2kcod(string2prc(temp, v));
	if (test == key || search_map(test) != NULL) {
		mlforce("[Attempted recursion]");
		return FALSE;
	}

	if ((m = search_map(key)) != NULL) {
		free(m->kbdseq);
	} else {
		m = typealloc(Mapping);
		if (m == NULL)
			return FALSE;
		m->next = mhead;
		mhead = m;
	}

	m->kbdseq = strmalloc(v);
	if (m->kbdseq == NULL)
		return FALSE;
	m->key = key;
	
	return install_bind(key, &f_map_proc, &m->oldfunc);
}	

/*
** Remove entry from list and restore the old function pointer.
*/
static int
remove_map(key)
int	key;
{
	register Mapping	*m;
	register Mapping	*pm;
	int	status = FALSE;

	for (m = mhead, pm = NULL; m; m = m->next) {
		if (m->key == key) {
			if (pm == NULL)
				mhead = m->next;
			else
				pm->next = m->next;

			status = rebind_key(key, m->oldfunc);
			free(m->kbdseq);
			free((char *)m);
			break;
		}
		pm = m;
	}
	return status;
}

#if OPT_MAP_DISPLAY
/*ARGSUSED*/
static void
makecharslist(flag, ptr)
int	flag;
char	*ptr;
{
	register Mapping *m;
	char	temp[NSTRING];

	bprintf("--- Mapped Characters %*P", term.t_ncol-1, '-');
	for (m = mhead; m != 0; m = m->next) {
		bprintf("\n%s ", kcod2prc(m->key, temp));
		bprintf("%s", string2prc(temp, m->kbdseq));
	}
}

static int
show_mapped_chars()
{
	return liststuff(MAPPED_LIST_NAME, makecharslist, 0, (char *)0);
}

static int
update_mapped_list()
{
	int	status = TRUE;

	if (find_b_name(MAPPED_LIST_NAME) != 0) {
		WINDOW	*savewp = curwp;
		status = show_mapped_chars();
		if (curwp != savewp) {
			(void)swbuffer(savewp->w_bufp);
			curwp = savewp;
		}
	}
	return status;
}
#else
#define update_mapped_list() TRUE
#endif

/*
** if a mapped char, save for subsequent processing
*/
void
map_check(key)
int	key;
{
	Mapping	*nkeymap;
	nkeymap = search_map(key);
	if (nkeymap != NULL && nkeymap == keymapped) {
		finish_kbm();
                mlforce("[Recursive map]");
		keymapped = NULL;
                return;
        }
	keymapped = nkeymap; 
}

/*
** set a map for the character/string combo
*/
/* ARGSUSED */
int
map(f, n)
int f,n;
{
	int	status;
	char 	kbuf[NSTRING];
	char 	val[NSTRING];

#if OPT_MAP_DISPLAY
	if (end_named_cmd()) {
		return show_mapped_chars();
	}
#endif
	kbuf[0] = EOS;
	status = mlreply("map key: ", kbuf, sizeof(kbuf));
	if (status != TRUE)
		return status;

	hst_glue(' ');
	val[0] = EOS;
	status = mlreply("map value: ", val, sizeof(val));
	if (status != TRUE)
		return status;

	if (install_map(prc2kcod(kbuf), val) != TRUE) {
		mlforce("[Mapping failed]");
		return FALSE;
	}
	return update_mapped_list();
}

/*
** remove map entry, restore saved CMDFUNC for key
*/
/* ARGSUSED */
int
unmap(f, n)
int f,n;
{
	int	status;
	char 	kbuf[NSTRING];

	kbuf[0] = EOS;
	status = mlreply("unmap key: ", kbuf, sizeof(kbuf));
	if (status != TRUE)
		return status;

	if (remove_map(prc2kcod(kbuf)) != TRUE) {
		mlforce("[Key not mapped]");
		return FALSE;
	}
	return update_mapped_list();
}

/*
** use the keyboard replay macro code to execute the mapped command
*/
/*ARGSUSED*/
int
map_proc(f, n)
int	f,n;
{
	/* Could be null if the user tries to execute map_proc directly */
	if (keymapped == NULL || keymapped->kbdseq == NULL) {
		mlforce("[Key not mapped]");
		return FALSE;
	}

	(void)tb_init(&MapMacro, abortc);
	if (n != 1) {
		char	num[10];
		(void)lsprintf(num, "%d", n);
		if (!tb_sappend(&MapMacro, num))
			return FALSE;
	}
	if (!tb_sappend(&MapMacro, keymapped->kbdseq))
		return FALSE;

	keymapped = NULL;

	return start_kbm(1, DEFAULT_REG, MapMacro);
}
