/* st_shlib.c - SunOS 4.0 shared library routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_shlib_c_sccsid[] = "@(#)st_shlib.c	1.18 13/5/92 (UKC)";

#include <mtrprog/ifdefs.h>

#ifdef OS_SUNOS_4
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <a.out.h>
#include <link.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "st_priv.h"
#include "data.h"

/*  BUG: this should be in a header file, but I don't know which one.
 *       ANSI presumably forbids putting it in stdio.h.
 */
FILE *fdopen PROTO((int fd, const char *mode));

static const char **add_to_env PROTO((const char *s));

/*  Dig out the list of loaded shared libraries and run time linked
 *  global addresses.  This basically involves trundling down linked	
 *  lists in the target via dread().  This must be called after the
 *  target has started and the mapping been done.
 */
int
get_shlibs_and_global_addrs(alloc_id, st, addr_dynamic, p_shlibs)
alloc_id_t alloc_id;
symtab_t *st;
taddr_t addr_dynamic;
shlib_t **p_shlibs;
{
	struct link_dynamic ldbuf;
	struct link_dynamic_1 ld1buf;
	struct link_map lmbuf, *lm;
	struct ld_debug lddbuf;
#ifdef COFF_SUN386
	struct syment nm;
#define rtc_sp		rtc_un.rtc_sp_coff
#define n_corename	n_offset
#else
	struct nlist nm;
#define n_corename	n_un.n_name
#endif
	struct rtc_symb rtcbuf, *rtc;
	shlib_t *shlist, *sh;
	char buf[256];

	if (dread(addr_dynamic, (char *)&ldbuf, sizeof(ldbuf)) != 0)
		return -1;
	
	/*  Get the list of addresses of shared data objects.
	 */
	if (dread((taddr_t)ldbuf.ldd, (char *)&lddbuf, sizeof(lddbuf)) != 0)
		return -1;
	for (rtc = lddbuf.ldd_cp; rtc != NULL; rtc = rtcbuf.rtc_next) {
		if (dread((taddr_t)rtc, (char *)&rtcbuf, sizeof(rtcbuf)) != 0)
			return -1;
		if (dread((taddr_t)rtcbuf.rtc_sp, (char *)&nm, sizeof(nm)) != 0)
			return -1;
		if (dgets((taddr_t)nm.n_corename, buf, sizeof(buf)) != 0)
			return -1;
		st->st_addrlist = insert_global_addr(st->st_alloc_id,
						     st->st_addrlist,
						     save_fname(st, buf, TRUE),
						     nm.n_value);
	}

	/*  Now dig out the list of loaded shared libraries.
	 */
	if (dread((taddr_t)ldbuf.ld_un.ld_1, (char *)&ld1buf, sizeof(ld1buf)) != 0)
		return -1;
	shlist = NULL;
	for (lm = ld1buf.ld_loaded; lm != NULL; lm = lmbuf.lm_next) {
		if (dread((taddr_t)lm, (char *)&lmbuf, sizeof(lmbuf)) != 0)
			return -1;
		if (dgets((taddr_t)lmbuf.lm_name, buf, sizeof(buf)) != 0)
			return -1;
		sh = (shlib_t *)alloc(alloc_id, sizeof(shlib_t));
		sh->sh_name = alloc_strdup(alloc_id, buf);
		sh->sh_addr = (taddr_t)lmbuf.lm_addr;
		sh->sh_next = shlist;
		shlist = sh;
	}

	*p_shlibs = shlist;
	return 0;
}

/*  Return a pointer to an array of strings which consists of the
 *  environment plus string s, which should be of the form
 *  "name=value".
 */
static const char **
add_to_env(s)
const char *s;
{
	extern const char **environ;
	const char **sptr, **envp, **src, **dst;

	for (sptr = environ; *sptr != NULL; ++sptr)
		;
	envp = dst = (const char **)e_malloc(sizeof(char *) * (sptr - environ + 2));
	src = environ;
	while (*src != NULL)
		*dst++ = *src++;
	*dst++ = s;
	*dst = NULL;
	return envp;
}

/*  Get the list of shared libraries that the execfile will load
 *  when it is run.  We do this in the same way as ldd(1), by
 *  running the target with LD_TRACE_LOADED_OBJECTS set in its
 *  environment and parsing the output.
 */
int
get_preload_shlib_list(alloc_id, execfile, p_shlibs)
alloc_id_t alloc_id;
const char *execfile;
shlib_t **p_shlibs;
{
	int pid, wpid;
	int fds[2];
	char buf[256], name[256];
	char *pos;
	const char **envp;
	FILE *fp;
	shlib_t *sh, *shlist;

	envp = add_to_env("LD_TRACE_LOADED_OBJECTS=1");
	if (pipe(fds) != 0) {
		errf("Pipe failed (%m)");
		free((char *)envp);
		return -1;
	}
	if ((fp = fdopen(fds[0], "r")) == NULL) {
		errf("Fdopen failed (%m)");
		free((char *)envp);
		return -1;
	}
	if ((pid = vfork()) == -1) {
		errf("Vfork failed (%m)");
		fclose(fp);
		close(fds[1]);
		free((char *)envp);
		return -1;
	}
	if (pid == 0) {
		close(fds[0]);
		if (fds[1] != 1)
			dup2(fds[1], 1);
		execle(execfile, execfile, (char *)NULL, envp);
		errf("Can't exec %s (%m)\n", execfile);
		_exit(1);
	}

	free((char *)envp);
	close(fds[1]);

	shlist = NULL;
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		/*  We seem to get carriage returns as well as newlines
		 */
		if ((pos = strchr(buf, '\r')) != NULL)
			*pos = '\0';
		if ((pos = strchr(buf, '\n')) != NULL)
			*pos = '\0';
		if (sscanf(buf, "%*s => %s", name) == 1) {
			sh = (shlib_t *)alloc(alloc_id, sizeof(shlib_t));
			sh->sh_name = alloc_strdup(alloc_id, name);
			sh->sh_addr = 0;
			sh->sh_next = shlist;
			shlist = sh;
		}
	}
	fclose(fp);
	while ((wpid = wait((int *)NULL)) != pid && wpid != -1)
		;
	*p_shlibs = shlist;
	return 0;
}
#endif /* OS_SUNOS_4 */
