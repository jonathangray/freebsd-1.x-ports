/*
 * Common declarations, includes, and other goodies.
 *
 * @(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/common.h,v 1.1 1993/07/19 20:04:30 nate Exp $
 */


#include "../common/conf.h"
#include <stdio.h>
#ifndef BSD_42
#include <sys/types.h>
#endif /* BSD_42 */
#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/file.h>
#ifdef BSD2_10
#include <short_names.h>
#endif /* BSD2_10 */

#include "../common/nntp.h"

#ifdef SYSLOG
# ifdef FAKESYSLOG
#  include "fakesyslog.h"
# else
#  include <syslog.h>
# endif
#endif

#ifdef USG
extern struct passwd *getpwent(), *getpwuid(), *getpwnam();
#define iolen_t unsigned
# include <string.h>
#else /* not USG */
# include <strings.h>
# include <sys/wait.h>
#define iolen_t int
#endif /* not USG */

#ifdef NDIR
#ifdef M_XENIX
# include <sys/ndir.h>
#else
# include <ndir.h>
#endif
#else /* not NDIR */
# include <sys/dir.h>
#endif /* not NDIR */

#ifdef FCNTL
# include <fcntl.h>
#endif /* FCNTL */

#ifdef ultrix
extern char * index();
extern char * rindex();
#endif

/*
 * <dbm.h> stupidly defines NULL, which is why the following
 * brain death is necessary.
 */

#ifdef DBM
# ifdef DBZ
#  include <dbz.h>
# else /* DBZ */
#  undef NULL
#  include <dbm.h>
#  undef NULL
#  define NULL	0
# endif /* DBZ */
#else
# ifdef NDBM
#  include <ndbm.h>
# endif /* NDBM */
#endif /* DBM */

#ifdef TIMEOUT
#ifndef TIMERS
#define TIMERS
#endif
#endif

#ifdef LOGINCHECK
#ifndef TIMERS
#define TIMERS
#endif
#endif

#ifdef BATCHCHECK
#ifndef TIMERS
#define TIMERS
#endif
#endif

/*
 * Some generic maximums.
 */

#ifndef MAXPATHLEN
#define	MAXPATHLEN	1024
#endif

#ifndef MAXHOSTNAMELEN
#define	MAXHOSTNAMELEN	256
#endif

#ifndef MINFREE
#define MINFREE         0
#endif

#ifndef POSTBUFFER
#define POSTBUFFER      0
#endif

#define	MAXBUFLEN	1024

/*
 * For "spew()"
 */

#define	ARTICLE	0
#define	HEAD	1
#define	BODY	2
#define	STAT	3

/*
 * For "ngmatch()"
 */

#define	ALLBUT	1

#define	valid_art(s)	(atoi(s) != 0)

#define	putline(s)	fputs((s), stdout); putchar('\r'); putchar('\n');

extern	int	errno;

extern	char	*gets(), *fgets();
extern	char	*mktemp();
extern	FILE	*open_valid_art();
extern	FILE	*openartbyid();
extern	char	*gethistent();
extern	int	restreql();
extern	int	s1strneql();	/* for ngmatch */
#ifdef DEBUG
void debugup(), debugdown();
#endif
#ifdef SETPROCTITLE
void setproctitle();
#endif

extern	char	spooldir[];
extern	char	activefile[];
extern	char	distributionsfile[];
extern	char	newsgroupsfile[];
extern	char	accessfile[];
extern	char	historyfile[];
extern	char	ngdatefile[];
extern	char	inews[];
extern	char	rnews[];

extern	char	**group_array;
extern	char	*actbuf;
extern	int	num_groups;
extern	char	*homedir;
extern	int	ingroup;
extern	int	maxgroups;
#ifdef DYNAMIC_ART_ARRAY
extern	int	*art_array;
extern	unsigned int size_art_array;
#else
extern	int	art_array[];
#endif
extern	int	art_ptr;
extern	FILE	*art_fp;
extern	int	num_arts;
extern	int	uid_poster, gid_poster;
extern	char	*home_poster;
extern	int	canread, canpost, canxfer;
extern	char	**ngpermlist;
extern	int	ngpermcount;

extern	char	nntp_version[];

extern	char	hostname[];
extern	int	debug;

#ifdef LOG
extern	int	grps_acsd, arts_acsd;

extern	int	ih_accepted;
extern	int	ih_rejected;
extern	int	ih_failed;

extern	int	nn_told;
extern	int	nn_took;
#endif
