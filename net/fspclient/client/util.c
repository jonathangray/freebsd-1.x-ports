    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *  Copyright (c) 1992 by Phil Richards (pgr@prg.ox.ac.uk)             *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "client.h"
#include <setjmp.h>

char *env_dir    = (char*)0;
char *env_myport = (char*)0;
char *env_host   = (char*)0;
char *env_port   = (char*)0;
int standalone = 1;
u_short client_buf_len = UBUF_SPACE;
u_short client_net_len;
int readme_max = -1;

static int oldserver = 0;

#ifndef __STDC__
#include <varargs.h>
void
ffprintf(va_alist)
    va_dcl
{
    FILE *out;
    char *fmt;
    va_list args;

    va_start(args);
    out = va_arg(args, FILE *);
    fmt = va_arg(args, char *);

    if (out)
    {
	(void)vfprintf(out, fmt, args);
	(void)fflush(out);
    }

    va_end(args);
}
#else /* __STDC__ */
#include <stdarg.h>
void
ffprintf(FILE *out, const char *fmt, ...)
{
    if (out)
    {
	va_list args;

	va_start(args, fmt);
	(void)vfprintf(out, fmt, args);
	va_end(args);

	(void)fflush(out);
    }
}
#endif /* __STDC__ */

static jmp_buf my_fgets_env;

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
my_fgets_intr(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
my_fgets_intr(int sig)
#endif /* ANSI_PROTOTYPES */
{
    ffprintf(STDPROMPT,"\n");
    longjmp(my_fgets_env, 0);
}

char *
#ifndef ANSI_PROTOTYPES
my_fgets(s, n, fs)
    char *s;
    int n;
    FILE *fs;
#else /* ANSI_PROTOTYPES */
my_fgets(char *s, int n, FILE *fs)
#endif /* ANSI_PROTOTYPES */
{
#ifndef ANSI_PROTOTYPES
    RETSIGTYPE (*oldintr)();
#else /* ANSI_PROTOTYPES */
    RETSIGTYPE (*oldintr)(int);
#endif /* ANSI_PROTOTYPES */
    int resetintr = 0;
    char *retval;

    if (!setjmp(my_fgets_env))
    {
	oldintr = signal(SIGINT, my_fgets_intr);
	resetintr = 1;
	retval = fgets(s, n, fs);
    }
    else
    {
	*s = '\0';
	retval = s;
    }

    if (resetintr)
	(void)signal(SIGINT, oldintr);

    return retval;
}

/* rewritten completely by pgr */
char *
#ifndef ANSI_PROTOTYPES
util_abs_path(buf)
    const char *buf;
#else /* ANSI_PROTOTYPES */
util_abs_path(const char *buf)
#endif /* ANSI_PROTOTYPES */
{
    char *dstp, *srcp, *slash, *path;

    if (env_dir == 0)
	env_dir = strdup("/");

    if (buf == 0)
	buf = "";

    if (buf[0] == '/')
    {
	path = (char *)malloc(strlen(buf) + 2);
	(void)sprintf(path, "%s/", buf);
    }
    else
    {
	path = (char *)malloc(strlen(env_dir) + strlen(buf) + 4);
	(void)sprintf(path, "/%s/%s/", env_dir, buf);
    }

    for (slash = path, dstp = srcp =  path + 1; *srcp; srcp++)
    {
	if (*srcp == '/')
	{
	    if (dstp == slash + 1) /* "//" */
		continue;

	    if (dstp == slash + 2 && slash[1] == '.')
	    {	/* "/./" */
		dstp = slash + 1;
		continue;
	    }

	    if (dstp == slash + 3 && slash[1] == '.' && slash[2] == '.')
	    {	/* "/../" */
		if (slash != path)
		    while (*(--slash) != '/')
			;
		dstp = slash + 1;
		continue;
	    }

	    slash = dstp;
	}

	*(dstp++) = *srcp;
    }

    if (slash != path)
	*slash = '\0';
    else
	*dstp = '\0';

    /* this returns a path with a "/" at the beginning */
    return path;
}

char *
#ifndef ANSI_PROTOTYPES
util_getwd(p)
    char *p;
#else /* ANSI_PROTOTYPES */
util_getwd(char *p)
#endif /* ANSI_PROTOTYPES */
{
    if (p)
	(void)strcpy(p, env_dir);
    return(p);
}

RDIRENT **
#ifndef ANSI_PROTOTYPES
get_dir_blk(path)
    char *path;
#else /* ANSI_PROTOTYPES */
get_dir_blk(char *path)
#endif /* ANSI_PROTOTYPES */
{
    RDIRENT **dp = (RDIRENT **)0;
    char *p1, *p2, *fpath, buf[2*UBUF_SPACE];
    u_long pos;
    int cnt, k, len, rem, acc, at_eof;
    UBUF *ub;

    if (!validate_operation(path, LITERAL_DIR | DIR_PRIV))
	return 0;

    fpath = util_abs_path(path);

    for (pos = 0, at_eof = acc = cnt = 0; ; )
    {
	while ((acc < UBUF_SPACE) && !at_eof)
	{
	    ub = client_interact(CC_GET_DIR, pos,
				 strlen(fpath), fpath+1,
				 2, (char *)&client_net_len);

	    if (client_intr_state > 1 || ub == 0)
	    {
		if (dp)
		    (void)free((char*)dp);
		(void)free(fpath);

		return((RDIRENT **) 0);
	    }

	    if(ub->cmd == CC_ERR)
	    {
		if (dp)
		    (void)free((char*)dp);
		(void)free(fpath);

		errno = EACCES;
		return((RDIRENT **) 0);
	    }

	    if (ub->len < client_buf_len)
		at_eof = 1;

	    for (p1 = ub->buf, p2 = buf + acc, k = ub->len; k--; )
		*p2++ = *p1++;

	    acc += ub->len;
	    pos += ub->len;
	}

	len = (acc >= UBUF_SPACE)? UBUF_SPACE: acc;

	for (p2 = buf, rem = len, k = 0; ; k++)
	{
	    if (rem < RDHSIZE)
		break;

	    if (((RDIRENT *) p2)->type == RDTYPE_SKIP)
		break;

	    if (((RDIRENT *) p2)->type == RDTYPE_END )
	    {
		k++;
		break;
	    }

	    p2 += RDHSIZE;
	    rem -= (RDHSIZE+1);

	    while (*p2++)
		rem--;

	    while ((p2 - buf) & 3)
	    {
		p2++;
		rem--;
	    }
	}

	if (cnt)
	    dp = (RDIRENT**)realloc((char*)dp, (cnt+k+1) * sizeof(RDIRENT*));
        else
	    dp = (RDIRENT**)malloc((cnt+k+1) * sizeof(RDIRENT*));

	if (!dp)
	{
	    (void)free(fpath);

	    ffprintf(STDERR, "?directory reading out of memory\n");
	    return (RDIRENT**)0;
	}

	for (p2 = buf, rem = len; ; cnt++)
	{
	    RDIRENT *p2p;
	    int l;

	    if (rem < RDHSIZE)
		break;

	    p2p = (RDIRENT*)p2;

	    if (p2p->type == RDTYPE_SKIP)
		break;

	    if (p2p->type == RDTYPE_END)
	    {
		(void)free(fpath);

		dp[cnt] = 0;
		return dp;
	    }

	    l = strlen(p2p->name);
	    dp[cnt] = (RDIRENT*)malloc(RDHSIZE + l + 1);

	    dp[cnt]->time = ntohl(p2p->time);
	    dp[cnt]->size = ntohl(p2p->size);
	    dp[cnt]->type = p2p->type;

	    (void)strcpy(dp[cnt]->name, p2p->name);

	    p2  += RDHSIZE + l + 1;
	    rem -= RDHSIZE + l + 1;

	    while ((p2 - buf) & 3)
	    {
		p2++;
		rem--;
	    }
	}

	if (acc < UBUF_SPACE)
	{
	    (void)free(fpath);

	    dp[cnt] = 0;
	    return dp;
	}

	for (p1 = buf + UBUF_SPACE, p2 = buf, k = (acc -= UBUF_SPACE); k--; )
	    *p2++ = *p1++;
    }
}

int
#ifndef ANSI_PROTOTYPES
util_download_main(path, fpath, fd, cmd, offset, length)
    char *path;
    char *fpath;
    int fd;
    int cmd;
    u_long offset;
    u_long length;
#else /* ANSI_PROTOTYPES */
util_download_main(char *path, char *fpath,
		   int fd,
		   int cmd, u_long offset, u_long length)
#endif /* ANSI_PROTOTYPES */
{   
    u_long pos;
    u_int sent_time;
    UBUF *ub;
    struct timeval starttime;
    struct stat rfstat;
    int retval = 0;

    if (!askprompt && client_intr_state > 1)
	return 0;

    if (util_stat(fpath, &rfstat) < 0)
    {
	ffprintf(STDERR, "?remote file `%s': %s\n", fpath, sys_errlist[errno]);
	return -1; 
    }

    if (offset > rfstat.st_size || length <= 0)
	return 0;

    (void)gettimeofday(&starttime, (struct timezone *)0);

    for (pos = offset, sent_time = 0;
	 client_intr_state < 2 && retval == 0 && pos - offset < length;
	 )
    {   
	int wrote;

        ub = client_interact(cmd, pos, strlen(fpath), fpath+1,
			     2, (char *)&client_net_len);

	if (client_intr_state > 1 || ub == 0 || ub->cmd == CC_ERR)
	    break;

	if (client_trace == 1 && (udp_sent_time != sent_time))
	{
	    sent_time = udp_sent_time;
	    if (client_buf_len == UBUF_SPACE)
		ffprintf(STDINFO,"\r%luk  ", 1 + (pos>>10));
	    else
		ffprintf(STDINFO,"\r%lu   ", pos);
	}
	else if (client_trace >= 2)
	{
	    (void)fputc('#', STDINFO);
	    (void)fflush(STDINFO);
	}
 
        wrote = write(fd, ub->buf, ub->len);

	if (wrote < 0)
	{
	    ffprintf(STDERR, "?download: cannot write to local file: %s\n",
		     sys_errlist[errno]);
	    retval = -1;
	    break;
	}
	else if (wrote == 0)
	    break;

	pos += wrote;
	if (wrote < ub->len || ub->len < client_buf_len)
	    break;
    }

    if (retval == 0)
	retval = pos - offset;

    if (client_trace)
    {
	struct timeval endtime;
	int millicnt;

	(void)gettimeofday(&endtime, (struct timezone *)0);
	millicnt = 1000 * (endtime.tv_sec - starttime.tv_sec) +
		   (endtime.tv_usec - starttime.tv_usec) / 1000;

	ffprintf(STDINFO,
		      "%cdone %lu bytes in %3.01f seconds (%3.02f K/sec)\n",
		      (client_trace == 1 && retval < 0)? '\r': '\n',
		      (u_long)(pos - offset),
		      (double)millicnt / 1000.0,
		      millicnt == 0? (double)(pos - offset)/1024.0
				    :((double)(pos - offset)/1024.0) /
					((double)millicnt/1000.0));
	if (client_trace == 3)
	    print_comm_stats(STDERR);

	(void)fflush(STDERR);
    }

    return retval;
}

int
#ifndef ANSI_PROTOTYPES
util_download(path, fd, offset, length)
    char *path;
    int fd;
    u_long offset;
    u_long length;
#else /* ANSI_PROTOTYPES */
util_download(char *path, int fd, u_long offset, u_long length)
#endif /* ANSI_PROTOTYPES */
{
    int bytes;
    char *fpath;

    fpath = util_abs_path(path);
    bytes = util_download_main(path, fpath, fd, CC_GET_FILE, offset, length);
    (void)free(fpath);

    return bytes;
}

int
#ifndef ANSI_PROTOTYPES
util_grab_file(path, fd, offset, length)
    char *path;
    int fd;
    u_long offset;
    u_long length;
#else /* ANSI_PROTOTYPES */
util_grab_file(char *path, int fd, u_long offset, u_long length)
#endif /* ANSI_PROTOTYPES */
{
    int bytes;
    char *fpath;
    UBUF *ub;

    fpath = util_abs_path(path);
    bytes = util_download_main(path, fpath, fd, CC_GRAB_FILE, offset, length);

    if (bytes != length)
    {
	(void)free(fpath);
	return -1;
    }

    ub = client_interact(CC_GRAB_DONE, 0L, strlen(fpath), fpath+1, 0, NULLP);
    (void)free(fpath);

    return -(ub == 0);
}

int
#ifndef ANSI_PROTOTYPES
util_upload(path,fp)
    char *path;
    FILE *fp;
#else /* ANSI_PROTOTYPES */
util_upload(char *path, FILE *fp)
#endif /* ANSI_PROTOTYPES */
{   
    u_long pos;
    u_int bytes, first, sent_time;
    char *fpath, buf[UBUF_SPACE];
    UBUF *ub;
    struct timeval starttime;
    struct stat wfstat;

    if (fstat(fileno(fp), &wfstat) < 0)
    {
	ffprintf(STDERR,"unexpected error on file descriptor %d: %s\n",
		      fileno(fp), sys_errlist[errno]);
	return(-1); 
    }

    (void)gettimeofday(&starttime, (struct timezone *)0);
 
    if (client_trace >= 2)
	ffprintf(STDINFO, "uploading `%s' (%d bytes)\n", path, wfstat.st_size);
 
    fpath = util_abs_path(path);

    for (sent_time = 0, pos = 0, first = 1; ; first = 0)
    {   
	if ((bytes = fread(buf,1,client_buf_len,fp)) || first)
	{
	    ub = client_interact(CC_UP_LOAD,pos, bytes,buf, 0,NULLP);

	    if (client_trace == 1 && (udp_sent_time != sent_time))
	    {
		sent_time = udp_sent_time;
		if (client_buf_len == UBUF_SPACE)
		    ffprintf(STDINFO,"\r%luk  ", 1 + (pos>>10));
		else
		    ffprintf(STDINFO,"\r%lu   ", pos);
	    }
	    else if (client_trace >= 2)
	    {
		(void)fputc('#', STDINFO);
		(void)fflush(STDINFO);
	    }
	}
	else
	    ub = client_interact(CC_INSTALL,pos,strlen(fpath),fpath+1,0,NULLP);

	if (client_intr_state > 1 || ub == 0)
	{
	    (void)free(fpath);
	    return 1;
	}
 
        if (ub->cmd == CC_ERR)
        {    
	    (void)free(fpath);
	    return(1); 
        }   
 
	if (!bytes && !first)
	    break;
        pos += bytes;
    }

    if (client_trace)
    {
	struct timeval endtime;
	int millicnt;

	(void)gettimeofday(&endtime, (struct timezone *)0);
	millicnt = 1000 * (endtime.tv_sec - starttime.tv_sec) +
		   (endtime.tv_usec - starttime.tv_usec) / 1000;

	ffprintf(STDINFO,
		      "%cdone %lu bytes in %3.01f seconds (%3.02f K/sec)\n",
		      (client_trace == 1)? '\r': '\n', pos,
		      (double)millicnt / 1000.0,
		      millicnt == 0? (double)pos/1024.0
				    :((double)pos/1024.0) /
					((double)millicnt/1000.0));
	if (client_trace == 3)
	    print_comm_stats(STDERR);

	(void)fflush(STDERR);
    }

    (void)free(fpath);
    return(0);
}

void
#ifndef ANSI_PROTOTYPES
util_get_env()
#else /* ANSI_PROTOTYPES */
util_get_env(void)
#endif /* ANSI_PROTOTYPES */
{
    char *p, *env_opts;

    if ((env_opts = (char *)getenv("FSP_OPTS")))
    {
	char *colon = (char*)0;

	/* ok, ok, so it does more tests than it needs; so what?             */
	/* at least the indenting is under control...                        */
	/* a good compiler will eliminate all the ones it doesn't need.. :-) */
	if (*env_opts)
	{
	    env_host = env_opts;
	    if((colon = strchr(env_opts, ':')) != (char*)0)
	    {
		*colon   = '\0';
		env_opts = colon + 1;
	    }
	    env_host = strdup(env_host);
	}

	if (colon && *env_opts)
	{
	    env_port = env_opts;
	    if((colon = strchr(env_opts, ':')) != (char*)0)
	    {
		*colon   = '\0';
		env_opts = colon + 1;
	    }
	    env_port = strdup(env_port);
	}

	if (colon && *env_opts)
	{
	    env_myport = env_opts;
	    if((colon = strchr(env_opts, ':')) != (char*)0)
	    {
		*colon   = '\0';
		env_opts = colon + 1;
	    }
	    env_myport = strdup(env_myport);
	}

	if (colon && *env_opts)
	    env_dir = strdup(env_opts);
    }

    if (!env_host && !(env_host = (char *)getenv("FSP_HOST")) && standalone)
    {
	ffprintf(STDERR,"Env var FSP_HOST not defined\n");
	exit(1);
    }

    if (!env_port && !(env_port = (char *)getenv("FSP_PORT")) && standalone)
    {
	ffprintf(STDERR,"Env var FSP_PORT not defined\n");
	exit(1);
    }

    if (!env_dir && !(env_dir = (char *)getenv("FSP_DIR")) && standalone)
	env_dir = strdup("/");

    if (!env_myport && !(env_myport = (char *)getenv("FSP_LOCALPORT")) && standalone)
	env_myport = strdup("0");

    if ((char *)getenv("FSP_TRACE"))
	client_trace = 1;

    if ((p = (char *)getenv("FSP_TIMEOUT")))
	time_out = atoi(p);

    if ((p = (char *)getenv("FSP_BUF_SIZE")))
    {
	client_buf_len = atoi(p);

	if (client_buf_len > UBUF_SPACE)
	    client_buf_len = UBUF_SPACE;
    }

    client_net_len = htons(client_buf_len);

    if ((p = (char *)getenv("FSP_DELAY")))
    {
	target_delay = atol(p);
	if (target_delay < MINDELAY)
	    target_delay = MINDELAY;
    }
}

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
client_intr(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
client_intr(int sig)
#endif /* ANSI_PROTOTYPES */
{
    switch(client_intr_state)
    {
	case 0: exit(2);
	case 1: client_intr_state = 2; break;
	case 2: exit(3);
    }
}

void
#ifndef ANSI_PROTOTYPES
env_client()
#else /* ANSI_PROTOTYPES */
env_client(void)
#endif /* ANSI_PROTOTYPES */
{
    util_get_env();
    if (init_client(env_host,atoi(env_port),atoi(env_myport)) < 0)
	exit(1);
    if (isatty(fileno(STDIN)))
	STDPROMPT = 0;
    (void)signal(SIGINT, client_intr);
}

/*****************************************************************************/

static DDLIST *ddroot = 0;

void
#ifndef ANSI_PROTOTYPES
util_flushdir()
#else /* ANSI_PROTOTYPES */
util_flushdir(void)
#endif /* ANSI_PROTOTYPES */
{
    DDLIST *ddp, *ddpnext;

    if (ddroot == (DDLIST*)0)
	return;

    for (ddp = ddroot; ddp; ddp = ddpnext)
    {
	int i;
	ddpnext = ddp->next;
	if (ddp->ref_cnt)
	{
	    ffprintf(STDDBG,
		"??internal error: flushing directory cache with non-zero referenced directory\n");
	    ffprintf(STDDBG, "??(dir = %s; ref_cnt = %d)\n",
			  ddp->path, ddp->ref_cnt);
	}

	for (i = 0; (ddp->dep_root)[i]; i++)
	    (void)free((char*)(ddp->dep_root[i]));

	(void)free((char*)(ddp->dep_root));
	(void)free((char*)(ddp->path));

	(void)free((char*)ddp);
    }
    ddroot = (DDLIST*)0;
}

void
#ifndef ANSI_PROTOTYPES
util_dirtydir(path)
    char *path;
#else /* ANSI_PROTOTYPES */
util_dirtydir(char *path)
#endif /* ANSI_PROTOTYPES */
{
    DDLIST *ddp, *ddprev;
    int i;
    char *fpath;

    if (ddroot == (DDLIST*)0)
        return;

    fpath = util_abs_path(path);

    for (ddp = ddroot, ddprev = 0; ddp; ddprev = ddp, ddp = ddp->next)
        if (!strcmp(ddp->path,fpath))
            break;

    (void)free(fpath);

    if (ddp == (DDLIST*)0)
        return;

    if (ddp->ref_cnt)
    {
        ffprintf(STDDBG,
            "??internal error: flushing directory \"%s\" with non-zero reference\n", fpath);
        ffprintf(STDDBG, "??(dir = %s; ref_cnt = %d)\n",
                      ddp->path, ddp->ref_cnt);
    }

    /* a goodly number of these free's will fail */
    for (i = 0; (ddp->dep_root)[i]; i++)
        (void)free((char*)(ddp->dep_root[i]));

    if (ddprev)
	ddprev->next = ddp->next;
    else
	/* ddp == ddroot */
	ddroot = ddp->next;

    (void)free((char*)(ddp->dep_root));
    (void)free((char*)(ddp->path));

    (void)free((char*)ddp);
}

static DDLIST *
#ifndef ANSI_PROTOTYPES
util_get_cached_ddlist(fpath)
    char *fpath;
#else /* ANSI_PROTOTYPES */
util_get_cached_ddlist(char *fpath)
#endif /* ANSI_PROTOTYPES */
{
    DDLIST *ddp;

    for (ddp = ddroot; ddp; ddp = ddp->next)
	if (!strcmp(ddp->path,fpath))
	    break;

    return ddp;
}

/* it is assumed that util_get_cached_ddlist(fpath) would return (DDLIST*)0 */
static DDLIST *
#ifndef ANSI_PROTOTYPES
util_cache_contents(fpath)
    char *fpath;
#else /* ANSI_PROTOTYPES */
util_cache_contents(char *fpath)
#endif /* ANSI_PROTOTYPES */
{
    RDIRENT **dep;
    DDLIST *ddp;

    if (!(dep = get_dir_blk(fpath)))
	return (DDLIST*)0;

    ddp = (DDLIST*)malloc(sizeof(DDLIST));

    ddp->dep_root = dep;
    ddp->path     = strdup(fpath);
    ddp->ref_cnt  = 0;
    ddp->next     = ddroot;
    ddroot        = ddp;

    return ddp;
}

static DDLIST *
#ifndef ANSI_PROTOTYPES
util_get_dir(fpath)
    char *fpath;
#else /* ANSI_PROTOTYPES */
util_get_dir(char *fpath)
#endif /* ANSI_PROTOTYPES */
{
    DDLIST *ddp;

    ddp = util_get_cached_ddlist(fpath);

    if (!ddp)
	ddp = util_cache_contents(fpath);

    return ddp;
}

RDIR *
#ifndef ANSI_PROTOTYPES
util_opendir(path)
    char *path;
#else /* ANSI_PROTOTYPES */
util_opendir(char *path)
#endif /* ANSI_PROTOTYPES */
{
    char *fpath;
    DDLIST *ddp;
    RDIR *rdirp;

    fpath = util_abs_path(path);
    ddp = util_get_dir(fpath);
    (void)free(fpath);

    if (!ddp)
	return (RDIR*)0;

    ddp->ref_cnt++;

    rdirp = (RDIR*)malloc(sizeof(RDIR));
    rdirp->ddp = ddp;
    rdirp->dep = ddp->dep_root;

    return rdirp;
}

int
#ifndef ANSI_PROTOTYPES
util_closedir(rdirp)
    RDIR *rdirp;
#else /* ANSI_PROTOTYPES */
util_closedir(RDIR *rdirp)
#endif /* ANSI_PROTOTYPES */
{
    rdirp->ddp->ref_cnt--;
    (void)free(rdirp);

    return 0;
}

rdirent *
#ifndef ANSI_PROTOTYPES
util_readdir(rdirp)
    RDIR *rdirp;
#else /* ANSI_PROTOTYPES */
util_readdir(RDIR *rdirp)
#endif /* ANSI_PROTOTYPES */
{
    static rdirent rde;
    RDIRENT **dep;

    dep = rdirp->dep;

    if (!*dep)
	return (rdirent*)0;

    rde.rd_fileno = 10;
    rde.rd_reclen = 10;
    rde.rd_namlen = strlen((*dep)->name);
    rde.rd_name   = (*dep)->name;
    rdirp->dep    = dep+1;

    return &rde;
}

void
#ifndef ANSI_PROTOTYPES
util_split_path(path,p1,p2,p3)
    char *path, **p1, **p2, **p3;
#else /* ANSI_PROTOTYPES */
util_split_path(char *path, char **p1, char **p2, char **p3)
#endif /* ANSI_PROTOTYPES */
{
    char *s;
    static char junk;

    *p1 = "/";
    if (*path == '/')
    {
	*p2 = path;
	*p3 = path+1;
    }
    else
    {
    	*p2 = &junk;
	*p3 = path;
    }

    for (s = *p3; *s; s++)
    {
	if (*s == '/')
	{
	    *p1 = path;
	    *p2 = s;
	    *p3 = s+1;
	}
    }

    if (**p3 == '\0')
	*p3 = ".";
}

int
#ifndef ANSI_PROTOTYPES
util_stat(path,sbuf)
    const char *path;
    struct stat *sbuf;
#else /* ANSI_PROTOTYPES */
util_stat(const char *path, struct stat *sbuf)
#endif /* ANSI_PROTOTYPES */
{
    RDIR *drp;
    RDIRENT **dep;
    char *fpath, *ppath, *pfile;

    if (!validate_operation(path, UTIL_DIR))
    {
	errno = ENOENT;
	return -1;
    }

    fpath = util_abs_path(path);

    if (!strcmp(fpath,env_dir))
    {
	ppath = fpath;
	pfile = ".";
    }
    else
    {
	char *p1;
	util_split_path(fpath,&ppath,&p1,&pfile);
	*p1 = 0;
    }

    if ((drp = util_opendir(ppath)))
    {
#if 0
	/* future expansion stuff... */
        char *bmap;
        u_int bmap_len;

	if (util_pro(ppath, 0, &bmap, &bmaplen, 0, 0) < 0
	   || bmap == 0 || bmaplen == 0)
	{
	    /* set defaults */
	}
	else
	{
	    /* get file information from directory permissions */
	    if ((bmap[0] & DIR_OWNER))
		XXX;
	}
#endif

	for (dep = drp->dep; *dep; dep++)
	{
	    if (!strcmp((*dep)->name, pfile))
	    {
		if ((*dep)->type & RDTYPE_DIR)
		    sbuf->st_mode = 0777 | S_IFDIR;
		else
		    sbuf->st_mode = 0666 | S_IFREG;

		if ((*dep)->type & RDTYPE_DIR)
		    sbuf->st_nlink  = 2;
		else
		    sbuf->st_nlink  = 1;

		sbuf->st_uid    = 0;
		sbuf->st_gid    = 0;
		sbuf->st_size   = (*dep)->size;
		sbuf->st_atime  = (*dep)->time;
		sbuf->st_mtime  = (*dep)->time;
		sbuf->st_ctime  = (*dep)->time;

		(void)util_closedir(drp);
		(void)free(fpath);

		return 0;
	    }
	}
	(void)util_closedir(drp);
    }

    (void)free(fpath);
    errno = ENOENT;
    return(-1);
}

int
#ifndef ANSI_PROTOTYPES
util_cd(p)
    char *p;
#else /* ANSI_PROTOTYPES */
util_cd(char *p)
#endif /* ANSI_PROTOTYPES */
{
    char *fpath;
    DDLIST *ddp;

    fpath = util_abs_path(p);
    ddp = util_get_dir(fpath);

    if (!ddp)
    {
	(void)free(fpath);
	errno = EACCES;
	return -1;
    }

    (void)free(env_dir);
    env_dir = fpath;

    return 0;
}

/*****************************************************************************/

typedef struct PROLIST
{
	struct PROLIST	*next;
	char		*path;
	char		*text;
	char		*readme;
	u_int		readmecnt;
	u_int		bmaplen;
	char		*bmap;
	int		error;
} PROLIST;

static PROLIST *proroot = 0;

static void
#ifndef ANSI_PROTOTYPES
free_prolist(prop)
    PROLIST *prop;
#else /* ANSI_PROTOTYPES */
free_prolist(PROLIST *prop)
#endif /* ANSI_PROTOTYPES */
{
    if (prop == 0)
	return;

    if (prop->path)
	(void)free((char *)(prop->path));

    if (prop->readme || prop->text)
    {
	if (prop->readme && prop->readme <= prop->text)
	    (void)free((char *)(prop->readme - 1));
	else
	    (void)free((char *)(prop->text));
    }

    if (prop->bmap)
	(void)free((char *)(prop->bmap));

    (void)free((char *)prop);
}

void
#ifndef ANSI_PROTOTYPES
util_flushpro()
#else /* ANSI_PROTOTYPES */
util_flushpro(void)
#endif /* ANSI_PROTOTYPES */
{
    PROLIST *prop, *propnext;

    if (proroot == (PROLIST*)0)
	return;

    for (prop = proroot; prop; prop = propnext)
    {
	propnext = prop->next;
        free_prolist(prop);
    }

    proroot = (PROLIST*)0;
    oldserver = 0;
}

void
#ifndef ANSI_PROTOTYPES
util_dirtypro(path)
    char *path;
#else /* ANSI_PROTOTYPES */
util_dirtypro(char *path)
#endif /* ANSI_PROTOTYPES */
{
    PROLIST *prop, *proprev;
    char *fpath;

    if (proroot == (PROLIST*)0)
        return;

    fpath = util_abs_path(path);

    for (prop = proroot, proprev = 0; prop; proprev = prop, prop = prop->next)
        if (!strcmp(prop->path, fpath))
            break;

    (void)free(fpath);

    if (prop == (PROLIST *)0)
        return;

    if (proprev)
	proprev->next = prop->next;
    else
	proroot = prop->next;

    free_prolist(prop);
}

static PROLIST *
#ifndef ANSI_PROTOTYPES
util_get_cached_prolist(fpath)
    char *fpath;
#else /* ANSI_PROTOTYPES */
util_get_cached_prolist(char *fpath)
#endif /* ANSI_PROTOTYPES */
{
    PROLIST *prop;

    for (prop = proroot; prop; prop = prop->next)
	if (!strcmp(prop->path, fpath))
	    break;

    return prop;
}

/* it is assumed that util_get_cached_prolist(fpath) would return (PROLIST*)0 */
static PROLIST *
#ifndef ANSI_PROTOTYPES
util_cache_protection(fpath)
    char *fpath;
#else /* ANSI_PROTOTYPES */
util_cache_protection(char *fpath)
#endif /* ANSI_PROTOTYPES */
{
    PROLIST *prop;
    UBUF *ub;

    ub = client_interact(CC_GET_PRO, 0L, strlen(fpath), fpath + 1, 0, NULLP);
    if (client_intr_state > 1)
	return 0;

    prop = (PROLIST *)malloc(sizeof(PROLIST));

    prop->error  = (ub->cmd == CC_ERR);
    prop->text   = strdup(ub->buf);
    prop->readme = strchr(prop->text, '\n');
    /*
    ** this may be an old style readme -- if it is, then the first
    ** character will be '\n'; the free()'ing code will have to clever
    ** to make sure it free's the least of ->text and ->readme...
    */
    if (prop->readme == prop->text)
    {
	char *eor;
	/* old style */
	/*
	** the first character is '\n', so skip it; (remember this
	** when free'ing!).  Identify the end of the protection
	** readme (the last '\n' I hope), replace it with a '\0',
	** and change the ->text to the character after this.
	*/
	prop->readme++;
	eor = strrchr(prop->readme, '\n');
	if (eor)
	{
	    *eor = '\0';
	    prop->text = eor + 1;
	}
	else
	{
	    /* the protection started with a '\n' but didn't have a README */
	    prop->text = prop->readme;
	}
    }
    else
    {
	/* new style, or no readme */
	/*
	** if there is a readme, change the trailing protection newline
	** to a null and skip it and any following '\n's
	*/
	if (prop->readme)
	{
	    *(prop->readme++) = '\0';
	    while (*(prop->readme) == '\n')
		prop->readme++;
	}
    }

    /* set the count of times that this readme has been read to 0 */
    prop->readmecnt = 0;

    if (ub->pos == 0)
    {
    	prop->bmap = 0;
    	prop->bmaplen = 0;
	oldserver = 1;
    }
    else
    {
	int i;

	prop->bmaplen = ub->pos;

	if (prop->bmaplen > PRO_BYTES)
	    prop->bmaplen = PRO_BYTES;

	prop->bmap = (char *)malloc(prop->bmaplen);
	for (i = 0; i < prop->bmaplen; i++)
	    prop->bmap[i] = ub->buf[ub->len + i];

	/* invert the `PRIV' bit -- it is now the `public' bit */
	prop->bmap[0] ^= DIR_PRIV;
	oldserver = 0;
    }

    prop->path = strdup(fpath);
    prop->next = proroot;
    proroot    = prop;

    return prop;
}

int
#ifndef ANSI_PROTOTYPES
util_pro(path, textp, bmapp, bmaplenp, readmep, readmecntpp)
    char *path;
    char **textp;
    char **bmapp;
    u_int *bmaplenp;
    char **readmep;
    u_int **readmecntpp;
#else /* ANSI_PROTOTYPES */
util_pro(char *path, char **textp,
         char **bmapp, u_int *bmaplenp,
         char **readmep, u_int **readmecntpp)
#endif /* ANSI_PROTOTYPES */
{
    PROLIST *prop;
    char *fpath;

    fpath = util_abs_path(path);

    prop = util_get_cached_prolist(fpath);
    if (!prop)
	prop = util_cache_protection(fpath);
    else if (prop->error)
	ffprintf(STDERR, "?error: %s\n", prop->text);

    (void)free(fpath);

    if (textp)
	*textp = (prop && !prop->error)? prop->text: 0;

    if (bmapp)
	*bmapp = (prop && !prop->error)? prop->bmap: 0;

    if (bmaplenp)
	*bmaplenp = (prop && !prop->error)? prop->bmaplen: 0;

    if (readmep)
	*readmep = (prop && !prop->error && prop->readme != prop->text)?
							prop->readme: 0;

    if (readmecntpp)
	*readmecntpp = (prop && !prop->error)? &(prop->readmecnt): 0;

    return prop? -prop->error: -1;
}

/*****************************************************************************/

/*
** apply a function to a file.  apply `process_file' to every file found.
** call `process_dir' to each directory to determine if it is enterable,
** and if it is, enter it, decrementing `recursive', incrementing
** `depth' -- on exit from the directory, call `tidy_dir'.  this allows
** the depth of the search to be limited if the value of `recursive' is
** set positive.
*/
int
#ifndef ANSI_PROTOTYPES
util_process_file(path, recursive, depth, process_file, process_dir, tidy_dir)
    char *path;
    int recursive;
    int depth;
    int (*process_file)();
    int (*process_dir)();
    void (*tidy_dir)();
#else /* ANSI_PROTOTYPES */
util_process_file(char *path, int recursive, int depth,
		  int (*process_file)(char *, struct stat *, int),
		  int (*process_dir)(char *, int, char **),
		  void (*tidy_dir)(char *, int, char *))
#endif /* ANSI_PROTOTYPES */
{
    int retval = 0;
    struct stat sbuf;

    if (util_stat(path, &sbuf) < 0)
    {
	ffprintf(STDERR, "?remote file `%s': %s\n", path, sys_errlist[errno]);
	return -1;
    }

    path = util_abs_path(path);

    if (S_ISREG(sbuf.st_mode))
    {
	if (process_file && sincetime <= sbuf.st_mtime)
	    retval |= ((*process_file)(path, &sbuf, depth) < 0);
    }
    else if (S_ISDIR(sbuf.st_mode))
    {
	if (recursive)
	{
	    char *private_data = 0;

	    if (process_dir && (*process_dir)(path, depth, &private_data) < 0)
	    {
		retval = 1;
		ffprintf(STDERR, "?skipping remote directory `%s'\n", path);
	    }
	    else
	    {
		RDIR *rdir;

		if ((rdir = util_opendir(path)))
		{
		    struct rdirent *rde;
		    int pathlen;

		    pathlen = strlen(path);

		    while ((rde = util_readdir(rdir)))
		    {
			char *newname;

			/* skip over "." and ".." */
			if (rde->rd_name[0] == '.'
			   && (rde->rd_name[1] == '\0'
			      || (rde->rd_name[1] == '.'
				 && rde->rd_name[2] == '\0')))
			    continue;

			newname = (char *)malloc(pathlen + rde->rd_namlen + 2);

			(void)strcpy(newname, path);
			newname[pathlen] = '/';
			(void)strcpy(newname + pathlen + 1, rde->rd_name);

			retval |= -util_process_file(newname,
						     recursive - 1, depth + 1,
					             process_file,
						     process_dir, tidy_dir);

			(void)free(newname);
		    }

		    util_closedir(rdir);
		}
		else
		    retval = 1;

		if (tidy_dir)
		    (*tidy_dir)(path, depth, private_data);

		if (private_data)
		    (void)free(private_data);
	    }
	}
    }
    else
    {
	retval = 1;
	ffprintf(STDERR, "?remote file `%s' is not a file or directory!\n",
		 path);
    }

    (void)free(path);

    return -retval;
}

/*****************************************************************************/

int
#ifndef ANSI_PROTOTYPES
util_process_arglist(argv, procfn)
    char *argv[];
    int (*procfn)();
#else /* ANSI_PROTOTYPES */
util_process_arglist(char **argv, int (*procfn)(char *))
#endif /* ANSI_PROTOTYPES */
{
    int retval = 0;

    for ( ; *argv && client_intr_state < 2; argv++)
    {
	char **globdata, **files, *singlefile[2];

	/* this is necessary to allow cd's to directories which can't
	   be seen -- it would be much nicer if we could change this
	   to `continue'.  */
	if (!(globdata = files = glob(*argv)))
	{
	    files         = singlefile;
	    singlefile[0] = *argv;
	    singlefile[1] = 0;
	}

	for ( ; *files && client_intr_state < 2; files++)
	    retval |= ((*procfn)(*files) < 0);

	if (globdata)
	    free_glob(globdata);
    }

    if (client_intr_state > 1)
       retval = 1;

    return -retval;
}

/*****************************************************************************/

static void
#ifndef ANSI_PROTOTYPES
util_print_readmepro(text, cntp)
    char *text;
    u_int *cntp;
#else /* ANSI_PROTOTYPES */
util_print_readmepro(char *text, u_int *cntp)
#endif /* ANSI_PROTOTYPES */
{
    if (!text || !*text)
	ffprintf(STDERR, "?no README\n");
    else
    {
	ffprintf(STDOUT, "-- README:\n%s\n", text);
	(*cntp)++;
    }
}

void
#ifndef ANSI_PROTOTYPES
util_print_readme()
#else /* ANSI_PROTOTYPES */
util_print_readme(void)
#endif /* ANSI_PROTOTYPES */
{
    char *readme;
    u_int *readmecntp;

    if (util_pro(env_dir, 0, 0, 0, &readme, &readmecntp) < 0)
        ffprintf(STDERR, "?cannot access README\n");
    else
        util_print_readmepro(readme, readmecntp);
}

#define YESNO(v,m) (((v) & (m))? "yes": "no")
static void
#ifndef ANSI_PROTOTYPES
util_print_probmap(name, bmap, bmaplen)
    char *name;
    char *bmap;
    int bmaplen;
#else /* ANSI_PROTOTYPES */
util_print_probmap(char *name, char *bmap, int bmaplen)
#endif /* ANSI_PROTOTYPES */
{
    if (!bmap || bmaplen < 1)
	return;

    ffprintf(STDOUT, "-- directory `%s':\n", name);
    ffprintf(STDOUT, "\towner:  %s", YESNO(bmap[0], DIR_OWNER));
    ffprintf(STDOUT, "\t\tdelete: %s", YESNO(bmap[0], DIR_DEL));
    ffprintf(STDOUT, "\t\tadd:    %s\n", YESNO(bmap[0], DIR_ADD));
    ffprintf(STDOUT, "\tmkdir:  %s", YESNO(bmap[0], DIR_MKDIR));
    ffprintf(STDOUT, "\t\tread:   %s\n", YESNO(bmap[0], DIR_PRIV));
}

int
#ifndef ANSI_PROTOTYPES
util_print_protection(name)
    char *name;
#else /* ANSI_PROTOTYPES */
util_print_protection(char *name)
#endif /* ANSI_PROTOTYPES */
{
    char *text, *bmap, *readme;
    u_int bmaplen, *readmecntp;

    if (util_pro(name, &text, &bmap, &bmaplen, &readme, &readmecntp) < 0)
    {
        ffprintf(STDERR, "?cannot get protection of `%s'\n", name);
	return -1;
    }

    if (bmap)
	util_print_probmap(name, bmap, bmaplen);
    else
	ffprintf(STDOUT, "%s: %s\n", name, text);

    if (readme && *readme && (readme_max < 0 || *readmecntp < readme_max))
        util_print_readmepro(readme, readmecntp);

    return 0;
}

/*****************************************************************************/

static char *version = 0;
static char *verbmap = 0;
static u_int verbmaplen = 0;
static int version_error = 0;
static u_long thruput_limit = 0;

void
#ifndef ANSI_PROTOTYPES
util_dirty_version()
#else /* ANSI_PROTOTYPES */
util_dirty_version(void)
#endif /* ANSI_PROTOTYPES */
{
    if (version)
    {
	(void)free(version);
	version = 0;
    }

    if (verbmap)
    {
	(void)free(verbmap);
	verbmap = 0;
    }

    verbmaplen = 0;
    version_error = 0;
}

static int
#ifndef ANSI_PROTOTYPES
util_get_version()
#else /* ANSI_PROTOTYPES */
util_get_version(void)
#endif /* ANSI_PROTOTYPES */
{
    UBUF *ub;

    ub = client_interact(CC_VERSION, 0L, 0, NULLP, 0, NULLP);

    if (client_intr_state > 1 || !ub)
	return -1;

    util_dirty_version();

    version = strdup(ub->buf);
    version_error = (ub->cmd == CC_ERR);

    if (version_error)
	return -1;

    if (ub->pos > 0)
    {
	int i;

	verbmaplen = ub->pos;
	i = ub->len + 1;

	/*
	** if we have a thruput bit set, the 4 characters after the first byte
	** are the thruput rate.  Don't allocate space for them, and skip
	** them when copying the bitfield.
	*/
	if ((ub->buf[ub->len] & VER_THRUPUT))
	{
	     verbmaplen -= 4;

	     thruput_limit = (u_char)(ub->buf[i++]);
	     thruput_limit = (thruput_limit << 8) | (u_char)(ub->buf[i++]);
	     thruput_limit = (thruput_limit << 8) | (u_char)(ub->buf[i++]);
	     thruput_limit = (thruput_limit << 8) | (u_char)(ub->buf[i++]);
	}

        if (verbmaplen > VER_BYTES)
	    verbmaplen = VER_BYTES;

	verbmap = (char *)malloc(verbmaplen);

	verbmap[0] = ub->buf[ub->len];
	for (i -= ub->len; i < verbmaplen; i++)
	    verbmap[i] = ub->buf[ub->len + i];
    }

    return 0;
}

static void
#ifndef ANSI_PROTOTYPES
util_print_versionbmap()
#else /* ANSI_PROTOTYPES */
util_print_versionbmap(void)
#endif /* ANSI_PROTOTYPES */
{
    if (!verbmap || verbmaplen < 1)
	return;

    /* should only print out the first line of the version string */
    ffprintf(STDOUT, "%s", version);

    ffprintf(STDOUT, "\tlogging:         %s",
			YESNO(verbmap[0], VER_LOG));
    ffprintf(STDOUT, "\t\treadonly:        %s\n",
			YESNO(verbmap[0], VER_READONLY));
    ffprintf(STDOUT, "\treverse naming:  %s",
			YESNO(verbmap[0], VER_REVNAME));
    ffprintf(STDOUT, "\t\tprivate:         %s\n",
			YESNO(verbmap[0], VER_PRIVMODE));
    ffprintf(STDOUT, "\tthruput limited: %s",
			YESNO(verbmap[0], VER_THRUPUT));

    if ((verbmap[0] & VER_THRUPUT))
	ffprintf(STDOUT, "\t\tthruput value:   %d KB/sec\n", thruput_limit);
    else
	ffprintf(STDOUT, "\n");
}

int
#ifndef ANSI_PROTOTYPES
util_print_version()
#else /* ANSI_PROTOTYPES */
util_print_version(void)
#endif /* ANSI_PROTOTYPES */
{
    if (version_error)
    {
        ffprintf(STDERR, "?error: %s\n", version);
	return -1;
    }

    if (!version && util_get_version() < 0)
	return -1;

    if (!verbmap)
	ffprintf(STDOUT, "%s\n", version);
    else
	util_print_versionbmap();

    return 0;
}

/*****************************************************************************/
#define SERVER_WRITE_OP         (DIR_ADD | DIR_MKDIR)

int
#ifndef ANSI_PROTOTYPES
validate_operation(name, opmask)
    const char *name;
    u_long opmask;
#else /* ANSI_PROTOTYPES */
validate_operation(const char *name, u_long opmask)
#endif /* ANSI_PROTOTYPES */
{
    char *dirname, *eod, *bmap;
    u_int bmaplen;

    if (!dbug_flag && (opmask == 0 || standalone || oldserver))
	return 1;

    if (oldserver && dbug_flag)
	ffprintf(STDDBG,
"validate_operation_mask(): old server detected -- debug validate continue\n");

    dirname = util_abs_path(name);
    if ((opmask & LITERAL_DIR) == 0)
    {
	eod  = strrchr(dirname, '/');
	eod[eod == dirname] = '\0';
    }

    if (dbug_flag > 0)
    {
	ffprintf(STDDBG,
		 "validate_operation_mask():\n\tname = `%s';\n\tdirectory = `%s'\n",
		 name, dirname);
    }

    if (util_pro(dirname, 0, &bmap, &bmaplen, 0, 0) < 0)
    {
        if (client_intr_state < 2)
	    ffprintf(STDWARN, "?cannot get protection of `%s'\n", dirname);
	/* it probably means the directory doesn't exist, but... */
	/* if we can't get the protection, assume the operation can be done */
	(void)free(dirname);
	return 0;
    }

    (void)free(dirname);

    /* if this is an old server, then assume the operation is possible */
    if (bmap == 0 || bmaplen == 0)
    {
	if (dbug_flag > 0)
	    ffprintf(STDDBG, "\t*** OPERATION ALLOWED\n");
	return 1;
    }

    if (dbug_flag > 0)
    {
	ffprintf(STDDBG,
	"\topmask = %08x; bmap[0] = %02x; opmask & bmap[0] = %02x\n",
	opmask, bmap[0], opmask & bmap[0]);
    }

    if ((bmap[0] & DIR_OWNER) || (opmask & bmap[0]) == (opmask & 0xff))
    {
	int write_needed;

	write_needed = (opmask & SERVER_WRITE_OP) != 0;

	/*
	** if this operation doesn't require a non-readonly server, then
	**	allow the command;
	** or, if this command needs a non-readonly server (i.e., this command
	**	causes a modification to the remote end) then if we have the
	**	version string but not a bitmap, allow the command;
	** or, if this command needs a non-readonly server, the version string
	**	is null, and attempting to get the version causes either
	**	an error or returns no version bitmap, then allow the command.
	** otherwise, the command requires a non-readonly server, and we
	** have got the version bitmap, so continue.
	*/
	if (!write_needed
	   || (version && !verbmap)
           || (version == 0
	      && (util_get_version() < 0 || verbmap == 0)))
	{
	    if (dbug_flag > 0)
		ffprintf(STDDBG, "\t*** OPERATION ALLOWED\n");
	    return 1;
	}

	/* at this point: write_needed == 1 && verbmap != 0 */
	if  (dbug_flag > 0)
		ffprintf(STDDBG,
			 "\treadonly = %u; write operation mask = %08x\n",
			 (verbmap[0] & VER_READONLY) != 0,
			 opmask & (u_long)SERVER_WRITE_OP);

	if ((verbmap[0] & VER_READONLY) == 0)
	{
	    if (dbug_flag > 0)
		ffprintf(STDDBG, "\t*** OPERATION ALLOWED\n");
	    return 1;
	}
    }

    ffprintf(STDERR,
	"?operation not allowed on `%s': insufficient permissions\n", name);
    return 0;
}

/*****************************************************************************/
