    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *  Copyright (c) 1993 by Phil Richards (pgr@prg.ox.ac.uk)             *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "common.h"
#include "local/local.h"
#include "remote/remote.h"

#ifndef MINDELAY
# define MINDELAY 1000
#endif

#ifndef MAXRECURSION
# define MAXRECURSION 512
#endif

#if defined(__STDC__) && __GNUC__ == 2
#define PRINTF_ATTRIB(x,y) __attribute__ ((format (printf, x, y)))
#else
#define PRINTF_ATTRIB(x,y)
#endif


/*
** a heuristic approach...
**  flock() is only going to work on a local system -- however, that is
**  precisely what we want
**  then we try lockf() and shared memory (though I have no idea why...)
**  and finally lockf() by itself
**  if none of these work, give up and use `NOLOCKING'
*/

#if HAVE_FLOCK
#  define USE_FLOCK
#else
#  if HAVE_LOCKF
#    if HAVE_SHMGET
#      define USE_SHAREMEM_AND_LOCKF
#    else
#      define USE_LOCKF
#    endif
#  else
#    define NOLOCKING
#  endif
#endif

/****************************************************************************
* These structures are used to implement a opendir/readdir mechanism similar
* to that of the normal opendir/reader mechanism in unix.
****************************************************************************/

typedef struct DDLIST {
	struct DDLIST *next;
	char          *path;
	RDIRENT  **dep_root;
	u_int       ref_cnt;
} DDLIST;

typedef struct RDIR {
	DDLIST   *ddp;
        RDIRENT **dep;
} RDIR;

typedef struct rdirent {
	u_long  rd_fileno;
	u_short rd_reclen;
	u_short rd_namlen;
	char     *rd_name;
} rdirent;

typedef struct iobuffers {
	FILE	*in;	 /* stream to read all command input from        */
	FILE	*out;	 /* stream to send all command output to         */
	FILE	*err;	 /* stream to send all command errors to         */
	FILE	*prompt; /* stream to send all prompt messages to        */
	FILE	*info;	 /* stream to send all informational messages to */
	FILE	*warn;	 /* stream to send all warning messages to       */
	FILE	*dbg;	 /* stream to send all debug messages to         */
} iobuffers;

/* externs from lib.c */
#ifndef ANSI_PROTOTYPES
extern UBUF *client_interact();
extern void print_comm_stats();
extern int init_client();
extern void finish_client();
extern void client_done();
#else /* ANSI_PROTOTYPES */
extern UBUF *client_interact(u_int cmd, u_long pos,
			     u_int l1, char *p1,
			     u_int l2, char *p2);
extern void print_comm_stats(FILE *out);
extern int init_client(char *host, int port, int myport);
extern void finish_client(void);
extern void client_done(void);
#endif /* ANSI_PROTOTYPES */

extern int client_trace;
extern int client_intr_state;
extern int client_intr_cnt;
extern int key_persists;
extern u_long target_delay;
extern u_long udp_sent_time;
extern u_int time_out;
extern int burst_max;
extern u_short client_buf_len;
extern u_short client_net_len;

/* externs from util.c */
extern char *env_host;
extern char *env_port;
extern char *env_myport;
extern char *env_dir;

#ifndef __STDC__
extern void ffprintf();

#else /* __STDC__ */

extern void ffprintf(FILE *out, const char *fmt, ...) PRINTF_ATTRIB(2,3);
#endif /* __STDC__ */

#ifndef ANSI_PROTOTYPES
extern char *my_fgets();
extern char *util_abs_path();
extern char *util_getwd();
extern RDIRENT **get_dir_blk();
extern int util_download_main();
extern int util_download();
extern int util_upload();
extern int util_grab_file();
extern void util_get_env();
extern void env_client();
extern void util_flushdir();
extern void util_dirtydir();
extern RDIR *util_opendir();
extern int util_closedir();
extern rdirent *util_readdir();
extern void util_split_path();
extern int util_stat();
extern int util_cd();
extern void util_flushpro();
extern void util_dirtypro();
extern int util_both_pro();
extern int util_process_file();
extern int util_process_arglist();
extern int util_print_protection();
extern void util_dirty_version();
extern int util_print_version();
extern void util_print_readme();

#else /* ANSI_PROTOTYPES */

extern char *my_fgets(char *s, int n, FILE *stream);
extern char *util_abs_path(const char *s2);
extern char *util_getwd(char *p);
extern RDIRENT **get_dir_blk(char *path);
extern int util_download_main(char *path, char *fpath, int fd,
			      int cmd, u_long offset, u_long length);
extern int util_download(char *path, int fd, u_long offset, u_long length);
extern int util_upload(char *path, FILE *fp);
extern int util_grab_file(char *path, int fd, u_long offset, u_long length);
extern void util_get_env(void);
extern void env_client(void);

extern void util_flushdir(void);
extern void util_dirtydir(char *path);
extern RDIR *util_opendir(char *path);
extern int util_closedir(RDIR *rdirp);
extern rdirent *util_readdir(RDIR *rdirp);
extern void util_split_path(char *path, char **p1, char **p2, char **p3);
extern int util_stat(const char *path, struct stat *sbuf);
extern int util_cd(char *p);

extern void util_flushpro(void);
extern void util_dirtypro(char *path);
extern int util_both_pro(char *path, char **textp,
			 char **bmapp, u_int *bmaplen);

extern int util_process_file(char *path, int recursive, int depth,
	 int (*process_file)(char *, struct stat *, int),
         int (*process_dir)(char *, int, char **),
         void (*tidy_dir)(char *, int, char *));
extern int util_process_arglist(char **argv, int (*procfn)(char *name));
extern int util_print_protection(char *name);

extern void util_dirty_version(void);
extern int util_print_version(void);

extern void util_print_readme(void);
#endif /* ANSI_PROTOTYPES */

/*
** kludge to allow validate_operation to act upon the filename specified
** rather than the basename of the file; this is only really used by rmdir
*/
#define LITERAL_DIR             0x80000000
#ifndef ANSI_PROTOTYPES
extern int validate_operation();
#else /* ANSI_PROTOTYPES */
extern int validate_operation(const char *name, u_long opmask);
#endif /* ANSI_PROTOTYPES */

#define UTIL_PRINT_PROTECTION	(0)
#define UTIL_PRINT_VERSION	(0)
#define UTIL_CD			(DIR_PRIV)
#define UTIL_DIR		(DIR_PRIV)	/* opendir/readdir/closedir */
#define UTIL_DOWNLOAD		(DIR_PRIV)
#define UTIL_STAT		(DIR_PRIV)
#define UTIL_UPLOAD		(DIR_ADD)
#define UTIL_PROCESS_FILE	(UTIL_DIR | UTIL_STAT)
#define UTIL_GLOB		(UTIL_DIR | UTIL_STAT)
#define UTIL_GRAB_FILE		(UTIL_DOWNLOAD | DIR_DEL)
#define UTIL_PROCESS_ARGLIST	(UTIL_GLOB)

/* externs from redirect.c */
extern int askprompt;
extern int datestamp;

extern iobuffers global_iobuffers;

#define STDIN		(global_iobuffers.in)
#define STDOUT		(global_iobuffers.out)
#define STDERR		(global_iobuffers.err)
#define STDPROMPT	(global_iobuffers.prompt)
#define STDINFO		(global_iobuffers.info)
#define STDWARN		(global_iobuffers.warn)
#define STDDBG		(global_iobuffers.dbg)

extern int dbug_flag;
extern char *pager_command;

/* externs from glob.c */
#ifdef __STDC__
typedef void VOIDDIR;
typedef void VOIDDIRENT;
#else
typedef char VOIDDIR;
typedef char VOIDDIRENT;
#endif

#ifndef ANSI_PROTOTYPES
extern int glob_match();
extern int glob_path();
extern char *expand_tilde();
extern char **glob();
extern void free_glob();
extern void set_glob_routines();
void local_glob_routines();
void remote_glob_routines();

#else /* ANSI_PROTOTYPES */

extern int glob_match(char *pattern, char *string);
extern int glob_path(char *pattern, char ***names);
extern char *expand_tilde(char *path);
extern char **glob(char *path);
extern void free_glob(char **argv);
extern void set_glob_routines(VOIDDIR *(*dopen)(char *dirname),
			      void (*dclose)(VOIDDIR *dirp),
			      VOIDDIRENT *(*dread)(VOIDDIR *dirp),
			      char *(*dgetname)(VOIDDIRENT *dp),
			      int (*dstat)(const char *buf, struct stat *s));
void local_glob_routines(void);
void remote_glob_routines(void);
#endif /* ANSI_PROTOTYPES */

extern int standalone;
