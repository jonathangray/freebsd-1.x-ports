/* ups.c - initialisation and entry point for ups */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ups_c_sccsid[] = "@(#)ups.c	1.42 13/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <local/ukcprog.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/wn.h>
#include <local/menu3.h>
#include <local/arg.h>		/* for arg_version */
#include <local/obj/obj.h>	/* for obj_version */
#include <mtrprog/utils.h>	/* for mtrprog_version */

#include "ups.h"
#include "symtab.h"

#include "reg.h"
#include "ui.h"
#include "tdr.h"
#include "core.h"
#include "exec.h"
#include "obj_target.h"
#include "obj_buildf.h"
#include "obj_bpt.h"
#include "state.h"
#include "debug.h"

#define DEFAULT_TABWIDTH 8

static void catch_fatal_sig PROTO((int sig));
static void add_multiple_paths PROTO((const char *ro_paths, const char *defpath));
static int is_directory PROTO((const char *name));
static void get_default_paths PROTO((const char *textname,
				     const char **p_dsrc, const char **p_dcore));
static int check_keys PROTO((event_t *ev));
static void usage PROTO((const char *mesg));
static int setup_display PROTO((void));
static int ups PROTO((const char *textname, const char *corename,
		      const char *args, bool have_common_blocks,
		      bool replay, bool want_window));
int main PROTO((int argc, char **argv));
static const char *checkarg PROTO((char **argv));
static void set_special_key PROTO((char **argv, int *p_key));
static void handle_panic PROTO((const char *s));
static void null_ofunc PROTO((const char *s));

static int Ignore_key = -1, Stop_key = -1, Check_key = -1;

unsigned long Debug_flags = 0;

static bool Done_panic = FALSE;

static int
check_keys(ev)
event_t *ev;
{
	if (ev->ev_type == EV_KEY) {
		if (ev->ev_char == Ignore_key) {
			errf("\bignoring key");
			return TRUE;
		}
		if (ev->ev_char == Stop_key) {
			kill(getpid(), SIGSTOP);
			return TRUE;
		}
		if (ev->ev_char == Check_key) {
			td_set_default_obj_to_selection();
			return TRUE;
		}
	}
	return FALSE;
}

static void
handle_panic(s)
const char *s;
{
	Done_panic = TRUE;
}

/*  This routine is called when we get a signal that causes a core dump.
 *  We want to avoid overwriting an existing core file in the current
 *  directory because that may be from the target and thus precious to
 *  the user.
 *
 *  We restrict ourselves to system calls and simple string routines here
 *  because we are in an inconsistent state (we have just had a SEGV or
 *  the like!).
 */
static void
catch_fatal_sig(sig)
int sig;
{
	struct stat stbuf;
	static const char upscore[] = "ups-core";
#ifdef SIG_UNBLOCK
	sigset_t mask;
#endif
#define ERR(m)	write(2, (m), sizeof(m) - 1)

	if (!Done_panic) {
		const char *sigstr;

		ERR("Fatal error: ");

		/*  Convert the signal number to text ourself as sprintf,
		 *  strf etc are too complex to call from here.
		 */

		switch (sig) {
		case SIGSEGV:	sigstr = "segmentation fault";		break;
		case SIGBUS:	sigstr = "bus error";			break;
		case SIGILL:	sigstr = "illegal instruction";		break;
		case SIGFPE:	sigstr = "floating point exception";	break;
		case SIGQUIT:	sigstr = "SIGQUIT";			break;
		default:	sigstr = NULL;				break;
		}

		if (sigstr != NULL) {
			write(2, sigstr, strlen(sigstr));
		}
		else {
			char nbuf[20];
			char *np;
			int n;

			np = nbuf + sizeof(nbuf);
			*--np = '\0';

			n = (sig < 0) ? -sig : sig;
			for (; n != 0 && np > nbuf; n /= 10)
				*--np = n % 10 + '0';
			
			if (sig < 0)
				*--np = '-';

			ERR("Got signal ");
			write(2, np, sizeof(nbuf) - (np - nbuf));
		}

		ERR(".\n");
	}
	
	if (lstat("core", &stbuf) != 0 && errno == ENOENT) {
		ERR("Dumping core");
	}
	else if (chdir(upscore) == 0 ||
		 	(mkdir(upscore, 0755) == 0 && chdir(upscore) == 0)) {
		ERR("Dumping core in ups-core/core");
	}
	else if (chdir("/tmp") == 0) {
		ERR("Dumping core in /tmp/core");
	}
	else {
		ERR("Exiting without dumping core.\n");
		_exit(1);
	}
	ERR(" ... ");

#ifdef SIG_UNBLOCK
	sigemptyset(&mask);
	sigaddset(&mask, sig);
	sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
#else
	sigsetmask(0);
#endif
	signal(sig, SIG_DFL);
	kill(getpid(), sig);

	ERR("Unexpectedly survived signal - exiting without dumping core.\n");
	_exit(1);
}

/*  Treat paths as a colon seperated list of paths, and add each to
 *  the source path list.
 *
 *  If paths starts with a colon, the default path is added first.
 */
static void
add_multiple_paths(ro_paths, defpath)
const char *ro_paths, *defpath;
{
	char *last, *paths, *pathcopy;

	pathcopy = paths = strsave(ro_paths);
	if (*paths == ':')
		so_add_to_source_path(defpath);
	last = paths;
	for (;;) {
		while (*paths == ':' && *paths != '\0')
			paths++;
		if (*paths == '\0')
			break;
		last = paths;
		while (*paths != ':' && *paths != '\0')
			paths++;
		if (*paths == ':')
			*paths++ = '\0';
		so_add_to_source_path(last);
	}
	free(pathcopy);
}

static int
is_directory(name)
const char *name;
{
	struct stat stbuf;

	return stat(name, &stbuf) == 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR;
}

static void
get_default_paths(textname, p_dsrc, p_dcore)
const char *textname;
const char **p_dsrc, **p_dcore;
{
	const char *cptr;

	if ((cptr = strrchr(textname, '/')) == NULL) {
		*p_dsrc = ".";
#ifdef OS_BSDI
		*p_dcore = strf("core.%s", textname);
#else
		*p_dcore = "core";
#endif
	}
	else if (cptr == textname) {
		*p_dsrc = "/";
		*p_dcore = "/core";
	}
	else {
		*p_dsrc = strf("%.*s", cptr - textname, textname);
#ifdef OS_BSDI
		*p_dcore = strf("%s/core.%s", *p_dsrc, cptr + 1);
#else
		*p_dcore = strf("%s/core", *p_dsrc);
#endif
	}
}

static void
usage(mesg)
const char *mesg;
{
	if (mesg != NULL)
		fprintf(stderr, "%s\n", mesg);

	errf_usage(
	    "obj [-coredir dir] [corefile|pid] [srcdir[:srcdir]...] [-a args]");
}

static const char *
checkarg(argv)
char **argv;
{
	if (*argv == NULL) {
		usage(strf("The %s option requires an argument", argv[-1]));
		exit(1);
	}

	return *argv;
}

static void
set_special_key(argv, p_key)
char **argv;
int *p_key;
{
	const char *s;

	s = checkarg(argv);

	if (isalpha(*s) && s[1] == '\0')
		*p_key = CONTROL(*s);
	else
		usage(strf("The %s flag takes a single alphabetic character argument",
							argv[-1]));

	*p_key = CONTROL(*s);
}

int
main(argc, argv)
int argc;
char **argv;
{
	static const int siglist[] = {
		SIGQUIT, SIGILL, SIGIOT, SIGEMT, SIGFPE, SIGSEGV, SIGSYS, SIGBUS,
	};
	static const char *(*vfuncs[])PROTO((void)) = {
		arg_version, 
		obj_version,
		Mversion,
		mtrprog_version,
		ukcprog_version,
		wn_version,
		NULL
	};
	static char marker[] = "MARKER";
	static bool started = FALSE;
	const char *corename, *textname;
	const char *default_source_path, *default_corename;
	const char *args;
	char *save_arg;
	taddr_t data_addr;
	int i, text_fd;
	bool got_source_path, have_common_blocks, replay, want_window;
	extern char *_ups_sccsdata[];

	/*  Object if main is called recursively.  This can happen on a
	 *  VAX with a call via a NULL function pointer.
	 */
	if (started)
		panic("main called recursively");
	started = TRUE;

	errf_set_progname(*argv);

	if (argc == 2 && strcmp(argv[1], "-V") == 0) {
		puts(_ups_sccsdata[0]);
		puts("Library versions:");
		for (i = 0; vfuncs[i] != NULL; ++i)
			printf("\t%s\n", (*vfuncs[i])());
		exit(0);
	}

	if (argc == 2 && strcmp(argv[1], "-fullusage") == 0) {
		puts("\
Usage: ups -V\n\
   or: ups -fullusage\n\
   or: ups obj [-coredir dir] [corefile|pid] [srcdir[:srcdir]...] [-a args]\n\
               [-replay path] [-record path] [-nowindow]\n\
	       [-replaymode [lnum:]{verbose|wait|waitcheck|echo|noecho|quit}]\n\
               [-ignorekey c] [-stopkey c] [-checkkey c]\n\
	       [-use_srcpath_only] [-dbflags flags]");
		exit(0);
	}

	set_message_wn(-1);
	errf_set_ofunc(display_message);

	for (i = 0; i < sizeof(siglist) / sizeof(siglist[0]); i++)
		signal(siglist[i], catch_fatal_sig);
	install_panic_handler(handle_panic);
	
	/*  We have to set this here because we want to act on the
	 *  -nowindow flag on the first pass through the arguments.
	 *  This is because we must not call wn_munge_args() if we
	 *  have the -nowindow flag (it opens the display connection).
	 *  This means we end up checking the -nowindow twice, but it
	 *  doesn't break anything.
	 */
	want_window = TRUE;

	/*  Hide anything following a "-a" argument from wn_munge_args().
	 */
	save_arg = 0; /* to satisfy gcc */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-a") == 0 && argv[i + 1] != NULL) {
			save_arg = argv[i + 1];
			argv[i + 1] = marker;
			break;
		}
		else if (strcmp(argv[i], "-nowindow") == 0) {
			want_window = FALSE;
		}
	}

	if (want_window) {
		wn_set_classname("Ups");
		wn_munge_args(argc, (const char **)argv);
	}

	++argv;
	for (i = 0; argv[i] != NULL; i++)
		if (argv[i] == marker)
			argv[i] = save_arg;

	if (*argv == NULL)
		usage((const char *)NULL);
	
	if (**argv == '-')
		usage("Flags must come after the object file name");

	textname = *argv++;
	get_default_paths(textname, &default_source_path, &default_corename);
	if ((text_fd = open_textfile(textname, &data_addr)) == -1)
		exit(1);

	got_source_path = FALSE;
	args = NULL;
	corename = NULL;
	replay = FALSE;

	for (; *argv != NULL; argv++) {
		if (strcmp(*argv, "-use_srcpath_only") == 0) {
			set_use_srcpath_only_flag();
		}
		else if (strcmp(*argv, "-dbflags") == 0) {
			Debug_flags = strtol(checkarg(++argv), (char **)NULL,0);
		}
		else if (strcmp(*argv, "-nowindow") == 0) {
			want_window = FALSE;
		}
		else if (strcmp(*argv, "-replay") == 0) {
			if (td_replay_from(checkarg(++argv)) != 0)
				exit(1);
			replay = TRUE;
		}
		else if (strcmp(*argv, "-replaymode") == 0) {
			if (td_set_replay_mode(checkarg(++argv)) != 0)
				exit(1);
		}
		else if (strcmp(*argv, "-record") == 0) {
			if (td_record_to(checkarg(++argv)) != 0)
				exit(1);
		}
		else if (strcmp(*argv, "-checkkey") == 0) {
			set_special_key(++argv, &Check_key);
		}
		else if (strcmp(*argv, "-ignorekey") == 0) {
			set_special_key(++argv, &Ignore_key);
		}
		else if (strcmp(*argv, "-stopkey") == 0) {
			set_special_key(++argv, &Stop_key);
		}
		else if (strcmp(*argv, "-coredir") == 0) {
			default_corename = strf("%s/core", checkarg(++argv));
		}
		else if (strcmp(*argv, "-a") == 0) {
			if (args != NULL)
				usage("Multiple -a options given");
			args = checkarg(++argv);
		}
		else if (**argv == '-') {
			usage(strf("Unknown flag `%s'", *argv));
		}
		else if (strchr(*argv, ':') != NULL || is_directory(*argv)) {
			if (got_source_path)
				usage("Multiple source paths given");
			
			add_multiple_paths(*argv, default_source_path);
			got_source_path = TRUE;
		}
		else {
			if (corename != NULL) {
				usage(strf(
				  "Multiple core files specified (%s and %s)",
							     corename, *argv));
			}
			corename = *argv;
			if (open_corefile(corename, textname,
							TRUE, data_addr) != 0)
				exit(1);
		}
	}

	/*  Before we do any potentially time consuming stuff, check
	 *  that we are going to be able to fire up a window.
	 */
	if (want_window) {
		if (setup_display() != 0)
			exit(1);
	}
	else {
		if (!replay) {
			errf("You must give the -replay flag with -nowindow");
			exit(1);
		}
		td_set_no_window_flag();
	}

	if (!got_source_path)
		add_multiple_paths(":", default_source_path);

	if (corename == NULL)
		if (open_corefile(default_corename, textname,
							FALSE, data_addr) == 0)
			corename = default_corename;

	if (get_and_install_symtabs(textname, text_fd,
						&have_common_blocks) != 0)
		exit(1);

	if (ups(textname, corename, args,
				have_common_blocks, replay, want_window) != 0)
		exit(1);

	exit(0);
	return 0;	/* to satisfy gcc */
}

static int
setup_display()
{
	const char *errmesg;
	const char *tabwidth_str, *menufont_name;
	font_t *menufont;
	int tabwidth;

	if ((errmesg = wn_open_display()) != NULL) {
		errf("%s", errmesg);
		return -1;
	}

	if ((tabwidth_str = wn_get_default("TabWidth")) == NULL)
		tabwidth = DEFAULT_TABWIDTH;
	else {
		tabwidth = atoi(tabwidth_str);
		if (tabwidth < 1) {
			errf("Illegal TabWidth resource value `%s' ignored",
								tabwidth_str);
			tabwidth = DEFAULT_TABWIDTH;
		}
	}
	set_tabwidth(tabwidth);
	so_set_default_tabwidth(tabwidth);

	menufont = NULL;
	if ((menufont_name = wn_get_default("MenuFont")) != NULL) {
		menufont = wn_open_font(menufont_name);
		if (menufont == NULL) {
			errf("Can't open font `%s' specified by the MenuFont resource - using the system font",
								menufont_name);
		}
	}
	Msetstdfont((menufont != NULL) ? menufont : wn_get_sysfont());

	if (Ignore_key != -1 || Stop_key != -1 || Check_key != -1)
		wn_interpose_event_handler(check_keys);
	
	return 0;
}

static void
null_ofunc(s)
const char *s;
{
}

static int
ups(textname, corename, args, have_common_blocks, replay, want_window)
const char *textname, *corename, *args;
bool have_common_blocks;
bool replay, want_window;
{
	region_id_t root_region_id;
	window_t wn;
	errf_ofunc_t oldf;
	func_t *f;
	int res;

	oldf = errf_set_ofunc(null_ofunc);
	res = find_func_by_name("MAIN", &f);
	errf_set_ofunc(oldf);

	if (res != 0 || f->fu_language != LANG_FORTRAN) {
		if (find_func_by_name("main", &f) != 0)
			return -1;
	}

	set_main_func(f);

	root_region_id = NULL;	/* to satisfy gcc */
	wn = -1;		/* to satisfy gcc */

	if (want_window) {
		wn_npixels_hint(1);
		if ((wn = wn_open_stdwin()) == -1) {
			errf("Can't open window");
			return -1;
		}
	}

	do_formats(want_window);

	/*  We don't want to get stopped by controlling tty related
	 *  signals sent to the process group of the child process
	 *  (which includes us).  We can't switch process groups
	 *  because we want to respond to SIGTSTP (i.e. ^Z should
	 *  stop ups).  We don't want to background ourself because
	 *  then the child process would have no controlling tty.
	 */
	signal(SIGTTIN, SIG_IGN);
	signal(SIGTTOU, SIG_IGN);
	signal(SIGINT, SIG_IGN);

	if (want_window)
		root_region_id = divide_window_into_regions(wn);

	initialise_display_area(textname, corename, args, have_common_blocks);

	if (want_window)
		update_target_menu_state();

	td_set_obj_updating(OBJ_UPDATING_ON);

	if (get_target_state() == TS_NOTR && f->fu_fil != NULL &&
							FU_LNOS(f) != NULL)
		show_source(f->fu_fil, FU_LNOS(f)->ln_num, TRUE);
	
	td_set_displayed_source((fil_t *)NULL, 0, (const char *)NULL);
	
	if (replay) {
		bool eof;

		res = td_event_loop(&eof);

		if (want_window && (res != 0 || eof)) {
			fprintf(stderr, "%s in replay file - %s",
					(res == 0) ? "EOF" : "Error",
					"switching to normal events\n");
			re_event_loop(root_region_id);
		}
	}
	else {
		re_event_loop(root_region_id);
		res = 0;
	}

	/*  make sure the target is dead or detached before exiting.
	 */
	if (target_process_exists())
		kill_or_detach_from_target();

	return res;
}
