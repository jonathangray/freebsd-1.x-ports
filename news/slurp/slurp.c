/*
 * slurp - a passive nntp news client
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * This is the main routine for slurp together with the routines to
 * handle the configuration files and command line arguments.
 *
 *
 * $Id: slurp.c,v 1.1 1993/08/27 02:47:47 alm Exp $
 *
 * $Log: slurp.c,v $
 * Revision 1.1  1993/08/27 02:47:47  alm
 * Initial revision
 *
 * Revision 1.8  1993/08/20  10:34:50  root
 * Unlink backup time file before renaming or an error occurs
 * with the rename under SVR3.
 *
 * Revision 1.7  1993/06/07  11:15:00  root
 * Added support for users to supply a filename for the time file
 * on the command line, for use with machines with short filenames.
 * Rewrote read_sys (again!) for a much cleaner implementation.
 * Fixed problem of time file being wrongly set if slurp was
 * interrupted before the newnews phase had completed.
 * If can't rename time file then don't abort, just write the new
 * file.
 *
 * Revision 1.6  1993/04/22  18:22:20  root
 * Added signal handler to catch SIGHUP, SIGINT, SIGQUIT and SIGTERM.
 * If occur then report signal in syslog and possibly submit open
 * batch to news and dump message ids of uncollected articles to
 * slurp.<hostname>
 *
 * Revision 1.5  1993/03/01  17:51:33  root
 * read_sys can cope with lines longer than BUFSIZ.
 * report when attempting to load unretrieved message ids.
 * Move sublist to a parameter after the hostname, separate by a slash.
 * Changed some system error checking.
 *
 * Revision 1.4  1993/02/14  14:57:43  root
 * Added support for simple authorisation protocol.
 * Added support for INN's 'MODE READER' command.
 * Re-arranged command line options.
 * Rewrote read_sys and added flags and authorisation options to it.
 * Rewrote get_ntime and set_ntime to use a filename of slurp.<hostname>
 * instead of slurp.tim, solving lack of locking and allowing the file
 * to contain a list of unretrieved message ids on the lines following
 * the time.
 * Don't care if slurp.<hostname> doesn't exist already.
 * If RNEWS is not defined, then change to INDIR for writing out batch
 * files.
 *
 * Revision 1.3  1992/12/15
 * Open syslog *before* we start doing things that might write to it.
 * Informational messages logged as LOG_INFO.
 * Assorted minor tidy-ups.
 *
 * Revision 1.2  1992/12/07
 * Corrected test for 4.2/4.3 BSD syslog open.
 *
 * Revision 1.1  1992/12/06
 * Made no_time_flag global.
 * Fixed null dereferencing of nn_distributions.
 *
 * Revision 1.0  1992/08/07
 * Initial coding.
 *
 */

#include "slurp.h"
#include <signal.h>

char *hostname = NULL;
char *pname;

int debug_flag = FALSE;
int no_time_flag = FALSE;
int no_id_load_flag = FALSE;
static int local_time_flag = FALSE;
static int mode_reader_flag = FALSE;

int  dupart = 0;
int  misart = 0;
int  newart = 0;
long totalsize = 0;

char *nn_newsgroups    = NULL;
char *nn_time          = NULL;
char *nn_distributions = NULL;

static char *ai_username = NULL;
static char *ai_password = NULL;

static char *sublist = NULL;
static char *timefile = NULL;

struct mnode *root = NULL;
int entries = 0;

static long newdate, newtime;


/*
 * test_time - Check NEWNEWS time string is in the right format (ish)
 */

	static int
test_time ()
	{
	return (!(isdigit (nn_time [0]) &&
	          isdigit (nn_time [1]) &&
	          isdigit (nn_time [2]) &&
	          isdigit (nn_time [3]) &&
	          isdigit (nn_time [4]) &&
	          isdigit (nn_time [5]) &&
	          isspace (nn_time [6]) &&
	          isdigit (nn_time [7]) &&
	          isdigit (nn_time [8]) &&
	          isdigit (nn_time [9]) &&
	          isdigit (nn_time [10]) &&
	          isdigit (nn_time [11]) &&
	          isdigit (nn_time [12])));
	}


/*
 * parse_args - Parse the command line arguments. Returns 1 if there is
 * an error, otherwise returns 0.
 */

	static int
parse_args (int argc, char **argv)
	{
	int c;
	extern int optind;
	extern char *optarg;
	char *pos;
	
	while ((c = getopt (argc, argv, "a:g:t:dilrw")) != EOF)
		switch (c)
			{
			case 'a':	/* Do an authinfo */
				if (pos = strchr (optarg, '/'))
					{
					ai_username = optarg;
					*pos++ = '\0';
					ai_password = pos;
					}
				else
					{
					(void) fprintf (stderr, "Invalid authinfo username/password");
					return (1);
					}
				break;
			case 'g':	/* Newsgroups list */
				if (pos = strchr (optarg, '/'))
					{
					*pos++ = '\0';
					nn_distributions = pos;
					}
				else
					nn_distributions = "";
				nn_newsgroups = optarg;
				no_time_flag++;
				break;
			case 't':	/* Start time */
				nn_time = optarg;
				break;
			case 'd':	/* Debugging on */
				debug_flag++;
				break;
			case 'i':	/* Don't load unprocessed ids */
				no_id_load_flag++;
				break;
			case 'l':	/* Use local time */
				local_time_flag++;
				break;
			case 'r':	/* Do a 'MODE READER' */
				mode_reader_flag++;
				break;
			case 'w':	/* Don't set next time */
				no_time_flag++;
				break;
			default:
				return (1);
			}

	/* Get server name */
	if (optind < argc)
		{
		hostname = argv [optind];
		if (pos = strchr (hostname, ':'))
			{
			*pos++ = '\0';
			timefile = pos;
			}
		if (pos = strchr (hostname, '/'))
			{
			*pos++ = '\0';
			sublist = pos;
			}
		}
	else
		{
		(void) fprintf (stderr, "No server name supplied\n");
		return (1);
		}

	/* If groups are specified, then must have a time */
	if ((nn_newsgroups != NULL) && (nn_time == NULL))
		{
		(void) fprintf (stderr, "Time must be specified for -g option\n");
		return (1);
		}

	/* Verify that the time is in something like the right format */
	if (nn_time)
		if (test_time ())
			{
			(void) fprintf (stderr, "Invalid time specification - should be 'YYMMDD HHMMSS'\n");
			return (1);
			}

	return (0);
	}


/*
 * read_sys_line - Read a line from the slurp.sys file, skipping lines
 * which are blank or all comments, truncating lines at comments.
 * If the line has not yet all been read or the continued-on-next-line
 * token '\' is present, then returns 1; if eof then returns -1, otherwise
 * returns 0.
 */

	static int
read_sys_line (char *line, int size, FILE *sysfp)
	{
	int status;
	char *pos;

	for (;;)
		{
		status = 0;

		(void) fgets (line, size, sysfp);
		if (feof (sysfp))
			return (-1);
		if (ferror (sysfp))
			log_sys ("read_sys_line: Error reading %s", SYSFILE);

		if (pos = strchr (line, '\n'))
			*pos = '\0';
		else
			status = 1;

		if (pos = strchr (line, '\\'))
			{
			*pos = '\0';
			status = 1;
			}

		if (pos = strchr (line, '#'))
			*pos = '\0';

		if (strlen (line))
			return (status);
		}
	}


/*
 * read_sys - Read in the appropriate entry from the slurp.sys file
 * for the specified hostname. Stores the relevant newsgroups for that
 * host in nn_newsgroups and the relevant distribution in nn_distributions.
 * Returns 0 if an appropriate entry for the current host is found, 
 * otherwise returns 1.
 */

	static int
read_sys ()
	{
	FILE *sysfp;
	char buf [BUFSIZ];
	char searchname [BUFSIZ];
	size_t tlen;
	char *pos;
	int status;
	int object;

	/* Attempt to open the sys file */
	if ((sysfp = fopen (SYSFILE, "r")) == NULL)
		log_sys ("read_sys: Error opening %s", SYSFILE);

	/* Create pattern to search for in the sys file */
	(void) strcpy (searchname, hostname);
	if (sublist)
		{
		(void) strcat (searchname, "/");
		(void) strcat (searchname, sublist);
		}
	(void) strcat (searchname, ":");
	tlen = strlen (searchname);

	/* Read in file until we find hostname */
	for (;;)
		{
		if ((status = read_sys_line (buf, sizeof (buf), sysfp)) == -1)
			{
			log_msg ("read_sys: Host %s not found in %s",
					 hostname, SYSFILE);
			return (1);
			}
		if (strncmp (buf, searchname, tlen) == 0)
			break;
		}

	/* Strip off hostname stuff from front of line */
	(void) strcpy (buf, buf + tlen);

	/* Start with the newsgroups list */
	object = 1;

	/* Loop through entry */
	for (;;)
		{
		/* Currently adding newsgroups */
		if (object == 1)
			{
			if (pos = strchr (buf, '/'))		/* Distributions next */
				{
				*pos++ = '\0';
				nn_newsgroups = stradd (nn_newsgroups, buf);
				(void) strcpy (buf, pos);
				object = 2;
				}
			else if (pos = strchr (buf, ':'))	/* Flags next */
				{
				*pos++ = '\0';
				nn_newsgroups = stradd (nn_newsgroups, buf);
				(void) strcpy (buf, pos);
				object = 3;
				}
			else								/* Nothing else this line */
				nn_newsgroups = stradd (nn_newsgroups, buf);
			}

		/* Currently adding distributions */
		if (object == 2)
			{
			if (pos = strchr (buf, ':'))		/* Flags next */
				{
				*pos++ = '\0';
				nn_distributions = stradd (nn_distributions, buf);
				(void) strcpy (buf, pos);
				object = 3;
				}
			else								/* Nothing else this line */
				nn_distributions = stradd (nn_distributions, buf);
			}

		/* Currently setting flags */
		if (object == 3)
			{
			if (pos = strchr (buf, ':'))		/* authinfo user next */
				*pos++ = '\0';
			if (strchr (buf, 'i'))
				no_id_load_flag++;
			if (strchr (buf, 'l'))
				local_time_flag++;
			if (strchr (buf, 'r'))
				mode_reader_flag++;
			if (pos)
				{
				(void) strcpy (buf, pos);
				object = 4;
				}
			}

		/* Currently setting username */
		if (object == 4)
			{
			if (pos = strchr (buf, '/'))		/* authinfo pass next */
				{
				*pos++ = '\0';
				ai_username = stradd (ai_username, buf);
				(void) strcpy (buf, pos);
				object = 5;
				}
			else
				{
				ai_username = stradd (ai_username, buf);
				break;
				}
			}

		/* Currently setting password */
		if (object == 5)
			{
			ai_password = stradd (ai_password, buf);
			}

		if (status != 1)
			break;

		status = read_sys_line (buf, sizeof (buf), sysfp);
		}

	(void) fclose (sysfp);

	if (nn_distributions == NULL)
		nn_distributions = "";

	return (0);
	}


/*
 * get_ntime - Get the start time for this NEWNEWS for system. Returns 0
 * if an appropriate entry for the current host is found, otherwise 1.
 */

	static int
get_ntime ()
	{
	FILE *timefp;
	char buf [BUFSIZ];
	char filename [PATH_MAX];
	char *pos;

	/* Attempt to open the time file */
	(void) strcpy (filename, TIMFILE);
	if (timefile == NULL)
		(void) strcat (filename, hostname);
	else
		(void) strcat (filename, timefile);
	if (sublist)
		{
		(void) strcat (filename, ".");
		(void) strcat (filename, sublist);
		}
	if ((timefp = fopen (filename, "r")) == NULL)
		log_sys ("get_ntime: error opening %s", filename);

	/* Read in the time and store it */
	if ((nn_time = (char *) malloc ((size_t) 14)) == NULL)
		log_sys ("get_ntime: malloc 14 bytes");
	(void) fgets (nn_time, (size_t) 14, timefp);
	if (ferror (timefp))
		log_sys ("get_ntime: Error reading %s", filename);
	
	/* Return if time doesn't look ok */
	if (test_time ())
		return (1);

	/* Load in any message ids following */
	if (!no_id_load_flag)
		{
		if (debug_flag)
			(void) fprintf (stderr, "Loading any unretrieved message IDs\n");
		for (;;)
			{
			(void) fgets (buf, sizeof (buf), timefp);
			if (feof (timefp))
				break;
			if (ferror (timefp))
				log_sys ("get_ntime: Error reading %s", filename);
			if (pos = strchr (buf, '\n'))
				*pos = '\0';
			if (strlen (buf))
				if ((buf [0] == '<') && (buf [strlen (buf) - 1] == '>'))
					process_id (buf);
			}
		}

	(void) fclose (timefp);
	return (0);
	}


/*
 * write_tree - Traverse the tree writing out ids of articles that were
 * not successfully retrieved.
 */

	static void
write_tree (struct mnode *p, FILE *timefp)
	{
	if (p != NULL)	
    	{
		write_tree (p->left, timefp);
		if (!p->used)
			{
			(void) fprintf (timefp, "%s\n", p->msgid);
			if (ferror (timefp))
				log_sys ("write_tree: Error writing ids");
			}
		write_tree (p->right, timefp);
    	}
	}


/*
 * set_ntime - Set the start time for the next NEWNEWS for system
 */

	void
set_ntime ()
	{
	FILE *timefp;
	char filename [PATH_MAX];
	char backup [PATH_MAX];

	/* Copy the file to a backup */
	(void) strcpy (filename, TIMFILE);
	if (timefile == NULL)
		(void) strcat (filename, hostname);
	else
		(void) strcat (filename, timefile);
	if (sublist)
		{
		(void) strcat (filename, ".");
		(void) strcat (filename, sublist);
		}
	(void) strcpy (backup, filename);
	(void) strcat (backup, ".o");
	if (unlink (backup))
		if (errno != ENOENT)
			log_ret ("set_ntime: Error unlinking %s", backup);
	if (rename (filename, backup))
		if (errno != ENOENT)
			log_ret ("set_ntime: Error renaming %s to %s", filename, backup);

	/* Open new file */
	if ((timefp = fopen (filename, "w")) == NULL)
		{
		log_ret ("get_ntime: Error opening %s", filename);
		exit (1);
		}

	/* Write the new time for current host */
	(void) fprintf (timefp, "%06ld %06ld\n", newdate, newtime);
	if (ferror (timefp))
		{
		log_ret ("set_ntime: Error writing %s", filename);
		exit (1);
		}

	/* Write out any message ids not read in */
	write_tree (root, timefp);

	(void) fclose (timefp);
	}


/*
 * do_authinfo - Check in the authinfo username and password with the
 * server.
 */

	static void
do_authinfo ()
	{
	char buf [NNTP_STRLEN];

	/* Send the username to the server */
	(void) sprintf (buf, "AUTHINFO USER %s", ai_username);
	if (debug_flag)
		(void) fprintf (stderr, "<- %s\n", buf);
	put_server (buf);

	/* Get the response and check it's okay */
	get_server (buf, sizeof (buf));
	if (debug_flag)
		(void) fprintf (stderr, "-> %s\n", buf);
	if (atoi (buf) != NEED_AUTHDATA)
		{
		log_msg ("do_authinfo: NNTP protocol error: got '%s'", buf);
		exit (4);
		}
	                
	/* Send the password to the server */
	(void) sprintf (buf, "AUTHINFO PASS %s", ai_password);
	if (debug_flag)
		(void) fprintf (stderr, "<- %s\n", buf);
	put_server (buf);

	/* Get the response and check it's okay */
	get_server (buf, sizeof (buf));
	if (debug_flag)
		(void) fprintf (stderr, "-> %s\n", buf);
	if (atoi (buf) != OK_AUTH)
		{
		log_msg ("do_authinfo: NNTP protocol error: got '%s'", buf);
		exit (4);
		}
	}


/*
 * do_mode_reader - Send mode reader command to INN to switch to nnrpd
 * so we can do a NEWNEWS.
 */

	static void
do_mode_reader ()
	{
	char buf [NNTP_STRLEN];

	/* Send the command to the server */
	if (debug_flag)
		(void) fprintf (stderr, "<- MODE reader\n");
	put_server ("MODE READER");

	/* Get the response and check it's okay */
	get_server (buf, sizeof (buf));
	if (debug_flag)
		(void) fprintf (stderr, "-> %s\n", buf);
	switch (atoi (buf))
		{
		case OK_CANPOST :
		case OK_NOPOST :
			break;
		default :
			log_msg ("do_authinfo: NNTP protocol error: got '%s'", buf);
			exit (4);
		}
	}


/*
 * interrupt - signal handler to report signal in log and possibly
 * submit remaining batch to news and dump uncollected message ids.
 */

	static void
interrupt (int signo)
	{
	log_msg ("interrupt: received signal %d", signo);

	enqueue_batch ();
	if ((!no_time_flag) && (!no_id_load_flag))
		set_ntime ();
	exit (1);
	}


/*
 * set_signals - set up signal handler to catch appropriate signals.
 */

	static void
set_signals ()
	{
	if (signal (SIGHUP, interrupt) == SIG_ERR)
		log_sys ("set_signals: can't catch SIGHUP");
	if (signal (SIGINT, interrupt) == SIG_ERR)
		log_sys ("set_signals: can't catch SIGINT");
	if (signal (SIGQUIT, interrupt) == SIG_ERR)
		log_sys ("set_signals: can't catch SIGQUIT");
	if (signal (SIGTERM, interrupt) == SIG_ERR)
		log_sys ("set_signals: can't catch SIGTERM");
	}


/*
 * MAIN PROCEDURE
 */

	int
main (int argc, char **argv)
	{
    int ret;
	time_t clock, starttime, endtime;
	struct tm *now;

	/* Set the name of the program and parse the args */
	pname = (pname = (char *) strrchr (argv [0], '/')) ? pname + 1 : argv [0];
	if (parse_args (argc, argv))
		{
		(void) fprintf (stderr, "Usage: %s [-g newsgroups/distribution] [-t time] [-a username/password]\n", pname);
		(void) fprintf (stderr, "       [-d] [-i] [-l] [-r] [-w] server[/sublist][:timefile]\n");
		exit (2);
		}

	/* Open syslog if required with appropriate BSD 4.2/4.3 call */
#ifdef SYSLOG
#ifdef LOG_AUTH
	openlog(pname, LOG_PID, SYSLOG);
#else
	openlog(pname, LOG_PID);
#endif
#endif

	/* If groups not supplied in args, then get from slurp.sys file */
	if (nn_newsgroups == NULL)
		if (read_sys ())
			exit (2);

	/* If start time not supplied in args, then get from slurp.tim file */
	if (nn_time == NULL)
		if (get_ntime ())
			exit (2);

	if (debug_flag)
		{
		(void) fprintf (stderr, "server: %s\n", hostname);
		(void) fprintf (stderr, "time: %s\n", nn_time);
		(void) fprintf (stderr, "newsgroups: '%s'\n", nn_newsgroups);
		(void) fprintf (stderr, "distributions: '%s'\n", nn_distributions);
		}

	/* Unless don't write flag set, get time for next NEWNEWS */
	if (!no_time_flag)
		{
		if (local_time_flag)
			clock = time ((time_t *) 0);
		else
			if ((clock = server_time (hostname)) == 0)
				exit (3);

		now = gmtime (&clock);
		newdate = (now->tm_year * 10000) +
				 ((now->tm_mon + 1) * 100) +
				   now->tm_mday;
		newtime = (now->tm_hour * 10000) +
				  (now->tm_min * 100) +
				   now->tm_sec;
		}

	/* Open the history file */
	if (open_history ())
		log_sys ("Can't open history file %s", HISTORY_FILE);

#ifndef RNEWS
	/* Change to the incoming batch directory */
	if (chdir (INDIR))
		log_sys ("Can't change directory to %s", INDIR);
#endif

	/* Set up the connection to the server */
	switch (ret = server_init (hostname))
		{
		case -1 :
			exit (3);
		case OK_CANPOST :
		case OK_NOPOST :
			break;
		default :
			log_msg ("Can't talk to %s: got response code %d", hostname, ret);
			exit (4);
		}

	/* If authinfo details supplied, then use 'em */
	if (ai_username)
		do_authinfo ();

	/* Switch INN to nnrpd instead of innd if needed */
	if (mode_reader_flag)
		do_mode_reader ();

	/* Get a list of the new articles */
	get_ids ();

	/* Now get the actual articles */
	starttime = time ((time_t *) 0);
	if (entries > 0)
		{
		set_signals ();
		get_articles ();
		}
	endtime = time ((time_t *) 0);

	/* Time to say goodbye */
	close_server ();
	close_history ();

	/* Submit the remaining batch, if present */
	enqueue_batch ();

	/* do we want to update the timestamp file? */
	if (!no_time_flag)
		set_ntime ();

#ifdef SYSLOG
	if (!debug_flag)
		syslog (LOG_INFO,"Processed %d new, %d duplicate, %d missing articles",
				newart, dupart, misart);
	else
#endif
		(void) fprintf (stderr, "Processed %d new, %d duplicate, %d missing articles\n",
						newart, dupart, misart);

#ifdef SPEEDSTATS
  #ifdef SYSLOG
	if (!debug_flag)
		syslog (LOG_INFO, "Average transfer speed %ld cps",
				totalsize / (starttime == endtime ? 1 : endtime - starttime));
	else
  #endif
		(void) fprintf (stderr, "Average transfer speed %ld cps\n",
				totalsize / (starttime == endtime ? 1 : endtime - starttime));
#endif

	exit (0);
	}

/* END-OF-FILE */
