/*
 * articles - handle retrieval and batching of articles
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * This file provides a set of routines to retrieve articles from the
 * remote NNTP server and add to a batch of articles being piped to
 * the local news system via rnews.
 *
 * $Id: articles.c,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: articles.c,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/14  15:20:52  root
 * Removed some unnecessary initialisation.
 *
 * Revision 1.6  1993/04/22  18:24:21  root
 * Treat ERR_ACCESS result code when retrieving an article the same
 * as if the article was missing.
 *
 * Revision 1.5  1993/03/01  17:36:51  root
 * Removed stray text after an endif.
 *
 * Revision 1.4  1993/02/14  14:44:25  root
 * Fixed problem with newline being added to article line buffer which
 * could already be at its maximum length.
 * Support for writing out batch files as well as piping batches to
 * rnews.
 * If BATCHSIZE is zero then keep pipe open to rnews indefinitely.
 * If error occurs then submit batch and dump unretrieved ids.
 *
 * Revision 1.3  1992/12/15
 * Minor tidy-ups, plus fixed flushing of tfp every time after it had
 * been opened once.
 *
 * Revision 1.1  1992/12/04
 * Print line before it is sent to server when debugging is on.
 *
 * Revision 1.0  1992/11/27
 * Adapted from nntpxfer-e code.
 * Pipe batches to rnews instead of creating in in.coming directory,
 * so can be used with INN.
 *
 */

#include "slurp.h"


static struct mnode *last_article = NULL; /* last article requested */
static char artbuf [COPYSIZE];		/* temp storage for article in memory */
static char *endart = artbuf;		/* points to just past end of article */
static int incore = TRUE;			/* article in memory, not temp file */
static FILE *tfp;					/* temporary file descriptor */

static FILE *batchfp = NULL;		/* file descriptor for batch */
static size_t batchsize = 0;		/* size of current batch */
static char batchname [PATH_MAX];	/* name of current batch */

static void new_batch ();
static void read_article ();
static void batch_article ();
static void fetch_article ();
static void get_article ();
static void request_article (char *msgid);
static void traverse_tree ();


/*
 * new_batch - Determines if there is enough room for the batch on the
 * disk containing the news spool directories. If there is, then a pipe
 * is opened to rnews and the batch variable initialised with the
 * details.
 */

	static void
new_batch ()
	{
	/* Make sure there is enough room for batch */
#ifdef MINFREE
	if (!space (MINFREE))
		{
		log_msg ("new_batch: Not enough space for incoming batch");
		if ((root != NULL) && (!no_time_flag) && (!no_id_load_flag))
			set_ntime ();
		exit (5);
		}
#endif

#ifdef RNEWS
	/* Open a pipe to rnews for the batch */
	if ((batchfp = popen (RNEWS, "w")) == NULL)
		log_sys ("new_batch: Can't open pipe to %s", RNEWS);
#else
	/* Open a file in incoming news directory with temporary name */
	(void) strcpy (batchname, BATCHNAME);
	(void) mktemp (batchname);
	if ((batchfp = fopen (batchname, "w")) == NULL)
		log_sys ("new_batch: Can't open file %s", batchname);
#endif
	}


/*
 * read_article - Read an article into artbuf or, if too large, into a
 * temporary file from the currently open NNTP server socket, reading up
 * to the end-of-article marker, a '.' on a single line. If it is stored
 * in memory, then incore will be TRUE, otherwise it will be FALSE and the
 * temporary file name will be stored in tempfile.
 */

	static void
read_article ()
	{
	char *realline;
	char line [NNTP_STRLEN];
	int lines = 0;
	int len;

	incore = TRUE;
	endart = artbuf;

	/* Read in the article */
	for (;;)
		{
		get_server (line, sizeof (line));

		/* Dot on its own means article end reached */
		if (!strcmp (line, "."))
			break;

		/* remove hidden dot if present */
		realline = (line [0] == '.' ? line + 1 : line);

		/* Article is currently stored in memory */
		if (incore)
			{
			/* If no room in artbuf, open tempfile and copy article there */
			len = strlen (realline);
			if ((endart + len + 2 ) > (artbuf + sizeof (artbuf)))
				{
				if ((tfp = tmpfile ()) == NULL)
					log_sys ("read_article: Can't create temporary file");
				(void) fwrite (artbuf, 1, endart - artbuf, tfp);
				if (ferror (tfp))
					log_sys ("read_article: Can't write to tempfile");
				(void) fputs (realline, tfp);
				(void) putc ('\n', tfp);
				if (ferror (tfp))
					log_sys ("read_article: Can't write to tempfile");
				incore = FALSE;
				}
			else
				{
				/* If fits, append realline to artbuf at endart */
				(void) strcpy (endart, realline);
				endart += len;
				*endart++ = '\n';
				*endart = '\0';
				}
			}

		/* Already writing article to temp file */
		else
			{
			(void) fputs (realline, tfp);
			(void) putc ('\n', tfp);
			if (ferror (tfp))
				log_sys ("read_article: Can't write to tempfile");
			}

		lines++;
		}

	/* Article successfully read in */
	if (debug_flag)
		(void) fprintf (stderr, "-> %d lines\n", lines);
	}


/* batch_article - Append "#! rnews <count>" and the article from artbuf
 * or temporary file to the batch file.
 */

	static void
batch_article ()
	{
	size_t bytes;
	size_t size;

	/* Find article size */
	if (incore)
		size = endart - artbuf;
	else
		size = ftell (tfp);

	totalsize += size;
	batchsize += size;

	/* Print the article header */
	(void) fprintf (batchfp, "#! rnews %ld %s\n", (long) size, hostname);

	/* Copy the article to the batch file */
	if (incore)
		{
		(void) fwrite (artbuf, 1, size, batchfp);
		if (ferror (batchfp))
			log_sys ("batch_article: Can't write to batch");
		}
	else
		{
		rewind (tfp);
		while ((bytes = fread (artbuf, 1, sizeof (artbuf), tfp)) > 0)
			{
			(void) fwrite (artbuf, 1, bytes, batchfp);
			if (ferror (batchfp))
				log_sys ("batch_article: Can't write to batch");
			}
		(void) fclose (tfp);

		}
	}


/*
 * fetch_article - Retrieve an article from the currently open NNTP
 * server socket which has already been requested. The article is written
 * to the end of the current batch. If there is not already a batch
 * then a new pipe to rnews for the batch will be opened. If the current
 * batch is too large or has too many articles then the pipe will be
 * closed so that the batch may be submitted to the news system.
 */

	static void
fetch_article ()
	{
	/* Open a new batch if required */
	if (batchfp == NULL)
		new_batch ();

	/* Read in article */
	read_article ();

	/* Add it to the batch */
	batch_article ();

	/* Submit batch if ready */
	if ((batchsize > BATCHSIZEMAX) && (BATCHSIZEMAX != 0))
		enqueue_batch ();
	}


/*
 * get_article
 */

	static void
get_article ()
	{
	char status [NNTP_STRLEN];

	/* Read status line from server */
	get_server (status, sizeof (status));
	if (debug_flag)
		(void) fprintf (stderr, "-> %s\n", status);

	switch (atoi (status))
		{
		/* If missing, then add to missing list */
		case ERR_NOART:
		case ERR_ACCESS:
			misart++;
			newart--;
			return;

		/* If present, then fetch and add to batch */
		case OK_ARTICLE:
			fetch_article ();
			break;

		/* Otherwise must be a protocol error */
		default:
			log_msg ("get_article: NNTP protocol error: got '%s'", status);
			if ((root != NULL) && (!no_time_flag) && (!no_id_load_flag))
				set_ntime ();
			exit (4);
		}
	}


/*
 * request_article - Request an article with specified id from the server
 */

	static void
request_article (char *msgid)
	{
	char request [NNTP_STRLEN];

	(void) sprintf (request, "ARTICLE %s", msgid);
	if (debug_flag)
		(void) fprintf (stderr, "<- %s\n", request);
	put_server (request);
	}


/*
 * traverse_tree - Traverse the tree requesting and getting each article
 */

	static void
traverse_tree (struct mnode *p)
	{
	if (p != NULL)	
    	{
		traverse_tree (p->left);
		request_article (p->msgid);
#ifdef SPEEDUP
		if (last_article != NULL)
			{
			get_article ();
			last_article->used = TRUE;
			}
    	last_article = p;
#else
		get_article ();
		p->used = TRUE;
#endif /* SPEEDUP */
		traverse_tree (p->right);
    	}
	}


/*
 * get_articles - Get the articles from the server whose message ids
 * were previously collected with do_newnews.
 */

	void
get_articles ()
	{
	traverse_tree (root);
#ifdef SPEEDUP
	get_article ();
	last_article->used = TRUE;
#endif
	}


/*
 * enqueue_batch - Submit the batch to the new system by closing the
 * currently open pipe to rnews or renaming the temporary file in the
 * incoming news directory so it can be seen by the news system.
 */

	void
enqueue_batch ()
	{
#ifndef RNEWS
	char permname [PATH_MAX];
	time_t now;
#endif

	/* Return if there is no currently open batch */
	if (batchfp == NULL)
		return;

#ifdef RNEWS
	/* Close the pipe to rnews */
	if (pclose (batchfp))
		log_sys ("enqueue_batch: Can't close pipe to %s", RNEWS);
#else
	/* Close the temporary file */
	if (fclose (batchfp))
		log_sys ("enqueue_batch: Can't close %s", batchname);

	/* Rename it so it can be seen by news */
	for (;;)
		{
		(void) sprintf (permname, "%ld.t", (long) time (&now));
		if (link (batchname, permname) == 0)
			break;
		if (errno != EEXIST)
			log_sys ("enqueue_batch: Error linking %s to %s", batchname, permname);
		(void) sleep (2);
		}
	if (unlink (batchname))
			log_sys ("enqueue_batch: Error unlinking %s", batchname);
#endif /* RNEWS */

	/* Reset the batch descriptor for a new batch */
	batchfp = NULL;
	batchsize = 0;
	}

/* END-OF-FILE */
