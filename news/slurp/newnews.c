/*
 * newnews - Read in list of ids of new articles
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * Using a previously initialised list of newsgroups, carries out a series
 * of NEWNEWS requests to the connected NNTP server, storing the message
 * ids of new articles in a binary tree in memory.
 *
 * $Id: newnews.c,v 1.1 1993/08/27 02:47:47 alm Exp $
 *
 * $Log: newnews.c,v $
 * Revision 1.1  1993/08/27 02:47:47  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/14  15:22:24  root
 * Modified parse_groups to only malloc enough space for arrays.
 * Rewrote get_ids, incorporating the get_not_groups function. This
 * makes the algorithm neater, fixes a problem with using memset on a
 * zero length block, and fixes a problem whereby with certain
 * newsgroup combinations a line could overflow its buffer.
 * In process_id when in debug mode print after a message ID if ID
 * discarded due to hitting maximum number of articles or was already
 * present in the tree.
 *
 * Revision 1.5  1993/03/01  17:45:16  root
 * Added cast to bzeroing of used_not_group_array.
 *
 * Revision 1.4  1993/02/14  14:55:41  root
 * Malloc msgid space separately from mnode.
 * Split-out process_id from do_newnews so it can be used in get_ntime
 * to load the tree with the unretrieved message ids.
 *
 * Revision 1.3  1992/12/14
 * Only malloc enough space for msgid, not whole mnode structure.
 * Minor tidy-ups.
 *
 * Revision 1.1  1992/12/06
 * Set no_time_flag if hit max no of messages
 * Print line before it is sent to server when debugging is on.
 * No longer need to handle null nn_distributions.
 *
 * Revision 1.0  1992/11/30
 * Transferred functions from slurp.c
 *
 */

#include "slurp.h"

static int hit_max = FALSE;

static char **group_array;
static char **not_group_array;
static int  *used_not_group_array;

static int  groups_no = 1;
static int  not_groups_no = 0;


static void parse_groups ();
static int  add_id (char *msgid);
static void do_newnews (char *line);
static int  restreql (register char *w, register char *s);


/*
 * parse_groups - Turn list of groups into two arrays containing 
 * pointers to groups to include and groups to exclude.
 */

	static void
parse_groups ()
	{
	int g = 0;
	int n = 0;
	int got = TRUE;
	int not = FALSE;
	char *cp;

	/* Calculate number of group entries */
	for (cp = nn_newsgroups; *cp != '\0'; cp++)
		{
		if (*cp == ',')
			groups_no++;
		if (*cp == '!')
			{
		 	not_groups_no++;
		 	groups_no--;
		 	}
		 }

	/* Malloc space for include and exclude group arrays */
	if ((group_array = (char **) malloc ((size_t) groups_no * sizeof (char *))) == NULL)
		log_sys ("parse_groups: malloc %d bytes", groups_no * sizeof (char **));

	if (not_groups_no > 0)
		{
		if ((not_group_array = (char **) malloc ((size_t) not_groups_no * sizeof (char *))) == NULL)
			log_sys ("parse_groups: malloc %d bytes", not_groups_no * sizeof (char **));

		if ((used_not_group_array = (int *) malloc ((size_t) not_groups_no * sizeof (int))) == NULL)
			log_sys ("parse_groups: malloc %d bytes", not_groups_no * sizeof (int));
		}

	/* Now start parsing the newsgroup list */
	for (cp = nn_newsgroups; *cp != '\0'; cp++)
		{
		if (*cp == '!')
			got = FALSE;

		if (got)
			{
			group_array [g++] = cp;
			got = FALSE;
			}

		if (not)
			{
			not_group_array [n++] = cp;
			not = FALSE;
			}

		if (*cp == ',')
			{
			*cp = '\0';
			got = TRUE;
			}

		if (*cp == '!')
			not = TRUE;
		}
	}


/*
 * store_node - Store a new node in the binary tree
 */

	static struct mnode *
store_node (char *msgid)
	{
	struct mnode *node;

	if ((node = (struct mnode *) malloc (sizeof (struct mnode))) == NULL)
		log_sys ("add_id: malloc %d bytes", sizeof (struct mnode));
	node->left = NULL;
	node->right = NULL;
	node->used = FALSE;
	if ((node->msgid = (char *) malloc (strlen (msgid) + sizeof (char))) == NULL)
		log_sys ("store_node: malloc %d bytes", strlen (msgid) + sizeof (char));
	(void) strcpy (node->msgid, msgid);
	entries++;
	return (node);
	}


/*
 * add_id - Add a message id to the binary tree if not already present.
 * Returns -1 if the maximum number of entries in the tree has been
 * reached, 0 if the item is added okay, 1 if an entry with that 
 * particular message id already exists.
 */

	static int
add_id (char *msgid)
	{
	struct mnode *current;
	int test;

	/* Test if hit the maximum number of entries in the cache */
	if (entries >= MAXCACHE)
		return (-1);

	/* Handle the case when the tree is empty */
	if (root == NULL)
		{
		root = store_node (msgid);
		return (0);
		}

	/* Search the tree for correct position to insert node */
	current = root;
	
	for (;;)
		{
		test = strcmp (msgid, current->msgid);
		if (test < 0)
			{
			if (current->left == NULL)
				{
				current->left = store_node (msgid);
				return (0);
				}
			else
				current = current->left;
			}
		else if (test > 0)
			{
			if (current->right == NULL)
				{
				current->right = store_node (msgid);
				return (0);
				}
			else
				current = current->right;
			}
		else
			return (1);
		}
	}


/*
 * process_id - Check if id already exists in local history file, if not
 * then add it to the message id tree if it isn't already in there.
 */

	void
process_id (char *msgid)
	{
	char *cp;

	/* Modify the message id appropriate to C-News history files */
	if ((cp = strchr (msgid, '@')) != NULL)
		{
		for (; *cp != '\0'; cp++)
			if (isupper (*cp))
				*cp = tolower (*cp);
		}

	if (debug_flag)
		(void) fprintf (stderr, "-> %s", msgid);

	if (check_id (msgid))
		{
		switch (add_id (msgid))
			{
			case -1 :
				hit_max = TRUE;
				if (debug_flag)
					(void) fprintf (stderr, " discarded\n");
				break;
			case  0 :
				newart++;
				if (debug_flag)
					(void) fprintf (stderr, " new\n");
				break;
			default :
				if (debug_flag)
					(void) fprintf (stderr, " present\n");
				break;
			}
		}
	else
		{
		dupart++;
		if (debug_flag)
			(void) fprintf (stderr, " duplicate\n");
		}
	}


/*
 * do_newnews - Process a newnews for supplied list of groups, adding the
 * resultant data to the message id tree.
 */

	static void
do_newnews (char *line)
	{
	char buf [NNTP_STRLEN];

	/* Create a full string to send to the server */
	(void) sprintf (buf, "NEWNEWS %s %s GMT %s", line, nn_time,
					nn_distributions);

	/* Do the actual NEWNEWS */
	if (debug_flag)
		(void) fprintf (stderr, "<- %s\n", buf);
	put_server (buf);
	
	/* Get the response and check it's okay */
	get_server (buf, sizeof (buf));
	if (debug_flag)
		(void) fprintf (stderr, "-> %s\n", buf);
	if (atoi (buf) != OK_NEWNEWS)
		{
		log_msg ("do_newnews: NNTP protocol error: got '%s'", buf);
		exit (4);
		}
	                
	/* Now get the data and stick it in the tree */
	for (;;)
		{
		get_server (buf, sizeof (buf));
		if (!strcmp (buf, "."))
			break;

		process_id (buf);
		}
	}


/*
 * restreql -- A small regular expression string equivalence routine
 * purloined from nntp 1.5.11 which credits <lai@shadow.berkeley.edu>
 * for its creation. Returns 1 if the string pointed to by 's' matches
 * the asterisk-broadened regexp string pointed to by 'w', otherwise
 * returns 0.
 */

	static int
restreql (register char *w, register char *s)
	{
	while (*s && *w)
		{
		switch (*w)
			{
			case '*':
				for (w++; *s; s++)
					if (restreql(w, s))
						return (1);
				break;
			default:
				if (*w != *s)
					return (0);
				w++, s++;
				break;
			}
		}
	if (*s)
		return (0);
	while (*w)
		if (*w++ != '*')
			return 0;

	return (1);
	}


/*
 * get_ids - Store in memory a tree of the message ids of new article at
 * the server which match the specified set of groups and distributions
 * for the currently connected host.
 */

	void
get_ids ()
	{
	char line [NNTP_STRLEN];
	char newgroups [NNTP_STRLEN];
	int i, j, add_comma;
	size_t newlen, linelen;
	size_t startlen = 30 + strlen (nn_distributions);

	/* Turn comma-separated list of groups into 2 arrays */
	parse_groups ();

	/* Initialiase for first list of groups to send to server */
	*line = '\0';
	linelen = startlen;
	if (not_groups_no)
		(void) memset (used_not_group_array, 0, not_groups_no * sizeof (int));
	add_comma = FALSE;

	for (i = 0 ; i < groups_no ; i++)
		{
		/* Check group isn't so big it doesn't fit at all */
		newlen = strlen (group_array [i]);
		if ((newlen + startlen) > NNTP_STRLEN)
			{
			log_msg ("get_ids: Not enough room in NNTP line for newsgroup %s",
					 group_array [i]);
			exit (2);
			}

		/* Get list containing new group and matching ! groups */
		(void) strcpy (newgroups, group_array [i]);
		for (j = 0 ; j < not_groups_no ; j++)
			if (!used_not_group_array [j])
				if (restreql (group_array [i], not_group_array [j]))
					{
					newlen += strlen (not_group_array [j]) + 2;
					if ((newlen + startlen) > NNTP_STRLEN)
						{
						log_msg ("get_ids: Not enough room in NNTP line for exclusion list %s",
								 newgroups);
						exit (2);
						}
					(void) strcat (newgroups, ",!");
					(void) strcat (newgroups, not_group_array [j]);
					used_not_group_array [j] = TRUE;
					}

		/* If can't add new groups to existing list, then do a newnews */
		if ((linelen + newlen + add_comma) > NNTP_STRLEN)
			{
			do_newnews (line);
			*line = '\0';
			linelen = startlen;
			if (not_groups_no)
				(void) memset (used_not_group_array, 0, not_groups_no * sizeof (int));
			add_comma = FALSE;
			}

		linelen += newlen + add_comma;
		if (add_comma)
			(void) strcat (line, ",");
		else
			add_comma = TRUE;
		(void) strcat (line, newgroups);
		}

	do_newnews (line);

	/* Report if couldn't fit everything in the tree */
	if (hit_max)
		{
		log_msg ("Maximum limit of %d messages hit", MAXCACHE);
		no_time_flag++;
		}
	}

/* END-OF-FILE */
