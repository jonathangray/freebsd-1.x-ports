/*
 * history - handle a news history file
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * Routines to open and close a C-News style history file and determine
 * whether or not a particular message id exists in the history file.
 *
 * $Id: history.c,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: history.c,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/07  11:07:14  root
 * If neither DBZ, DBM or NDBM are defined then don't carry out
 * any history file lookups.
 *
 * Revision 1.6  1993/04/22  18:07:11  root
 * No changes - put back in RCS after the RCS file went missing...
 *
 * Revision 1.4  1993/02/14  14:51:59  root
 * No changes.
 *
 * Revision 1.0  1992/09/92
 * Initial coding.
 *
 */

#include "slurp.h"

#ifdef DBM
  #undef NULL
  #include <dbm.h>
  #undef NULL
  #define NULL 0
#endif

#ifdef DBZ
  #include <dbz.h>
#endif

#ifdef NDBM
  #include <ndbm.h>
  #include <fcntl.h>
  static DBM *db = NULL;
#endif


/*
 * open_history - Open history file
 */

	int
open_history ()
	{
#if defined (DBM) || defined (DBZ)
	if (dbminit (HISTORY_FILE) < 0)
		return (1);
#elif defined (NDBM)
 	if ((db = dbm_open (HISTORY_FILE, O_RDONLY, 0)) == NULL)
		return (1);
#endif

	return (0);
	}


/*
 * close_history - Close history file
 */

	void
close_history ()
	{
#if defined (DBM) || defined (DBZ)
	(void) dbmclose ();
#elif defined (NDBM)
 	dbm_close (db);
#endif
	}


/*
 * Determine if message id already exists in the history file
 */

	int
check_id (char *message_id)
	{
#if defined (DBM) || defined (NDBM) || defined (DBZ)
	datum k, d;

/* Now check for presence with dbm/ndbm */

	k.dptr = message_id;
	k.dsize = strlen (message_id) + 1;
#endif

#if defined (DBM) || defined (DBZ)
	d = fetch (k);
	return (d.dptr == NULL);
#elif defined (NDBM)
 	d = dbm_fetch (db, k);
	return (d.dptr == NULL);
#else
	return (TRUE);
#endif
	}

/* END-OF-FILE */
