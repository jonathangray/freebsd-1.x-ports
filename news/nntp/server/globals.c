#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/globals.c,v 1.1 1993/07/19 20:04:30 nate Exp $";
#endif

/*
 * Common variables.
 */

#include "common.h"

/*
 * Variables initialized from ../common/conf.h
 */

char	spooldir[] = SPOOLDIR;
char	activefile[] = ACTIVE_FILE;
char	accessfile[] = ACCESS_FILE;
char	distributionsfile[] = DISTRIBUTIONS_FILE;
char	newsgroupsfile[] = NEWSGROUPS_FILE;
char	historyfile[] = HISTORY_FILE;
#ifdef ACTIVE_TIMES_FILE
char	ngdatefile[] = ACTIVE_TIMES_FILE;
#else
char	ngdatefile[] = NGDATE_FILE;
#endif
char	inews[] = INEWS;
char	rnews[] = RNEWS;

/*
 * Other random externals.
 */

char	**group_array;
char 	*actbuf;
int	num_groups;
int	ingroup = 0;
int	art_ptr;
int	num_arts;
#ifdef DYNAMIC_ART_ARRAY
int	*art_array = 0;		/* dynamic array */
unsigned int size_art_array = 0;	/* current size of art_array */
#else
int	art_array[MAX_ARTICLES];
#endif
FILE	*art_fp;
int	uid_poster, gid_poster;
char	*home_poster;
int	canpost, canread, canxfer;
char	**ngpermlist;
int	ngpermcount;
char	hostname[256];
int	debug;

#ifdef AUTH
int	Needauth;	/* 1 if we need to do authorization */
char	User[10];	/* username for authentication */
#endif AUTH

#ifdef LOG
int	arts_acsd;
int	grps_acsd;
#endif
