/*	SCCS Id: @(#)macunix.c	3.1	90/22/02
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

/* This file collects some Unix dependencies */

#include "hack.h"
#include <fcntl.h>

int FDECL(uptodate, (int));
void FDECL(regularize,(char *));
void NDECL(getlock);


int
uptodate(int fd)
{
#if defined(applec)
# pragma unused(fd)
#endif
	return(1);
}


void
regularize(char *s)
{
	register char *lp;

	while((lp=index(s, '.')) || (lp=index(s, ':')) )
		*lp = '_';
}


void
getlock( void )
{
	int fd ;
	int pid = getpid() ; /* Process Serial Number ? */

	set_levelfile_name ( lock , 0 ) ;

	if ( ( fd = open ( lock , O_RDWR | O_EXCL | O_CREAT , LEVL_TYPE ) ) == -1 ) {
		raw_printf ( "Could not lock the game %s." , lock ) ;
		panic ( "Another game in progress ?" ) ;
	}

	if ( write ( fd , ( char * ) & pid , sizeof ( pid ) ) != sizeof ( pid ) ) {
		raw_printf ( "Could not lock the game %s." , lock ) ;
		panic ( "Disk Locked ?" ) ;
	}

	close ( fd ) ;
}
