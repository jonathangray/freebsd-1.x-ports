/*
**  VERSION.H -- system version definition file.
**
**	NOTICE:
**		Version numbers stored in files are SCCS id's
**		and may not correspond to the external distribution
**		version number.  The distribution number applies to
**		the entire system and not to any particular file.
**		This file defines a "release" number, used for
**		creating file names.  The entire system version
**		number (including mod number) is defined by
**		conf/version.c.
**
**	Version:
**		@(#)version.h	8.1	12/31/84
*/
#ifndef INGRES_VERSION_H_
#define INGRES_VERSION_H_

/*
**	DBVERCODE is the code for this database version stored in
**		the admin file.
*/

#define	MAJOR_VERSION		"8"	/* major version number */
#define	MINOR_VERSION		"9"	/* minor version number */

#define	DBVERCODE	1		/* database version code */

#endif /* !INGRES_VERSION_H_ */
