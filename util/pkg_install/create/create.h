/* $Id: create.h,v 1.5 1994/05/19 18:27:38 alm Exp $ */

/*
 * FreeBSD install - a package for the installation and maintainance
 * of non-core utilities.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * Jordan K. Hubbard
 * 18 July 1993
 *
 * Include and define various things wanted by the create command.
 *
 */

#ifndef _INST_CREATE_H_INCLUDE
#define _INST_CREATE_H_INCLUDE

extern char	*Prefix;
extern char	*Comment;
extern char	*Desc;
extern char	*Install;
extern char	*DeInstall;
extern char	*Contents;
extern char	*Require;
extern char	*PlayPen;
extern char	*ExcludeFrom;
extern int	Dereference;

void		check_list(char *, Package *);
void		usage(const char *, const char *, ...);
int		pkg_perform(char **);
void		copy_plist(char *, Package *);

#endif	/* _INST_CREATE_H_INCLUDE */
