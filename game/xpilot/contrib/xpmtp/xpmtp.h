/* 
 * Copyright (C) 1993 Andrew Scherpbier (Andrew@sdsu.edu)
 *
 * This file is part of xpmtp.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _xpmtp_h
#define _xpmtp_h

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>

#undef	FALSE
#define FALSE	0
#undef	TRUE
#define TRUE	1

#undef	MIN
#define	MIN(a,b)				((a)<(b)?(a):(b))

#define XPMTP_PORT				4444
#define	XPMTP_HOST				"xpilot.sdsu.edu"


/*
 * XPMTP errors used by xpmtp_errno
 */
#define XPMTP_ERROR_NONE		0
#define XPMTP_ERROR_MEMORY		1
#define XPMTP_ERROR_HOST		2
#define XPMTP_ERROR_CONNECT		3
#define XPMTP_ERROR_SOCKET		4
#define XPMTP_ERROR_OPEN		5
#define XPMTP_ERROR_READ		6
#define XPMTP_ERROR_WRITE		7
#define XPMTP_ERROR_TIMEOUT		8

/*
 * XPMTP response types
 */
#define XPMTP_ERROR				'-'
#define XPMTP_OK				'+'
#define XPMTP_TIMEOUT			'!'


#define XPMTP_MAX_LINE			1024
#define XPMTP_MAX_ARGS			32

extern int	xpmtp_errno;
extern char	*xpmtp_errlist[];

#ifdef __cplusplus
#ifndef __STDC__
#define __STDC__
#endif
extern "C" {
#endif

#ifdef __STDC__
extern int	xpmtp_open(char *host, int port, char *response, int response_size);
extern int	xpmtp_read(int xpmtp_fd, char *buf, int nbytes);
extern int	xpmtp_write(int xpmtp_fd, char *buf, int nbytes);
extern int	xpmtp_close(int xpmtp_fd);
extern void	xpmtp_perror(char *message);
extern int	xpmtp_putline(int xpmtp_fd, char *fmt, ...);
extern int	xpmtp_getline(int xpmtp_fd, char *buf, int nbytes);
extern int	xpmtp_command(int xpmtp_fd, char *command, char *response, int response_size);
extern char *xpmtp_default_host();
#else
extern int	xpmtp_open(/* char *host, int port, char *response, int response_size */);
extern int	xpmtp_read(/* int xpmtp_fd, char *buf, int nbytes */);
extern int	xpmtp_write(/* int xpmtp_fd, char *buf, int nbytes */);
extern int	xpmtp_close(/* int xpmtp_fd */);
extern void	xpmtp_perror(/* char *message */);
extern int	xpmtp_putline(/* int xpmtp_fd, char *fmt, ... */);
extern int	xpmtp_getline(/* int xpmtp_fd, char *buf, int nbytes */);
extern int	xpmtp_command(/* int xpmtp_fd, char *command, char *response, int response_size */);
extern char *xpmtp_default_host();
#endif

#ifdef __cplusplus
}
#endif

#ifdef NEED_STRDUP
#ifdef __STDC__
extern char	*strdup(char *str);
#else
extern char	*strdup(/* char *str */);
#endif
#endif /* NEED_STRDUP */

#endif /* _xpmtp_h */
