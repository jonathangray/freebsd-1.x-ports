/* 
 * Copyright (C) 1993 Andrew Scherpbier (Andrew@sdsu.edu)
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
#include "xpmtp.h"
#include <sys/errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>
#ifdef __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif
#include <errno.h>

int		xpmtp_errno = XPMTP_ERROR_NONE;

char	*xpmtp_errlist[] =
{
	"no error",					/* XPMTP_ERROR_NONE */
	"out of memory",			/* XPMTP_ERROR_MEMORY */
	"host not found",			/* XPMTP_ERROR_HOST */
	"connection failed",		/* XPMTP_ERROR_CONNECT */
	"cannot create socket",		/* XPMTP_ERROR_SOCKET */
	"cannot open socket",		/* XPMTP_ERROR_OPEN */
	"cannot read from socket",	/* XPMTP_ERROR_READ */
	"cannot write to socket",	/* XPMTP_ERROR_WRITE */
	"connection timeout",		/* XPMTP_ERROR_TIMEOUT */
};

#ifdef __STDC__
unsigned long	inet_addr(char *rp);
#else
unsigned long	inet_addr();
#endif

/*
 * open an XPMTP connection to host using port.
 * the connection response will be stored in the response buffer
 */
#ifdef __STDC__
int xpmtp_open(char *host, int port, char *response, int response_size)
#else
int xpmtp_open(host, port, response, response_size)
char	*host;
int		port;
char	*response;
int		response_size;
#endif
{
	u_long				addr;
	struct sockaddr_in	s;
	struct hostent		*hp;
	int					xpmtp_fd;
	int					i, n;

	xpmtp_errno = XPMTP_ERROR_NONE;

	memset((char *) &s, 0, sizeof(s));

	addr = inet_addr(host);
	if (addr == 0xffffffff)
	{
		hp = gethostbyname(host);
		if (hp == NULL)
		{
			xpmtp_errno = XPMTP_ERROR_HOST;
			return -1;
		}
		memcpy((char *)&s.sin_addr.s_addr, (char *)hp->h_addr, hp->h_length);
	}
	else
	{
		memcpy((char *)&s.sin_addr.s_addr, (char *)&addr, sizeof(addr));
	}

	s.sin_port = htons(port);
	s.sin_family = AF_INET;

	xpmtp_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (xpmtp_fd < 0)
	{
		xpmtp_errno = XPMTP_ERROR_SOCKET;
		return -1;
	}

	n = connect(xpmtp_fd, (struct sockaddr *) &s, sizeof(s));
	if (n == 0)
	{
		xpmtp_getline(xpmtp_fd, response, response_size);
		if (response[0] == XPMTP_ERROR)
		{
			xpmtp_errno = XPMTP_ERROR_OPEN;
			return -1;
		}
		return xpmtp_fd;
	}
	xpmtp_errno = XPMTP_ERROR_CONNECT;
	return -1;
}


/*
 * read nbytes from the XPMTP connection
 *
 * return the number of bytes actually read
 *
 * The routine was adapted from the readn() in "Unix Network Programming"
 * by W. Richard Stevens, page 279.
 */
#ifdef __STDC__
int	xpmtp_read(int xpmtp_fd, char *ptr, int nbytes)
#else
int	xpmtp_read(xpmtp_fd, ptr, nbytes)
int	xpmtp_fd;
char	*ptr;
int	nbytes;
#endif
{
	int	nleft, nread;

	xpmtp_errno = XPMTP_ERROR_NONE;

	nleft = nbytes;

	while (nleft > 0)
	{
		nread = read(xpmtp_fd, ptr, nleft);
		if (nread < 0)
		{
			xpmtp_errno = XPMTP_ERROR_READ;
			return -1;
		}
		if (nread == 0)
		{
			break;
		}
		nleft -= nread;
		ptr += nread;
	}

	return nbytes - nleft;
}

/*
 * write nbytes to the XPMTP connection
 *
 * return the number of bytes actually written
 *
 * The routine was adapted from the writen() in "Unix Network Programming"
 * by W. Richard Stevens, page 279-80.
 */
#ifdef __STDC__
int	xpmtp_write(int xpmtp_fd, char *ptr, int nbytes)
#else
int	xpmtp_write(xpmtp_fd, ptr, nbytes)
int	xpmtp_fd;
char	*ptr;
int	nbytes;
#endif
{
	int	nleft, nwritten;
	
	xpmtp_errno = XPMTP_ERROR_NONE;

	nleft = nbytes;

	while (nleft > 0)
	{
		nwritten = write(xpmtp_fd, ptr, nleft);
		if (nwritten < 0)
		{
			xpmtp_errno = XPMTP_ERROR_WRITE;
			return -1;
		}
		else if (nwritten == 0)
		{
			xpmtp_errno = XPMTP_ERROR_WRITE;
			return -1;
		}
		nleft -= nwritten;
		ptr += nwritten;
	}

	return nbytes - nleft;
}

/*
 * write a line to the XPMTP connection
 *
 * "\r\n" is appened to the string
 */
#ifdef __STDC__
int	xpmtp_putline(int xpmtp_fd, char *fmt, ...)
#else
int	xpmtp_putline(va_alist)
va_dcl
#endif
{
	va_list	args;
	char	buf[XPMTP_MAX_LINE];

#ifdef __STDC__
	va_start(args, fmt);
#else
	int	xpmtp_fd;
	char	*fmt;
	va_start(args);
	xpmtp_fd = va_arg(args, int);
	fmt = va_arg(args, char *);
#endif

	xpmtp_errno = XPMTP_ERROR_NONE;

	vsprintf(buf, fmt, args);
	va_end(args);
	strcat(buf, "\n");

	return xpmtp_write(xpmtp_fd, buf, strlen(buf)) != strlen(buf) ? -1 : 0;
}

/*
 * read a line from the XPMTP connection
 *
 * "\r\n" is removed from the line
 */
#ifdef __STDC__
int	xpmtp_getline(int xpmtp_fd, char *buf, int nbytes)
#else
int	xpmtp_getline(xpmtp_fd, buf, nbytes)
int	xpmtp_fd;
char	*buf;
int	nbytes;
#endif
{
	int	i, n, nleft, x;
	char	*ptr;
	char	tmp_buf[XPMTP_MAX_LINE];
	
	xpmtp_errno = XPMTP_ERROR_NONE;

	nleft = nbytes;
	ptr = buf;
	
	while (nleft > 0)
	{
		/*
		 * peek at the message so only the necessary data
		 * is actually read
		 */
		n = recv(xpmtp_fd, ptr, nleft, MSG_PEEK);
		if (n < 0)
		{
			xpmtp_errno = XPMTP_ERROR_READ;
			return -1;
		}
		else if (n == 0)
		{
			xpmtp_errno = XPMTP_ERROR_READ;
			return -1;
		}
		nleft -= n;
		for (i = 0; i < n; i++)
		{
			if (ptr[i] == '\r')
			{
				ptr[i] = '\0';
			}
			else if (ptr[i] == '\n')
			{
				ptr[i] = '\0';
				break;
			}
		}
again:
		x = read(xpmtp_fd, tmp_buf, i == n ? n : i+1);
		if (x < 0)
		{
			xpmtp_errno = XPMTP_ERROR_READ;
			return -1;
		}
		else if (x == 0)
		{
			xpmtp_errno = XPMTP_ERROR_READ;
			return -1;
		}
		else if (i < n)
		{
			return 0;
		}
		else
		{
			ptr += n;
		}
	}

	xpmtp_errno = XPMTP_ERROR_READ;

	return -1;
}

/*
 * send the XPMTP command to the XPMTP connection storing the response in the
 * response buffer
 */
#ifdef __STDC__
int	xpmtp_command(int xpmtp_fd, char *command, char *response, int response_size)
#else
int	xpmtp_command(xpmtp_fd, command, response, response_size)
int	xpmtp_fd;
char	*command;
char	*response;
int	response_size;
#endif
{
	xpmtp_errno = XPMTP_ERROR_NONE;

	if (xpmtp_putline(xpmtp_fd, command) < 0)
	{
		return -1;
	}
	if (xpmtp_getline(xpmtp_fd, response, response_size) < 0)
	{
		return -1;
	}
	return *response;
}

/*
 * close the XPMTP connection
 */
#ifdef __STDC__
int	xpmtp_close(int xpmtp_fd)
#else
int	xpmtp_close(xpmtp_fd)
int	xpmtp_fd;
#endif
{
	xpmtp_errno = XPMTP_ERROR_NONE;

	close(xpmtp_fd);

	return 0;
}

/*
 * report XPMTP errors
 */
#ifdef __STDC__
void	xpmtp_perror(char *message)
#else
void	xpmtp_perror(message)
char	*message;
#endif
{
	fprintf(stderr, "%s: %s\n", message, xpmtp_errlist[xpmtp_errno]);
}


/*
 * return the default XPMTP host
 */
char *xpmtp_default_host()
{
	return XPMTP_HOST;
}


