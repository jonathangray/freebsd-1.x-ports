/********************************************************************
 * lindner
 * 3.10
 * 1993/08/16 19:41:23
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/util.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: util.c
 * Various useful utilities for gopher clients and servers
 *********************************************************************
 * Revision History:
 * util.c,v
 * Revision 3.10  1993/08/16  19:41:23  lindner
 * Fix for DECC/Alpha
 *
 * Revision 3.9  1993/08/09  20:17:13  lindner
 * Fixes for CMULIB and NETLIB for VMS
 *
 * Revision 3.8  1993/08/05  03:23:40  lindner
 * Changes for CMUIP and NETLIB
 *
 * Revision 3.7  1993/07/27  05:30:30  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.6  1993/07/23  04:49:24  lindner
 * none
 *
 * Revision 3.5  1993/06/22  05:49:39  lindner
 * *** empty log message ***
 *
 * Revision 3.4  1993/04/23  20:11:56  lindner
 * Fixed misdeclaration of DEBUG
 *
 * Revision 3.3  1993/04/15  17:53:12  lindner
 * Debug stuff from Mitra
 *
 * Revision 3.2  1993/03/18  22:28:27  lindner
 * changed hex routines around for admit1
 *
 * Revision 3.1.1.1  1993/02/11  18:03:05  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.5  1993/01/31  00:31:12  lindner
 * New functions, readword() and readtotoken()
 *
 * Revision 2.4  1993/01/12  21:12:23  lindner
 * Reverted to old readfield behavior()  \n is now ignored again.
 *
 * Revision 2.3  1993/01/08  23:29:21  lindner
 * More mods from jqj.
 *
 * Revision 2.2  1992/12/31  04:58:41  lindner
 * merged 1.1.1.1 and 2.1
 *
 * Revision 2.1  1992/12/21  19:41:14  lindner
 * Added check for null in writestring
 * Added function skip_whitespace
 *
 * Revision 1.1.1.1  1992/12/31  04:52:01  lindner
 * Changes for VMS
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "Malloc.h"
#include "String.h"
#include <ctype.h>
#include "boolean.h"
#include "util.h"
#include <stdio.h>
#include "Debug.h"


#if defined(VMS) && defined(UCX)
#include <errno.h>
#endif
 

#if defined(VMS) && (defined(WOLLONGONG) || defined(MULTINET) ||defined(CMUIP)||defined(NETLIB))
/* Multinet and Wollongong,etc. (non UCX-emulation) use channel numbers */
/* for sockets, which are small multiples of 16.  The first 5 */
/* channels can be assumed to be already used, so we assume that */
/* sockets start at 64, and that only 64 VAXC fds are simultaneously */
/* open in the program.  Actually, the first socket is likely to be */
/* more like 176! */
#define IS_SOCKET(s) ((s)>=64)
 
/* Close a socket.  
 * Note that in old Wollongong and Multinet implementations close()
 * works only on fds, not sockets.
 * For UCX and Unix, closenet() is #defined to be close()
 */
int closenet(s)
int s;
{
    if (IS_SOCKET(s)) {
#ifdef MULTINET
	return (socket_close(s));
#else /* WOLLONGONG, CMUIP, NETLIB */
	return (netclose(s));
#endif
    }
    else
	close(s); /* shouldn't be calling this routine */
}
#endif /* WOLLANGONG or MULTINET */


/* Read "n" bytes from a descriptor.
 * Use in place of read() when fd is a stream socket
 *
 * Returns the number of total bytes read.
 */

int readn(fd, ptr, nbytes)
  int fd;
  char *ptr;
  int nbytes;
{
     int nleft, nread;
     
     nleft = nbytes;
     while (nleft > 0) {
#if defined(VMS) && (defined(WOLLONGONG) || defined(CMUIP) || defined(NETLIB))
	  nread = IS_SOCKET(fd) ? netread(fd, ptr, nleft) : read(fd, ptr, nleft);
#else
#if defined(VMS) && defined(MULTINET)
	  nread = IS_SOCKET(fd) ? socket_read(fd, ptr, nleft) : read(fd, ptr, nleft);
#else
	  nread = read(fd, ptr, nleft);
#endif
#endif
#if defined(VMS) && defined(UCX)
          if (nread < 0 && errno == EPIPE)
	       break;
#endif
	  if (nread < 0)
	       return(nread);	/* error, return <0 */
	  else if (nread == 0)	/* EOF */
	       break;
	  
	  nleft 	-= nread;
	  ptr 	+= nread;
     }
     return(nbytes - nleft);	/* return >= 0) */
}



/*
 * Write "n" bytes to a descriptor.
 * Use in place of write() when fd is a stream socket
 *
 * We return the number of bytes written
 */

int 
writen(fd, ptr, nbytes)
  int	fd;
  char	*ptr;
  int	nbytes;
{
     int nleft, nwritten;
     
     nleft = nbytes;
     while(nleft > 0) {
#if defined(VMS) && (defined(WOLLONGONG) || defined(CMUIP) || defined(NETLIB))
	  nwritten = IS_SOCKET(fd) ? netwrite(fd, ptr, nleft) : write(fd, ptr, nleft);
#else
#if defined(VMS) && defined(MULTINET)
	  nwritten = IS_SOCKET(fd) ? socket_write(fd, ptr, nleft) : write(fd, ptr, nleft);
#else
	  nwritten = write(fd, ptr, nleft);
#endif
#endif
	  if (nwritten <= 0)
	       return(nwritten);	/* error */
	  
	  nleft	-= nwritten;
	  ptr	+= nwritten;
     }
     return(nbytes - nleft);
}


/*
 * Writestring uses the writen and strlen calls to write a
 * string to the file descriptor fd.  If the write fails
 * a -1 is returned. Otherwise zero is returned.
 */

int writestring(fd, stringptr)
  int	fd;
  char	*stringptr;
{
     int length;

     Debug("writing: %s\n",stringptr);

     if (stringptr == NULL)
	  return(0);

     length = strlen(stringptr);
     if (writen(fd, stringptr, length) != length) {
	  Debug("writestring: writen failed",0);
	  return(-1);
     }
     else
	  return(0);
}

/*
 * Read from the socket into a buffer.  Mucho more efficent in terms of
 * system calls..
 * 
 * returns bytes read, or <0 for an error
 */

#define RECVSIZE 4096

int readrecvbuf(sockfd, buf, len)
  int sockfd;
  char *buf;
  int len;
{
     static char recvbuf[RECVSIZE];
     static int  recvbufptr  = 0;
     static int  recvbufsize = 0;
     static int Oldsockfd = 0;
     int bytesread = 0;

     if (recvbufptr == 0 || Oldsockfd != sockfd) {
#if defined(VMS) && (defined(WOLLONGONG)||defined(CMUIP)||defined(NETLIB))
	  recvbufsize = IS_SOCKET(sockfd) ?
		netread(sockfd, recvbuf, RECVSIZE) : read(sockfd, recvbuf, RECVSIZE);
#else
#if defined(VMS) && defined(MULTINET)
	  recvbufsize = IS_SOCKET(sockfd) ?
		socket_read(sockfd, recvbuf, RECVSIZE) : read(sockfd, recvbuf, RECVSIZE);
#else
	  recvbufsize = read(sockfd, recvbuf, RECVSIZE);
#endif
#endif
	  Oldsockfd = sockfd;
	  recvbufptr = 0;
	  if (recvbufsize == 0)
	       return(0);
#if defined(VMS) && defined(UCX)
	  if (recvbufsize < 0 && errno == EPIPE)
	       return(0);
#endif

     }
     
     while (len--) {
	  *buf++ = recvbuf[recvbufptr++];
	  bytesread++;

	  if (recvbufptr == recvbufsize && len != 0) {
	       recvbufsize = readn(sockfd, recvbuf, RECVSIZE);
	       recvbufptr = 0;
	       if (recvbufsize == 0)
		    return(bytesread);
	       if (recvbufsize < 0)
		    return(recvbufsize);

	  } else if (recvbufptr >= recvbufsize)
	       recvbufptr = 0;
     }
     return(bytesread);
}     


/*
 * Read a line from a descriptor.  Read the line one byte at a time,
 * looking for the newline.  We store the newline in the buffer,
 * then follow it with a null (the same as fgets(3)).
 * We return the number of characters up to, but not including,
 * the null (the same as strlen(3))
 */

int readline(fd, ptr, maxlen)
  int	fd;
  char	*ptr;
  int 	maxlen;
{
     int n;
     int rc;
     char c;

     
     for (n=1; n < maxlen; n++) {
	  if ( (rc = readrecvbuf(fd, &c, 1)) == 1) {
	       *ptr++ = c;
	       if (c == '\n')
		    break;
	  }
	  else if (rc == 0) {
	       if (n == 1)
		    return(0);	/* EOF, no data read */
	       else
		    break;		/* EOF, some data was read */
	  }
	  else
	       return(-1);		/* error */
     }
     
     *ptr = 0; 				/* Tack a NULL on the end */
     Debug("readline: %s\n", (ptr-n));

     return(n);
}

/*
 * Readfield reads data up to a tab, (like readline above)
 */

int 
readfield(fd, ptr, maxlen)
  int	fd;
  char	*ptr;
  int 	maxlen;
{
     int n;
     int rc;
     char c;
     
     for (n=1; n < maxlen; n++) {
	  if ( (rc = readrecvbuf(fd, &c, 1)) == 1) {
	       *ptr++ = c;
	       if (c == '\t') {
		    *(ptr - 1) = '\0';
		    break;
	       }
	  }
	  else if (rc == 0) {
	       if (n == 1)
		    return(0);	/* EOF, no data read */
	       else
		    break;		/* EOF, some data was read */
	  }
	  else
	       return(-1);		/* error */
     }
     *ptr = 0; 				/* Tack a NULL on the end */
     Debug("readfield: %s\r\n", (ptr-n));
     return(n);
}



int 
readword(fd, ptr, maxlen)
  int	fd;
  char	*ptr;
  int 	maxlen;
{
     int n;
     int rc;
     char c;
     
     for (n=1; n < maxlen; n++) {
	  if ( (rc = readrecvbuf(fd, &c, 1)) == 1) {
	       *ptr++ = c;
	       if (c == '\t'||c==' ') {
		    *(ptr - 1) = '\0';
		    break;
	       }
	  }
	  else if (rc == 0) {
	       if (n == 1)
		    return(0);	/* EOF, no data read */
	       else
		    break;		/* EOF, some data was read */
	  }
	  else
	       return(-1);		/* error */
     }
     
     *ptr = 0; 				/* Tack a NULL on the end */
     Debug("readword: %s\n", (ptr-n));
     return(n);
}



int 
readtotoken(fd, ptr, maxlen, zechar)
  int	fd;
  char	*ptr;
  int 	maxlen;
  char  zechar;
{
     int n;
     int rc;
     char c;
     
     for (n=1; n < maxlen; n++) {
	  if ( (rc = readrecvbuf(fd, &c, 1)) == 1) {
	       *ptr++ = c;
	       if (c == zechar) {
		    *(ptr - 1) = '\0';
		    break;
	       }
	  }
	  else if (rc == 0) {
	       if (n == 1)
		    return(0);	/* EOF, no data read */
	       else
		    break;		/* EOF, some data was read */
	  }
	  else
	       return(-1);		/* error */
     }
     
     *ptr = 0; 				/* Tack a NULL on the end */
     Debug("readtoken: %s\n", (ptr-n));
     return(n);
}


int
sreadword(input, output, maxlen)
  char *input;
  char *output;
  int maxlen;
{
     int n;
     char c;
     
     for (n=0; n < maxlen; n++) {
	  c = *input++;
	  *output++ = c;
	  if (isspace(c)) {
	       *(output - 1) = '\0';
	       break;
	  }
	  
	  if (c == '\0') {
	       break;
	  }
     }
     
     *output = '\0'; 				/* Tack a NULL on the end */
     return(n);
}


/*
 * ZapCRLF removes all carriage returns and linefeeds from a C-string.
 */

void
ZapCRLF(inputline)
  char *inputline;
{
     char *cp;

     cp = strchr(inputline, '\r');    /* Zap CR-LF */
     if (cp != NULL)
	  *cp = '\0';
     else {
	  cp = strchr(inputline, '\n');
	  if (cp != NULL)
	       *cp = '\0';
     }
}

/*
 *  Utilities for dealing with HTML junk
 */

static boolean acceptable[256];
static boolean acceptable_inited = FALSE;

void init_acceptable()
{
    unsigned int i;
    char * good = 
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./-_$";
    for(i=0; i<256; i++) acceptable[i] = FALSE;
    for(;*good; good++) acceptable[(unsigned int)*good] = TRUE;
    acceptable_inited = TRUE;
}

static char hex[17] = "0123456789abcdef";

char from_hex(c)
  char c;
{
     return (c>='0')&&(c<='9') ? c-'0'
	  : (c>='A')&&(c<='F') ? c-'A'+10
	       : (c>='a')&&(c<='f') ? c-'a'+10
		    :                      0;
}


/*
 * return a hex encoding of the char,
 */

char *to_hex(c)
  char c;
{
     static char out[4];

     out[0] = '\0';

     out[0]='%';
     out[1]=hex[c >> 4];
     out[2]=hex[c & 15];
     out[3]='\0';

     return(out);
}
     
/*
 * Replace hex escape sequences with the proper codes...
 *
 * input and output can be the same if you want
 */

void
Fromhexstr(input, output)
  char *input, *output;
{
     char c;
     unsigned int b;

     while (*input) {
	  if (*input == '%') {
	       input++;
	       c = *input++;
	       b = from_hex(c);
	       c = *input++;
	       if (!c) break;
	       *output++ = (b<<4) + from_hex(c);
	  }
	  else 
	       *output++ = *input++;
     }
     *output = '\0';
}
	  
void
Tohexstr(input, output)
  char *input, *output;
{

     if (acceptable_inited == FALSE)
	  init_acceptable();
     
     while (*input) {

	  if (acceptable[(int)*input] == TRUE) {
	       *output++ = *input++;
	  }
	  else {
	       *output++ = '%';
	       *output++ = hex[*input >> 4];
	       *output++ = hex[*input & 15];
	       input++;
	  }
     }

     *output = '\0';
}

/*
 * This fcn hexifies everything
 */

void
Hexall(input, output)
  char *input, *output;
{
     while (*input) {

	  *output++ = hex[*input >> 4];
	  *output++ = hex[*input & 15];
	  input++;
     }

     *output = '\0';
}


/*
 * String insensitive strstr
 */

char *
strcasestr(inputline, match)
  char *inputline;
  char *match;
{
     int matchlen=0;
     int i, inlen;

     matchlen = strlen(match);
     inlen = strlen(inputline);

     for(i=0; i<inlen; i++) {
	  if (strncasecmp(inputline+i, match, matchlen)==0)
	       return(inputline+i);
     }

     return(NULL);
}


/*
 * Iterate over a string, return a pointer to the next character
 * that isn't whitespace.
 */

char *
skip_whitespace(str)
  char *str;
{
     while (isspace(*str) && *str!='\0')
	  str++;

     return(str);
}



#if defined(VMS) && defined(NETLIB)

/*
 * netclose, netread, and netwrite for NETLIB
 */

#include iodef

static struct {
     short status;
     short size;
     long xxx;
} netlib_iosb;

void netlib_ast (param)
int param;
{
     SYS$SETEF (0);
}

int netclose (channel)
int channel;
{
     NET_DEASSIGN (&channel);

     return (0);
}

int netread (channel, buffer, length)
int channel;
char *buffer;
int length;
{
     struct {
          long len;
          char *adr;
     } buffer_desc;
     int status;

     if (length > 1500) length = 1500;
     buffer_desc.len = length;
     buffer_desc.adr = buffer;
     SYS$CLREF (0);
     status = TCP_RECEIVE (&channel, &buffer_desc, &netlib_iosb, netlib_ast, 0);
     if ((status & 1) == 0)
          return (-1);
     SYS$WAITFR (0);
     if ((netlib_iosb.status & 1) == 0) {
          return (-1);
     }
     return (netlib_iosb.size);
}

int netwrite (channel, buffer, length)
int channel;
char *buffer;
int length;
{
     struct {
          long len;
          char *adr;
     } buffer_desc;
     int status;

     buffer_desc.len = length;
     buffer_desc.adr = buffer;
     SYS$CLREF (0);
     status = TCP_SEND (&channel, &buffer_desc, 6, &netlib_iosb, netlib_ast, 0);
     if ((status & 1) == 0)
          return (-1);
     SYS$WAITFR (0);
     if ((netlib_iosb.status & 1) == 0) {
          return (-1);
     }
     return (netlib_iosb.size);
}

#endif	/* NETLIB */

#if defined(VMS) && defined(CMUIP)

/*
 * netclose, netread, and netwrite for CMUIP
 */

#include iodef

static struct {
     short status;
     short size;
     long xxx;
} cmu_iosb;

int netclose (channel)
  int channel;
{
     int status;

     status = SYS$QIOW (0, channel, IO$_DELETE, &cmu_iosb, 0, 0,
			0, 0, 0, 0, 0, 0);
     status = SYS$DASSGN (channel);

     return (0);
}

int netread (channel, buffer, length)
  int channel;
  char *buffer;
  int length;
{
     int status;

     if (length > 1500) length = 1500;
     status = SYS$QIOW (0, channel, IO$_READVBLK, &cmu_iosb, 0, 0,
			buffer, length, 0, 0, 0, 0);
     if ((status & 1) == 0)
          return (-1);
     if ((cmu_iosb.status & 1) == 0)
          return (-1);
     return (cmu_iosb.size);
}

int netwrite (channel, buffer, length)
  int channel;
  char *buffer;
  int length;
{
     int status;

     status = SYS$QIOW (0, channel, IO$_WRITEVBLK, &cmu_iosb, 0, 0,
			buffer, length, 0, 1, 0, 0);
     if ((status & 1) == 0)
          return (-1);
     if ((cmu_iosb.status & 1) == 0)
          return (-1);
     return (cmu_iosb.size);
}

#endif	/* CMUIP */



/*
 *  Checks to see if Remote server is up..
 */

boolean
RemoteIsUp(host, port)
  char *host;
  int  port;
{
     ;
     return(TRUE);
}



