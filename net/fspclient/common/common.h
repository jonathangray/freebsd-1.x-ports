    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <errno.h>
#include <fcntl.h>
#include <signal.h>

#include <sys/socket.h>
#include <netinet/in.h>

#if !FD_SET_IN_SYS_TYPES_H && !FD_SET_IN_SYS_TIME_H
#  if FD_SET_IN_SYS_SELECT_H
#    include <sys/select.h>
#  else
#    if FD_SET_IN_SYS_INET_H
#      include <sys/inet.h>
#    else
typedef long fd_set;
#      undef FD_SET
#      define FD_SET(n,p) (*(p) |= (1 << (n)))
#      undef FD_CLR
#      define FD_CLR(n,p) (*(p) &= ~(1 << (n)))
#      undef FD_ISSET
#      define FD_ISSET(n,p) (*(p) & (1 << (n)))
#      undef FD_ZERO
#      define FD_ZERO(p) (*(p) = 0)
#    endif
#  endif
#endif

#ifdef HAVE_VFORK_H
#  include <vfork.h>
#endif

#if defined(DIRENT)
#  include <dirent.h>
#else /* !DIRENT */
#  define dirent direct
#  ifdef SYSNDIR
#    include <sys/ndir.h>
#  endif /* SYSNDIR */
#  ifdef SYSDIR
#    include <sys/dir.h>
#  endif /* SYSDIR */
#  ifdef NDIR
#    include <ndir.h>
#  endif /* NDIR */
#endif /* not DIRENT */

#if STDC_HEADERS || HAVE_STRING_H
#  include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#  if !STDC_HEADERS && HAVE_MEMORY_H
#    include <memory.h>
#  endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#  define bcopy(s, d, n) memcpy ((d), (s), (n))
#  define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#  define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#  include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#ifndef S_ISREG
#  define S_ISREG(mode) (((mode) & S_IFREG) != 0)
#endif

#ifndef S_ISDIR
#  define S_ISDIR(mode) (((mode) & S_IFDIR) != 0)
#endif

/* definitions missing from many system headers */
extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

#if !HAVE_STRDUP
#ifndef ANSI_PROTOTYPES
extern char *strdup();

#else /* ANSI_PROTOTYPES */

extern char *strdup(const char *str);
#endif /* ANSI_PROTOTYPES */
#endif

/* externs from udp_io.c */
#ifndef ANSI_PROTOTYPES
extern int _x_udp();
extern int _x_adr();
extern int _x_select();

#else /* ANSI_PROTOTYPES */

extern int _x_udp(int *port);
extern int _x_adr(char *host, int port, struct sockaddr_in *his);
extern int _x_select(fd_set *rf, long tt);
#endif /* ANSI_PROTOTYPES */

/****************************************************************************
*  UBUF is the structure of message exchanged between server and clients. 
*
*    The 'buf' part of the buffer is variable length up to max of 1024.
*    The 'key' field is used by the server for sequence identification.
*    The 'seq' field is used by the client for sequence identification.
*
*  Client's message to server contain a key value that is the same as the
*  key value of the previous message received from the server.  Similarly,
*  the server's message to client contains a seq value that is the same
*  as the seq value of the previous message from the client. 
*
*  The buf field is logically partitioned into two parts by the len field.
*  The len field indicate the size of the first part of the buffer starting
*  at buf[0].  The rest of the buffer is the second field.  In some cases
*  both fields can contain information.
*
****************************************************************************/

#define UBUF_HSIZE 12                           /* 12 bytes for the header */
#define UBUF_SPACE 1024			        /* maximum payload.        */

typedef struct UBUF {            char   cmd;  /* message code.             */
                        unsigned char   sum;  /* message checksum.         */
                        unsigned short  key;  /* message key.              */
                        unsigned short  seq;  /* message sequence number.  */
                        unsigned short  len;  /* number of bytes in buf 1. */
                        unsigned long   pos;  /* location in the file.     */

                        char   buf[UBUF_SPACE];
                    } UBUF;

/* definition of cmd */

#define CC_VERSION	0x10	/* return server's version string.	*/
#define CC_ERR          0x40    /* error response from server.          */
#define CC_GET_DIR      0x41    /* get a directory listing.             */
#define CC_GET_FILE     0x42    /* get a file.                          */
#define CC_UP_LOAD      0x43    /* open a file for writing.             */
#define CC_INSTALL      0x44    /* close a file opened for writing.     */
#define CC_DEL_FILE     0x45    /* delete a file.                       */
#define CC_DEL_DIR      0x46    /* delete a directory.                  */
#define CC_GET_PRO      0x47    /* get directory protection.            */
#define CC_SET_PRO      0x48    /* set directory protection.            */
#define CC_MAKE_DIR     0x49    /* create a directory.                  */
#define CC_BYE          0x4A    /* finish a session.                    */
#define CC_GRAB_FILE	0x4B	/* atomic get+delete a file             */
#define CC_GRAB_DONE	0x4C	/* atomic get+delete a file done        */
#define CC_LIMIT        0x80    /* # > 0x7f for future cntrl blk ext.   */

/* definition of global bitfield for version information */
/* Global information is also going to be a bit vector   */
#define VER_BYTES       1       /* currently only 8 bits or less of info  */
#define VER_LOG         0x01    /* does the server do logging             */
#define VER_READONLY    0x02    /* is the server in readonly mode         */
#define VER_REVNAME     0x04    /* does the server refuse non reversables */
#define VER_PRIVMODE    0x08    /* Is the server being run 'private' mode */
#define VER_THRUPUT     0x10    /* does the server enforce thruput control*/

/* definition of directory bitfield for directory information */
/* directory information is just going to be a bitfield encoding
 * of which protection bits are set/unset
 */
#define PRO_BYTES       1       /* currently only 8 bits or less of info */
#define DIR_OWNER       0x01    /* does caller own directory             */
#define DIR_DEL         0x02    /* can files be deleted from this dir    */
#define DIR_ADD         0x04    /* can files be added to this dir        */
#define DIR_MKDIR       0x08    /* can new subdirectories be created     */
#define DIR_PRIV        0x10    /* are files readable by non-owners      */

/****************************************************************************
*  RDIRENT is the structure of a directory entry contained in a .FSP_CONTENT
*  file.  Each entry contains a 4 bytes quantity 'time', a 4 bytes quantity
*  'size', and 1 byte of 'type'.  Then followed by x number of bytes of
*  'name'.  'name' is null terminated.  Then followed by enough number of
*  padding to fill to an 4-byte boundary.  At this point, if the next entry
*  to follow will spread across 1k boundary, then two possible things will
*  happen.  1) if the header fits between this entry and the 1k boundary,
*  a complete header will be filled in with a 'type' set to RDTYPE_SKIP.
*  And then enough bytes to padd to 1k boundary.  2) if the header does
*  not fit, then simply pad to the 1k boundary.  This will make sure that
*  messages carrying directory information carry only complete directory
*  entries and no fragmented entries.  The last entry is type RDTYPE_END.
****************************************************************************/

#define RDHSIZE (2*sizeof(unsigned long)+sizeof(unsigned char))

typedef struct RDIRENT { unsigned long  time;
                         unsigned long  size;
                         unsigned char  type;
                         char        name[1]; } RDIRENT;

#define RDTYPE_END      0x00
#define RDTYPE_FILE     0x01
#define RDTYPE_DIR      0x02
#define RDTYPE_SKIP     0x2A

#define NULLP ((char *) 0)
