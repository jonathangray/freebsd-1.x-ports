/********************************************************************
 * lindner
 * 3.5
 * 1993/06/22 05:53:17
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/compatible.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: compatible.c
 * Compatibility routines
 *********************************************************************
 * Revision History:
 * compatible.c,v
 * Revision 3.5  1993/06/22  05:53:17  lindner
 * Added getdtablesize() option
 *
 * Revision 3.4  1993/04/15  21:36:30  lindner
 * Emulation of geteuid calls for HPs
 *
 * Revision 3.3  1993/03/18  22:27:46  lindner
 * better portable tempnam()
 *
 * Revision 3.2  1993/02/19  21:33:27  lindner
 * Gopher1.2b2 release
 *
 * Revision 3.1.1.1  1993/02/11  18:03:05  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.4  1993/01/17  03:46:12  lindner
 * Fixed tempnam for VMS
 *
 * Revision 1.3  1993/01/08  23:13:55  lindner
 * Added more VMS mods from jqj
 *
 *
 *********************************************************************/


/*
 * Some functions that aren't implemented on every machine on the net
 *
 * definitions should be in the form "NO_FNNAME"
 * compatible.h looks at preprocessor symbols and automatically defines
 * many of the NO_FNNAME options
 *
 */

#include <string.h>
#include <stdio.h>
#include "Malloc.h"  /*** For NULL ***/
#include "compatible.h"

/*** For machines that don't have strstr ***/

#if defined(NOSTRSTR)

char *
strstr(host_name, cp)
  char *host_name;
  char *cp;
{
     int i, j;

     for (i = 0; i < strlen(host_name); i++) {
          j = strncmp(host_name+i, cp, strlen(cp));
          if (j == 0)
               return(host_name+i);
     }
     return(NULL);
}
#endif

#if defined(sequent)

#include <varargs.h>
vsprintf(va_alist)
  va_dcl
{
        ;
}

vfprintf(va_alist)
  va_dcl
{
        ;
}


#endif

#if defined(NO_TEMPNAM)
/* A tip of the hat to the developers of elm 2.4pl17, from whence 
   the non-VMS portion of this routine comes.  
*/
/* and a tempnam for temporary files */
static int cnt = 0;

char *tempnam(dir, pfx)
  char *dir;
  char *pfx;
{
#ifndef VMS
	char space[512];
	char *newspace;

	if (dir == NULL) {
		dir = "/usr/tmp";
	} else if (*dir == '\0') {
		dir = "/usr/tmp";
	}
	
	if (pfx == NULL) {
		pfx = "";
	}

	sprintf(space, "%s/%s%d.%d", dir, pfx, getpid(), cnt);
	cnt++;
	
	newspace = (char *)malloc(strlen(space) + 1);
	if (newspace != NULL) {
		strcpy(newspace, space);
	}
	return newspace;
#else
	char *tmpname;
	register int len;
        char tmpfilename[L_tmpnam];
 
        (void) tmpnam(tmpfilename);
	len = strlen(tmpfilename)+13;
	tmpname = (char *) malloc(sizeof(char)*len+1);
        sprintf(tmpname,"sys$scratch:%s.",tmpfilename);
        return(tmpname);
#endif
}
#endif

#if defined(NO_STRDUP)
char *strdup(str)
  char *str;
{
        int len;
        char *temp;

        if (str == NULL) return(NULL);
        len = strlen(str);

        temp = (char *) malloc(sizeof(char) * len + 1);

        strcpy(temp, str);
        return(temp);
}
#endif

#if defined(NO_TZSET)
void
tzset()
{
     ;
}
#endif

#if defined(NO_STRCASECMP)
/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Modified for use on VMS by Earl Fogel, University of Saskatchewan
 * Computing Services, January 1992
 */

typedef unsigned char u_char;

/*
 * This array is designed for mapping upper and lower case letter
 * together for a case independent comparison.  The mappings are
 * based upon ascii character sequences.
 */
static const u_char charmap[] = {
	'\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
	'\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
	'\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
	'\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
	'\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
	'\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
	'\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
	'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
	'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
	'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
	'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
	'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
	'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
	'\300', '\301', '\302', '\303', '\304', '\305', '\306', '\307',
	'\310', '\311', '\312', '\313', '\314', '\315', '\316', '\317',
	'\320', '\321', '\322', '\323', '\324', '\325', '\326', '\327',
	'\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

int
strcasecmp(s1, s2)
	const char *s1, *s2;
{
	register const u_char *cm = charmap,
			*us1 = (const u_char *)s1,
			*us2 = (const u_char *)s2;

	while (cm[*us1] == cm[*us2++])
		if (*us1++ == '\0')
			return (0);
	return (cm[*us1] - cm[*--us2]);
}

int
strncasecmp(s1, s2, n)
	const char *s1, *s2;
	register size_t n;
{
	if (n != 0) {
		register const u_char *cm = charmap,
				*us1 = (const u_char *)s1,
				*us2 = (const u_char *)s2;

		do {
			if (cm[*us1] != cm[*us2++])
				return (cm[*us1] - cm[*--us2]);
			if (*us1++ == '\0')
				break;
		} while (--n != 0);
	}
	return (0);
}

#endif

#if defined(NO_GETDTABLESIZE)
int getdtablesize()
{
     struct rlimit rlp;
     
     rlp.rlim_cur = rlp.rlim_max = RLIM_INFINITY;
     
     if (getrlimit( RLIMIT_NOFILE, &rlp ) )
	  return(-1);
     fds = rlp.rlim_cur;
}
#endif

#if defined(VMS)
/* In all versions of VMS, fopen() and open() are needlessly inefficient.
 * Define jacket routines to do file opens with more sensible parameters
 * than the VAXCRTL default.
 * [Should we really be doing this for EVERY fopen() and open()?]
 */
#ifdef fopen
#undef fopen
#endif
#ifdef open
#undef open
#endif

FILE *fopen_VMSopt ( name, mode )
    char *name, *mode;
{
    return fopen ( name, mode, "mbc=32" );
}

int open_VMSopt ( name, flags, mode )
    char *name;
    int flags;
    unsigned int mode;
{
    return  open ( name, flags, mode, "mbc=32");
}
#endif


