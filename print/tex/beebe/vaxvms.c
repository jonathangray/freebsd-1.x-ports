/***********************************************************************
This file provides alternative functions for several VMS VMS  C  library
routines which either unacceptable, or incorrect, implementations.  They
have  been developed and  tested under VMS Version  4.4, but indications
are  that they apply  to  earlier versions, back to 3.2  at least.  They
should be retested with each new release of VMS C.

Some of these (memxxx(), strxxx(),  and system()) are available with VMS
C 2.3 or later, but these versions should still work.

Contents:
	EXIT
	FSEEK
	FTELL
	GETCHAR
	GETENV
	READ
	UNGETC
	getjpi		-- system-service access
	getlogin
	memchr
	memcmp
	memcpy
	memmove
	memset
	qsort
	stricmp
	strtok
	strtol
	system
	tell
	unlink
	utime

The VAX VMS  file system record  structure has  unfortunate consequences
for random access files.

By default, text  files written by most system  utilities, and languages
other than C, have a variable  length record format,  in  which a 16-bit
character count is  aligned on an  even-byte boundary in the  disk block
b(always 512 bytes   in VMS, independent  of  record and  file  formats),
followed by  <count> bytes of data.   Binary files, such  as .EXE, .OBJ,
and  TeX .DVI  and font  files, all use a  512-byte  fixed record format
which  has no explicit  length  field.  No  file  byte count  is stored;
instead, the block count,  and the  offset of the  last data byte in the
last block are recorded in the file header  (do ``DUMP/HEADER filespec''
to see it).  For binary files with fixed-length  records, the last block
is normally  assumed to be  full,  and  consequently, file   transfer of
binary data from other machines  via Kermit, FTP, or DCL  COPY from ANSI
tapes, generally fails because  the input file length is  not a multiple
of 512.

This record organization may  be contrasted with  the STREAM, STREAM_LF,
and STREAM_CR organizations supported from Version 4.0; in  these,  disk
blocks contain a continuous byte stream in which nothing, or  LF, or CR,
is recognized as a record terminator.  These formats are similar to  the
Unix  and TOPS-20 file system  formats  which also use continuous   byte
streams.

For C, this  means that a  program operating on a file  in record format
cannot count input characters and expect that count to be the same value
as the  offset parameter passed  to fseek(),  which  numerous C programs
assume to  be the case.  The draft  ANSI C  standard,  and  Harbison and
Steele's ``C Reference Manual'', emphasize that only  values returned by
ftell() should be used as arguments to fseek(),  allowing the program to
return to  a position previously read or  written.  UNFORTUNATELY, VMS C
ftell()  DOES NOT  RETURN   A CORRECT  OFFSET VALUE FOR   RECORD  FILES.
Instead, for record files, it returns the byte  offset  of the start  of
the current record, no matter where in that  record the current position
may  be.   This  misbehavior  is  completely unnecessary,   since    the
replacements below perform correctly, and are written entirely in C.

Another problem is that ungetc(char c,  FILE*  fp) is unreliable.  VMS C
implements  characters  as  signed 8-bit integers  (so  do many other  C
implementations).  fgetc(FILE*  fp) returns an int,  not  a  char, whose
value is EOF (-1) in the event of end-of-file;  however, this value will
also  be returned for  a   character  0xFF, so  it  is essential  to use
feof(FILE  *fp) to test  for a  true end-of-file condition  when  EOF is
returned.   ungetc() checks the sign of  its argument c,  and  if it  is
negative (which it will be for 128 of the 256 signed  bytes), REFUSES TO
PUT IT BACK IN THE INPUT STREAM, on the assumption that c is really EOF.
This  too can  be fixed;   ungetc()  should only  do   nothing if feof()
indicates  a  true  end-of-file  condition.   The   overhead of  this is
trivial, since feof() is   actually implemented  as a macro   which does
nothing more than a logical AND and compare-with-zero.

getchar()  waits for a <CR> to  be typed when stdin is  a terminal;  the
replacement vms_getchar() remedies this.

Undoubtedly  other  deficiencies  in   VMS  C will   reveal  themselves.

VMS read() returns   only  a  single  disk   block on  each call.    Its
replacment, vms_read(), will  return  the  requested number of bytes, if
possible.

[29-Apr-87] Brendan Mackay (munnari!anucsd.oz!bdm@seismo.CSS.GOV)
This fix has been incorporated in vms_read() below.  Here are  Brendan's
comments:
>> The code for vms_read() has problems.  One is that you don't test for
>> end of file.  The other is that there is a bug in the C library which
>> prevents you  asking for  more than  65535 bytes  at a  time.  It  is
>> documented that no more  than 65535 bytes will  be returned, but  not
>> that you can't ask for more.  If you do, it reduces your request  mod
>> 65536!

There are also a  few Unix standard  functions which are  unimplemented.
qsort() is not provided.  getlogin()  and unlink() have VMS  equivalents
provided below.  tell() is considered obsolete, since its  functionality
is available from lseek(), but it is still seen in a few programs, so is
provided below.   getenv()  fails if  the  name contains  a  colon;  its
replacement allows the colon.

In the interest  of  minimal source perturbation,  replacements  for VMS
functions   are  given   the same  names,    but prefixed  "vms_".   For
readability,   the original names  are  preserved,  but are converted to
upper-case:

	#define FTELL vms_ftell
	#define FSEEK vms_fseek
	#define GETCHAR vms_getchar
	#define GETENV vms_getenv
	#define UNGETC vms_ungetc

These  are  only defined to work   correctly for fixed  length  512-byte
records, and no check is made that the file has that organization (it is
possible, but   not without  expensive calls to    fstat(), or access to
internal library structures).

[02-Apr-87]  --	Nelson   H.F.  Beebe,  University  of Utah  Center  for
		Scientific Computing
[13-Apr-88]  -- added memxxx(), strxxx(), fixed return code in system()
***********************************************************************/

#if 1				/* prior to VMS C 2.3 */
#define VOIDP	char*		/* char* prior to ANSI C */
#define const			/* const is a type modifier in ANSI C */
#else				/* VMS C 2.3 or later */
#define VOIDP	void*		/* char* prior to ANSI C */
#endif

#define EXIT	vms_exit
#define FTELL	vms_ftell
#define FSEEK	vms_fseek
#define GETENV	vms_getenv
#define GETCHAR vms_getchar
#define READ	vms_read
#define UNGETC	vms_ungetc

#include <stdio.h>

#if 0
#include <string.h>		/* stupid VMS gets type of memchr() wrong! */
#else
char*	strchr();
#endif

#include <types.h>
#include <ctype.h>
#include <errno.h>		/* need for utime() */
#include <stat.h>
#include <rms.h>		/* need for utime() */
#include <descrip.h>
#include <iodef.h>		/* need for vms_getchar() */
#include <ssdef.h>
#include <time.h>		/* need for utime() */

/**********************************************************************/
/*-->EXIT*/

void
vms_exit(code)
int code;
{
    switch (code)
    {
    case 0:
	exit(1);			/* success */
	break;

    default:
	exit((1 << 28) + 2);		/* error */
	break;				/* (suppresses %NONAME-E-NOMSG) */
    }
}


/**********************************************************************/
/*-->FSEEK*/

/* VMS fseek() and ftell() on fixed-length record files work correctly
only at block boundaries.  This replacement code patches in the offset
within	the  block.  Directions	 from	current	  position   and  from
end-of-file are converted to absolute positions, and then the code for
that case is invoked. */

long
FSEEK(fp,n,dir)
FILE *fp;
long n;
long dir;
{
    long k,m,pos,val,oldpos;
    struct stat buffer;

    for (;;)			/* loops only once or twice */
    {
      switch (dir)
      {
      case 0:			/* from BOF */
	  oldpos = FTELL(fp);	/* get current byte offset in file */
	  k = n & 511;		/* offset in 512-byte block */
	  m = n >> 9;		/* relative block number in file */
	  if (((*fp)->_cnt) && ((oldpos >> 9) == m)) /* still in same block */
	  {
	    val = 0;		/* success */
	    (*fp)->_ptr = ((*fp)->_base) + k; /* reset pointers to requested byte */
	    (*fp)->_cnt = 512 - k;
	  }
	  else
	  {
	    val = fseek(fp,m << 9,0); /* move to start of requested 512-byte block */
	    if (val == 0)	/* success */
	    {
	      (*fp)->_cnt = 0;	/* indicate empty buffer */
	      (void)fgetc(fp);	/* force refill of buffer */
	      (*fp)->_ptr = ((*fp)->_base) + k;	/* reset pointers to requested byte */
	      (*fp)->_cnt = 512 - k;
	    }
	  }
	  return(val);

      case 1:			/* from current pos */
	  pos = FTELL(fp);
	  if (pos == EOF)	/* then error */
	    return (EOF);
	  n += pos;
	  dir = 0;
	  break;		/* go do case 0 */

      case 2:			/* from EOF */
	  val = fstat(fileno(fp),&buffer);
	  if (val == EOF)	/* then error */
	    return (EOF);
	  n += buffer.st_size - 1; /* convert filesize to offset and */
				   /* add to requested offset */
	  dir = 0;
	  break;		/* go do case 0 */

      default:			/* illegal direction parameter */
	  return (EOF);
      }
    }
}

/**********************************************************************/
/*-->FTELL*/

/* With fixed-length record files, ftell() returns the offset of the
start of block.	 To get the true position, this must be biased by
the offset within the block. */

long
FTELL(fp)
FILE *fp;
{
    char c;
    long pos;
    long val;
    if ((*fp)->_cnt == 0)	/* buffer empty--force refill */
    {
	c = fgetc(fp);
	val = UNGETC(c,fp);
	if (val != c)
	    return (EOF);	/* should never happen */
    }
    pos = ftell(fp);		/* this returns multiple of 512 (start of block) */
    if (pos >= 0)		/* then success--patch in offset in block */
      pos += ((*fp)->_ptr) - ((*fp)->_base);
    return (pos);
}

/**********************************************************************/
/*-->GETCHAR*/

static int tt_channel = -1;	/* terminal channel for image QIO's */

#define FAILED(status) (~(status) & 1) /* failure if LSB is 0 */

int
GETCHAR()
{
    int ret_char;		/* character returned */
    int status;			/* system service status */
    static $DESCRIPTOR(sys_in,"TT:");

    if (tt_channel == -1)	/* then first call--assign channel */
    {
	status = sys$assign(&sys_in,&tt_channel,0,0);
	if (FAILED(status))
	    lib$stop(status);
    }
    ret_char = 0;
    status = sys$qiow(0,tt_channel,IO$_TTYREADALL | IO$M_NOECHO,0,0,0,
	&ret_char,1,0,0,0,0);
    if (FAILED(status))
        lib$stop(status);

    return (ret_char);
}


/**********************************************************************/
/*-->memchr*/

/* This is a simple implementation of memchr(), which searches for the
first occurrence of a byte in the first n bytes of a byte string.  A
library version should use hardware moves, or unrolled loops, or other
tricks for greater efficiency. */

VOIDP
memchr(s,c,n)
const VOIDP s;
int c;
size_t n;
{
    for (; n > 0; s++,--n)
	if ((unsigned char)(*s) == (unsigned char)c)
	    return ((VOIDP)s);
    return ((VOIDP)NULL);
}


/**********************************************************************/
/*-->memcmp*/

/* This is a simple implementation of memcmp(), which compares two
objects byte by byte, stopping after n bytes.  A library version
should use hardware moves, or unrolled loops, or other tricks for
greater efficiency. */

int
memcmp(s1,s2,n)
const VOIDP s1;
const VOIDP s2;
size_t n;
{
	VOIDP org_s1 = s1;
	VOIDP org_s2 = s2;

	for (; n > 0; --n, s1++, s2++)
	{
		if (*s1 < *s2)
			return (-(int)(s2 - org_s2));
		else if (*s1 > *s2)
			return ((int)(s2 - org_s2));		
	}
	return (0);
}




/**********************************************************************/
/*-->memcpy*/

/* This is a simple implementation of memcpy(), which copies source
to target with undefined behavior in the event of overlap.  This
particular implementation copies from first to last byte, in order. */

VOIDP
memcpy(t,s,n)
VOIDP t;	/* target */
const VOIDP s;	/* source */
size_t n;
{
    for (; n > 0; --n)
        *t++ = *s++;	/* copy in forward order */
}

/**********************************************************************/
/*-->memmove*/

/* This is a simple implementation of memmove(), which copies as if the
source were first completely copied to a temporary area, then that
area were copied to the target.    A library version should
use hardware moves, or unrolled loops, or other tricks for greater
efficiency. */

VOIDP
memmove(t,s,n)
VOIDP t;	/* target */
const VOIDP s;	/* source */
size_t n;
{
	if ((s < t) && ((s + n) > t))	/* source overlaps target from below */
		for (s += n, t += n; n > 0; --n)
			*t-- = *s--;	/* copy in reverse order */
	else
		for (; n > 0; --n)
			*t++ = *s++;	/* copy in forward order */
}

/**********************************************************************/
/*-->memset*/

/* This is a simple implementation of memset().   A library version should
use hardware moves, or unrolled loops, or other tricks for greater
efficiency. */


VOIDP
memset(s,ch,n)
VOIDP s;	/* target */
int ch;		/* fill character (treated as unsigned char) */
size_t n;	/* fill count */
{
	for (; n > 0; --n)
		*s++ = (unsigned char)ch;
}

/**********************************************************************/
/*-->READ*/
int
READ(file_desc,buffer,nbytes)
register int file_desc;
register char *buffer;
register int nbytes;
{
    register int ngot;
    register int left;

    for (left = nbytes; left > 0; /* NOOP */)
    {
        ngot = read(file_desc,buffer,(left > 65024 ? 65024 : left));
        if (ngot < 0)
            return (-1);        /* error occurred */
        if (ngot == 0)          /* eof occurred */
            return(nbytes-left);
        buffer += ngot;
        left -= ngot;
    }
    return(nbytes-left);
}


/**********************************************************************/
/*-->UNGETC*/
long
UNGETC(c,fp)	/* VMS ungetc() is a no-op if c < 0 (which is half the time!) */
char c;
FILE *fp;
{

    if ((c == EOF) && feof(fp))
	return (EOF);		/* do nothing at true end-of-file */
    else if ((*fp)->_cnt >= 512)/* buffer full--no fgetc() done in this block!*/
	return (EOF);		/* must be user error if this happens */
    else			/* put the character back in the buffer */
    {
      (*fp)->_cnt++;		/* increase count of characters left */
      (*fp)->_ptr--;		/* backup pointer to next available char */
      *((*fp)->_ptr) = c;	/* save the character */
      return (c);		/* and return it */
    }
}

/**********************************************************************/
/*-->getenv*/
char*
GETENV(name)
char* name;
{
    char* p;
    char* result;
    char ucname[256];

    p = ucname;
    while (*name)	/* VMS logical names must be upper-case */
    {
      *p++ = islower(*name) ? toupper(*name) : *name;
      ++name;
    }
    *p = '\0';

    p = strchr(ucname,':');		/* colon in name? */
    if (p == (char *)NULL)		/* no colon in name */
        result = getenv(ucname);
    else				/* try with and without colon */
    {
	result = getenv(ucname);
	if (result == (char *)NULL)
	{
	    *p = '\0';
	    result = getenv(ucname);
	    *p = ':';
	}
    }
    return (result);
}

/**********************************************************************/
/*-->getjpi*/

/***********************************************************************
Return  a system job/process value  obtained   from the VMS  system call
LIB$GETJPI.   This call   can return  either 32-bit   integer values, or
strings.  The  obtained value is stored in  an  internal  static  buffer
which is overwritten on subsequent calls.

The function return is a (char*) pointer to  that buffer, which must  be
coerced to  (long*) if an integer value is  obtained.  String values are
guaranteed to be NUL terminated, with no trailing blanks.

The argument, jpi_code, is one of the values defined in <jpidef.h>.

In the event of an error return from LIB$GETJPI, (char*)NULL is returned
instead.

[30-Oct-87]
***********************************************************************/

#define LIB$_INVARG 0x158234		/* not defined in standard .h files */

int lib$getjpi();

char*
getjpi(jpi_code)
int jpi_code;				/* values defined in <jpidef.h> */
{
    short retlen = 0;
    long retval;
    static char buffer[256];		/* space for up to 255-char results */
    static $DESCRIPTOR(strdes,buffer);

    strdes.dsc$w_length = sizeof(buffer)-1; /* $DESCRIPTOR doesn't set this */

    /* lib$getjpi() will normally return a string representation.
       Try first to get the integer representation, then if an invalid
       argument is signalled, get the string representation. */

    retval = lib$getjpi(&jpi_code,0L,0L,&buffer);
    if (retval == LIB$_INVARG)
    {
        retval = lib$getjpi(&jpi_code,0L,0L,&buffer,&strdes,&retlen);
        buffer[retlen] = '\0';		/* terminate any string value */
	while ((retlen > 0) && (buffer[--retlen] == ' '))
	    buffer[retlen] = '\0';
    }

    return ((retval == SS$_NORMAL) ? (char*)(&buffer) : (char*)NULL);
}

/**********************************************************************/
/*-->getlogin*/
char*
getlogin()
{
    return ((char *)getenv("USER")); /* use equivalent VMS routine */
}

/**********************************************************************/
/*-->qsort*/

/***********************************************************************
TeXindex uses  the standard  Unix  library function  qsort()  for
record sorting.  Unfortunately, qsort()  is not a stable  sorting
algorithm, so input order is not necessarily preserved for  equal
sort  keys.    This  is   important,  because   the  sorting   is
case-independent, while  the  actual  entries may  not  be.   For
example, the input

\entry{i}{22}{{\CODE{i}}}
\entry{i}{42}{{\CODE{i}}}
\entry{I}{41}{{\CODE{I}}}
\entry{I}{42}{{\CODE{I}}}

produces

\initial {I}
\entry {{\CODE{i}}}{22}
\entry {{\CODE{I}}}{41--42}
\entry {{\CODE{i}}}{42}

instead of the correct

\initial {I}
\entry {{\CODE{i}}}{22, 42}
\entry {{\CODE{I}}}{41--42}

We  therefore  provide  this  stable  shellsort  replacement  for
qsort() based  on the  code  given on  p.  116 of  Kernighan  and
Ritchie, ``The  C Programming  Language'', Prentice-Hall  (1978).
This has  order  N**1.5  average performance,  which  is  usually
slower than qsort().  In the interests of simplicity, we make  no
attempt to handle short sequences by alternative methods.

[07-Nov-86]
***********************************************************************/


#define BASE(i) &base[(i)*width]

void
qsort(base, nel, width, compar)
    char base[];	/* start of data in memory */
    int nel;		/* number of elements to be sorted */
    int width;		/* size (in bytes) of each element */
    int (*compar)();	/* comparison function */
{
    int gap;
    int i;
    int j;

    register int k;	/* inner exchange loop parameters */
    register char* p;
    register char* q;
    register char  c;

    for (gap = nel/2; gap > 0; gap /= 2)
    {
	for (i = gap; i < nel; i++)
	{
	    for (j = i-gap; j >= 0; j -= gap)
	    {
	        p = BASE(j);
		q = BASE(j+gap);
		if ((*compar)(p,q) <= 0)
		    break;	/* exit j loop */
		else
		{
		    for (k = 0; k < width; (++p, ++q, ++k))
		    {
			c = *q;
			*q = *p;
			*p = c;
		    }
		}
	    }
	}
    }
}

/**********************************************************************/
/*-->stricmp*/

/* This is a simple implementation of stricmp().  A library version
should use hardware support, or unrolled loops, to improve
performance. */

int
stricmp(s1,s2)				/* compare strings ignoring case */
const char* s1;
const char* s2;
{
	int c1;
	int c2;
	char* org_s1 = s1;

	for (; *s1 && *s2; ++s1, ++s2)
	{
		c1 = 0xff & (int)(islower(*s1) ? *s1 : tolower(*s1));
		c2 = 0xff & (int)(islower(*s2) ? *s2 : tolower(*s2));
		if (c1 < c2)
			return (-(int)(s1 - org_s1));
		else if (c1 > c2)
			return ((int)(s1 - org_s1));
	}
	if (*s1 == '\0')
		return ((*s2 == '\0') ? 0 : -(int)(s1 - org_s1));
	else /* (*s2 == '\0') */
		return ((int)(s1 - org_s1));
}

/**********************************************************************/
/*-->strtok*/

/* This is a simple implementation of strtok().	 A library version
should use hardware support, or unrolled loops, to improve
performance. */

char*
strtok(s,sepchars)
char* s;		/* non-NULL on first call, NULL on subsequent ones */
const char* sepchars;	/* separator characters */
{
	static char* s_init = (char*)NULL;
	char* start;
	char* end;

	if (s != (char*)NULL)		/* initial call for this s[] */
		s_init = s;
	else if ((s_init == (char*)NULL) || (*s_init == '\0'))
		return ((char*)NULL);

	/* resume scan from saved token start */

	for (start = s_init; *start && strchr(sepchars,*start) != (char*)NULL;
		start++)
		/* skip over leading separators */;
	if (*start == '\0')
		return ((char*)NULL);
	for (end = start; *end && strchr(sepchars,*end) == (char*)NULL; end++)
		/* skip over token */;
	s_init = *end ? end+1 : end;	/* next token starts past this */
					/* but not beyond end of string */
	*end = '\0';			/* terminate current token */
	return (start);			/* and return pointer to it */
}

/**********************************************************************/
/*-->strtol*/

#define IN(l,a,r) (((l) <= (a)) && ((a) <= (r)))


/* This is a simple implementation of ANSI strtol().  A library version
should be programmed with more care. */

long
strtol(nptr,endptr,base)
const char* nptr;
char** endptr;
int base;
{
	char* q;		/* pointer past end of digits */
	int c;			/* current character value */
	char* pos;		/* pointer into digit list */
	int negative;		/* 0 for positive, non-0 for negative */
	int digit;		/* digit value */
	long number;		/* the accumulating number */
	static char* digits = "0123456789abcdefghijklmnopqrstuvxwyz";


	if (!(IN(2,base,36) || (base == 0)))
	{
		if (endptr != (char**)NULL)
			*endptr = (char*)nptr;
		return (0L);
	}


	while (isspace(*nptr))
		nptr++;		/* ignore leading whitespace */

	switch (*nptr)		/* set number sign */
	{
	case '-':
		negative = -1;
		nptr++;
		break;

	case '+':
		negative = 0;
		nptr++;
		break;

	default:
		negative = 0;
		break;
	}

	q = (char*)nptr;
	if (base == 0)		/* variable base; set by lookahead */
	{
		if (*q == '0')
			base = ((*(q+1) == 'x') || (*(q+1) == 'X')) ? 16 : 8;
		else
			base = 10;
	}

	/* eliminate optional "0x" or "0X" prefix */

	if (	(base == 16) &&
		(*q == '0') &&
		((*(q+1) == 'x') || (*(q+1) == 'X')) )
		q += 2;

	number = 0L;

	/* Number conversion is done by shifting rather than multiplication
	   when the base is a power of 2, in order that the results not be
	   impacted by integer overflow. */
	switch (base)
	{
	case 2:
		while (IN('0',*q,'1'))
		{
			number <<= 1;
			number |= *q - '0';
			q++;
		}
		break;

	case 4:
		while (IN('0',*q,'3'))
		{
			number <<= 2;
			number |= *q - '0';
			q++;
		}
		break;


	case 8:
		while (IN('0',*q,'7'))
		{
			number <<= 3;
			number |= *q - '0';
			q++;
		}
		break;


	case 16:
		for (;;)
		{
			if (!*q)
				break;
			c = (unsigned)*q;
			if (isupper(c))
				c = tolower(c);
			pos = strchr(digits,c);
			if (pos == (char*)NULL)
				break;
			digit = pos - digits;
			if (!IN(0,digit,15))
				break;
			number <<= 4;
			number |= digit;
			q++;
		}
		break;


	case 32:
		for (;;)
		{
			if (!*q)
				break;
			c = (unsigned)*q;
			if (isupper(c))
				c = tolower(c);
			pos = strchr(digits,c);
			if (pos == (char*)NULL)
				break;
			digit = pos - digits;
			if (!IN(0,digit,31))
				break;
			number <<= 5;
			number |= digit;
			q++;
		}
		break;

	default:		/* all other bases done by multiplication */
		for (;;)	/* accumulate negative so most negative */
		{		/* number on two's-complement is handled */
			if (!*q)
				break;
			c = (unsigned)*q;
			if (isupper(c))
				c = tolower(c);
			pos = strchr(digits,c);
			if (pos == (char*)NULL)
				break;
			digit = pos - digits;
			if (!IN(0,digit,base-1))
				break;
			number *= base;
			number -= digit;
			q++;
		}
		if (endptr != (char**)NULL)
			*endptr = q;
		if (negative)
			return(number);
		number = -number;
		break;
	}
	if (negative)
		number = -number;
	if (endptr != (char**)NULL)
		*endptr = q;
	return (number);
}
/**********************************************************************/
/*-->system*/
int
system(s)
char *s;
{
	struct	dsc$descriptor t;
	int stat;

	t.dsc$w_length = strlen(s);
	t.dsc$a_pointer = s;
	t.dsc$b_class = DSC$K_CLASS_S;
	t.dsc$b_dtype = DSC$K_DTYPE_T;

	/* The 3 low-order bits of stat are 
	   	0 (warning),
		1 (success),
		2 (error),
		3 (information), or
		4 (severe or fatal error)
	   Consider values of 0, 1, or 3 to be success.  LIB$SPAWN
	   will usually return SS$_NORMAL, independent of the value
	   of stat.  
	*/

	if (LIB$SPAWN(&t,0,0,0,0,0,&stat) != SS$_NORMAL)
	    return (127);
	switch (stat & 7)
	{
	case 0:
	case 1:
	case 3:
	    return (0);
	default:
	    return (127);
	}
}


/**********************************************************************/
/*-->tell*/
long
tell(handle)
int handle;
{
    return (lseek(handle,0L,1));
}

/**********************************************************************/
/*-->unlink*/
int
unlink(filename)
char *filename;
{
	return (delete(filename)); /* use equivalent VMS routine */
}

/**********************************************************************/
/*-->utime*/

/* utime(path,times) sets the access and modification times of the 
   file 'path' to the Unix binary time values, 'times'.  Return 0
   on success, and -1 on error (setting errno as well). */

utime(path,times)		/* VAX VMS C version */
char* path;
time_t times[2];
{
    int status;
    struct dsc$descriptor_s time_desc;
    char *ftime = "23-OCT-1907 12:34:56";
    struct tm *timeval;
    static char* months[] = {"JAN","FEB","MAR","APR","MAY","JUN",
			     "JUL","AUG","SEP","OCT","NOV","DEC"};
    struct FAB fab1;
    struct XABRDT xab1;

    /* Zero FAB and XAB structures */
    (void)memset(&fab1,'\0',sizeof(fab1));
    (void)memset(&xab1,'\0',sizeof(xab1));

    /* Convert Unix binary time to ASCII string for 
       sys$bintime().  We use localtime() instead of ctime(),
       because although ctime() is simpler, it drops the seconds
       field, which we would rather preserve.  */

    timeval = (struct tm*)localtime(&times[0]);
    sprintf(ftime,"%02d-%3s-19%02d %02d:%02d:%02d",
	timeval->tm_mday,
	months[timeval->tm_mon],
	timeval->tm_year,
	timeval->tm_hour,
	timeval->tm_min,
	timeval->tm_sec);

    /* Setup fab1 and rab fields. */
    fab1.fab$b_bid = FAB$C_BID;
    fab1.fab$b_bln = FAB$C_BLN;
    fab1.fab$l_fop = FAB$V_UFO;
    fab1.fab$b_fac = FAB$V_GET;
    fab1.fab$l_fna = path;
    fab1.fab$b_fns = strlen(path);
    fab1.fab$l_xab = &xab1;

    xab1.xab$b_bln = XAB$C_RDTLEN;
    xab1.xab$b_cod = XAB$C_RDT;
    xab1.xab$l_nxt = (char*)NULL;

    /* Open the file */
    status = sys$open(&fab1);
    if (status != RMS$_NORMAL)
    {
      errno = ENOENT;
      return (-1);
    }

    /* Convert the time string to a VMS binary time value in the XAB */
    time_desc.dsc$w_length = strlen(ftime);
    time_desc.dsc$a_pointer = ftime;
    time_desc.dsc$b_class = DSC$K_CLASS_S;
    time_desc.dsc$b_dtype = DSC$K_DTYPE_T;
    status = sys$bintim(&time_desc,&xab1.xab$q_rdt);
    if (status != SS$_NORMAL)
    {
      status = sys$close(&fab1);
      errno = EFAULT;
      return (-1);
    }

    /* Close the file, updating the revision date/time value */
    status = sys$close(&fab1);
    if (status != RMS$_NORMAL)
    {
      errno = EACCES;
      return (-1);
    }
    return (0);
}
