char *help[] = {
  "       'IO Zone' Benchmark Program",
  " ",
  "       Author: Bill Norcott (norcott_bill@tandem.com)",
  "               1060 Hyde Avenue",
  "               San Jose, CA  95129",
  " ",
  "  Copyright 1991, 1992   William D. Norcott",
  " ",
  "  License to freely use and distribute this software is hereby granted ",
  "  by the author, subject to the condition that this copyright notice ",
  "  remains intact.  The author retains the exclusive right to publish ",
  "  derivative works based on this work, including, but not limited to, ",
  "  revised versions of this work",
  " ",
  "  This test writes a X MEGABYTE sequential file in Y byte chunks, then",
  "  rewinds it  and reads it back.  [The size of the file should be",
  "  big enough to factor out the effect of any disk cache.].  Finally,",
  "  IOZONE deletes the temporary file",
  "        ",
  "  The file is written (filling any cache buffers), and then read.  If the",
  "  cache is >= X MB, then most if not all the reads will be satisfied from",
  "  the cache.  However, if it is less than or equal to .5X MB, then NONE of",
  "  the reads will be satisfied from the cache.  This is becase after the ",
  "  file is written, a .5X MB cache will contain the upper .5 MB of the test",
  "  file, but we will start reading from the beginning of the file (data",
  "  which is no longer in the cache)",
  "        ",
  "  In order for this to be a fair test, the length of the test file must",
  "  be AT LEAST 2X the amount of disk cache memory for your system.  If",
  "  not, you are really testing the speed at which your CPU can read blocks",
  "  out of the cache (not a fair test)",
  "        ",
  "  IOZONE does not normally test the raw I/O speed of your disk or system.",
  "  It tests the speed of sequential I/O to actual files.  Therefore, this",
  "  measurement factors in the efficiency of you machines file system,",
  "  operating system, C compiler, and C runtime library.  It produces a ",
  "  measurement which is the number of bytes per second that your system",
  "  can read or write to a file.  ",
  " ",
  "  You use IOZONE to test the I/O speed of a UNIX 'RAW DEVICE' such",
  "  as a tape drive, hard disk drive, floppy disk drive, etc.  To do this,",
  "  you must define the symbol NO_DELETE when you compile IOZONE.  If you",   
  "  fail to define NO_DELETE, IOZONE will treat the raw device as a ",
  "  temporary file, and WILL DELETE THE RAW DEVICE after the test completes!",
  "  When testing raw devices, any UNIX buffer caching is bypassed.  IOZONE",
  "  still is using the read()/write() system calls, so you are not quite",
  "  testing the device at the low level of say, disk controller diagnostics.",
  "  On the other hand, that kind of testing is highly system- and device-",
  "  specific, and my goal for IOZONE has been to build a highly portable",
  "  benchmark -- not one which is tied to a particular operating system or",
  "  hardware configuration.  In practice, I have tested raw disk and tape",
  "  peripherals and the results are very close to the manufacturer's specs",
  "  for those devices.", 
  " ",
  "  For V1.06, IOZONE adds the 'auto test' feature.  This is activated",
  "  by the command:  'iozone auto' .  The auto test runs IOZONE repeatedly  ",
  "  using record sizes from 512 to 8192 bytes, and file sizes from 1 to 16",
  "  megabytes.  It creates a table of results.",
  "        ",
  "  For V1.06, IOZONE lets you specify the number of file system sizes and      ",
  "  record lengths to test when using auto mode.  Define the constants",
  "  MEGABYTES_ITER_LIMIT and RECLEN_ITER_LIMIT as seen below      ",
  "        ",
  "  For V1.09 you can show the development help by typing 'iozone help'",
  "        ",
  "  For V1.10 IOzone traps SIGINT (user interrupt) and SIGTERM",
  "  (kill from shell) signals and deletes the temporary file",
  "        ",
  "  For V1.11 IOzone requires no compilation flags for AIX",
  "  Also, come miscellaneous cleanups have been made to the source",
  "        ",
  "  For V1.12 IOzone support has been added for the MIPS RISCos,",
  "  Tandem Non-StopUX, and Tandem GUARDIAN 90 operating systems.",
  "  IOzone is now a 'Conforming POSIX.1 Application'  (IEEE Std 1003.1-1988)",
  "        ",
  "  For V1.14 IOzone supports Next and QNX systems.  It also prints out", 
  "  the name of the operating system when run.  There is now the option",
  "  to force IOzone to flush all writes to disk via fsync()",
  "  Defining USE_FSYNC will make IOzone include in its measurements the time",
  "  it takes to actually write the data onto disk, as opposed to",
  "  just writing into the system cache.  BSD UNIX and SVR4 support fsync(),",
  "  but SVR3 and generic POSIX systems do not.  I have enabled USE_FSYNC",
  "  for the systems which support it",
  "        ",
  "  For V1.14, we now officially support AT&T SVR4.  It has worked just",
  "  fine using SVR4 with previous versions of IOzone.  Also, for systems",
  "  which use the times() function, we calculate the 'base time' the first",
  "  time we ever call time_so_far(), then subtract this time from all",
  "  future measurements.  This increases the precision of our measurement",
  "  and fixes a loss-of-precision problem which occurred on some systems",
  "        ",
  "  For V1.15, add the NO_DELETE symbol.  If you define NO_DELETE during",
  "  the compilation (e.g., for UNIX systems compile with cc -DNO_DELETE),",
  "  IOzone will not delete the 'temporary' file which it reads & writes.",
  "  This is REQUIRED when testing RAW DEVICES such as disks and tape drives!",
  "        ",
  " ",
  "  This program has been ported and tested on the following computer",
  "  operating systems:",
  " ",
  "    Vendor             Operating System    Notes on compiling IOzone",
  "    -------------------------------------------------------------------------",
  "    Apollo		  Domain/OS           no cc switches -- BSD domain", 
  "    AT&T               UNIX System V Release 4",
  "    AT&T 6386WGS       AT&T UNIX 5.3.2     can't get it to compile with cc",
  "					      It should work with gcc via:",
  "					      'gcc -ansi -o iozone iozone.c'",
  "    Generic AT&T       UNIX System V R3    may need cc -DSVR3",
  "    Convergent         Unisys/AT&T Sys5r3  cc -DCONVERGENT -o iozone iozone.c",
  "    Digital Equipment  ULTRIX V4.1 ",
  "    Digital Equipment  VAX/VMS V5.4        see below **         ",
  "    Digital Equipment  VAX/VMS (POSIX) ",
  "    Hewlett-Packard    HP-UX 7.05",
  "    IBM                AIX Ver. 3 rel. 1",
  "    Interactive        UNIX System V R3    ",
  "    Microsoft          MS-DOS 3.3          tested Borland, Microsoft C",
  "    MIPS               RISCos 4.52",
  "    NeXt               NeXt OS 2.x",
  "    OSF                OSF/1",
  "    Portable!          POSIX 1003.1-1988   may need to -D_POSIX_SOURCE",
  "    QNX                QNX 4.0",
  "    SCO                UNIX System V/386 3.2.2",
  "    SCO                XENIX 2.3",
  "    SCO                XENIX 3.2",
  "    Silicon Graphics   UNIX                cc -DSGI -o iozone iozone.c",
  "    Sony Microsystems  UNIX                same as MIPS",
  "    Sun Microsystems   SUNOS 4.1.1",
  "    Tandem Computers   GUARDIAN 90         1. call the source file IOZONEC",
  "                                           2. C/IN IOZONEC/IOZONE;RUNNABLE",
  "                                           3. RUN IOZONE",
  "    Tandem Computers   Non-Stop UX",
  "        ",
  "    ** for VMS, define iozone as a foreign command via this DCL command:       ",
  " ",
  "       $IOZONE :== $SYS$DISK:[]IOZONE.EXE      ",
  " ",
  "       this lets you pass the command line arguments to IOZONE",
  " ",
  "  Acknowledgements to the following persons for their feedback on IOzone:       ",
  " ",
  "  Andy Puchrik, Michael D. Lawler, Krishna E. Bera, Sam Drake, John H. Hartman, ",
  "  Ted Lyszczarz, Bill Metzenthen, Jody Winston, Clarence Dold, Axel",
  "  Dan Hildebrand, Joe Nordman, Bob Fritz, Jeff Johnson",
  "        ",
  "  --- MODIFICATION HISTORY:",
  " ",
  " ",
  "    3/7/91 William D. Norcott (Bill.Norcott@nuo.mts.dec.com)",
  "                               created",
  " ",
  "    3/22/91 Bill Norcott       tested on OSF/1 ... it works",
  " ",
  "    3/24/91 Bill Norcott       V1.02 -- use calloc in TURBOC to",
  "                                       fix bug with their malloc",
  " ",
  "    3/25/91 Bill Norcott       V1.03 -- add ifdef for XENIX",
  "                                       ",
  "    3/27/91 Bill Norcott       V1.04 -- Includes for SCO UNIX",
  "                                       ",
  "    4/26/91 Bill Norcott       V1.05 -- support AIX and SUNos, check",
  "                                       length of read() and write()",
  "    4/26/91 Bill Norcott       V1.06 -- tabulate results of a series ",
  "                                       of tests",
  "    5/17/91 Bill Norcott       V1.07 -- use time() for VMS",
  "    5/20/91 Bill Norcott       V1.08 -- use %ld for Turbo C and",
  "                                       use #ifdef sun to bypass",
  "                                       inclusion of limits.h",
  "    6/19/91 Bill Norcott       V1.09 -- rid #elif to support HP-UX and ",
  "                                       Silicon Graphics UNIX, and",
  "                                       add #ifdef SGI",
  "                                       add #ifdef CONVERGENT",
  "                                       for Convergent Technologies",
  "                                       also add help option",
  "    7/2/91 Bill Norcott        V1.10 -- delete file if get SIGINT",
  "                                       or SIGTERM",
  "    8/20/91 Bill Norcott       V1.11 -- require no flags with AIX",
  "    11/4/91 Bill Norcott       V1.12 -- support MIPS RISCos",
  "                                         Tandem NonStop-UX, and",
  "                                        IEEE Std POSIX 1003.1-1988",
  "    12/4/91 Bill Norcott       V1.13 -- support NeXT; tell host OS type",
  "    1/23/92 Bill Norcott      V1.14 -- support QNX & use calloc() for buffer",
  "    5/1/92 Bill Norcott      V1.15 -- support SVR4; fix loss of precision",
  "                                       in times() function.  ",
  "                                       support Interactive UNIX",
  "                                       detect ANSI if no O/S",
  "                                       Also, define for generic SVR3",
  "                                       Apollo Domain/OS",
  "                                       Define NO_DELETE and iozone wont",
  "                                       delete the temp file.  Needed to",
  "                                       test raw devices without deleting",
  "                                       them",
  "    10/28/92 Bill Norcott    V1.16 -- bug fix: some unsigned longs changed",
  "                                      to unsigned in V1.15 caused problem",
  "                                      so change back.  Also, note problems",
  "					 with AT&T 6386WGS systems",
  " ",
  "" };

/******************************************************************
  
  INCLUDE FILES (system-dependent)
  
  ******************************************************************/
/* 
V1.15 -- Define the symbol NO_DELETE if you plan to use IOzone to test
the speed of UNIX raw devices such as disk- and tape drives.  This will
tell IOzone NOT to delete the file after the test completes
*/
/* #define NO_DELETE */
/* 
V1.15 -- If you have a generic System V R3 not on my list of supported
systems, define SVR3 when you compile IOzone. If you have to use this, please
send tell me what preprocessor symbols your C compiler defines which
will help me test for your particular system... do a 'man cc' on a
UNIX system to check, or just do a 'man cc > cc.txt' and mail me
cc.txt
*/
/*  Define the following if you have a generic System V R3 system which
is not one of the specific versions listed above
*/
/* #define SVR3 */
#ifdef SVR3
#ifndef OS_TYPE
#define OS_TYPE "'Generic' UNIX System V Release 3 -- vendor unknown" 
#endif
#include <fcntl.h>
#include <sys/types.h>
#define SysVtime
#endif
/* V1.15 -- add Apollo Domain O/S to the list -- it also worked in previous
versions, now it will print its identity
*/
#ifdef apollo
#ifndef OS_TYPE
#define OS_TYPE "Apollo Domain/OS -- using BSD libraries"
#define BSDtime
#define USE_FSYNC
#endif
#endif

/* V1.14 -- use calloc instead of stack for buffer, on all platforms */
#define usecalloc
/* 
V1.14b -- check for ultrix which uses sysconf in newer POSIX version 
but uses BSD-style time in the pre-POSIX versions
V1.15 use fsync() for ultrix even though V4.2 will pick up other options
from POSIX
*/
#ifdef ultrix
#ifndef OS_TYPE
#define OS_TYPE "ULTRIX 4.0 or earlier"
#define BSDtime
#define USE_FSYNC
#endif
#endif
/* V1.13 -- support NeXT by treating it like a Sun... Thanks Axel! */
#ifdef __NeXT__
#ifndef OS_TYPE
#define OS_TYPE "NeXT OS"
#endif
#define sun
#endif
/*
  define nolimits if your system has no limits.h.  Sun's don't but I
  take care of this explicitly beginning with V1.08 of IOzone.
  */
#ifdef sun
#ifndef OS_TYPE
#define OS_TYPE "SunOS"
#endif
#define nolimits
#define BSDtime
#define USE_FSYNC
#endif
/* V1.09 -- Silicon Graphics compile with -DSGI  */
#ifdef SGI
#ifndef OS_TYPE
#define OS_TYPE "Silicon Graphics"
#endif
#define nolimits
#define BSDtime
#endif

/* V1.13 For MIPS RISC/OS and Tandem NonStop-UX*/
#ifdef SYSTYPE_BSD43
#define bsd4_3
#ifndef OS_TYPE
#define OS_TYPE "MIPS RISC/os (BSD 4.3 libraries)"
#endif
#endif

#ifdef SYSTYPE_SYSV
#include <sys/utsname.h>
#define nolimits
#ifdef T_NONSTOP
#define OS_TYPE "TANDEM NonStop-UX (System V libraries)"
#endif
#ifndef OS_TYPE
#define OS_TYPE "MIPS RISC/os (System V libraries)"
#endif
#define SysVtime
#include <sys/types.h>
#include <sys/times.h>
#include <sys/fcntl.h>
#endif
/* V1.14 -- define nolimits and BSDtime for Xenix 2.3.3 */
/* incl definitions of O_* flags for XENIX */
#ifdef M_UNIX
#define SCOunix
#else
#ifdef M_XENIX
#define SCOxenix
#endif
#endif

/* SCO Unix System V */
#ifdef SCOunix
#define OS_TYPE "SCO UNIX System V/386"
#include <sys/types.h>
#include <sys/fcntl.h>
#endif

#ifdef SCOxenix
#ifdef XENIX_2_3
#define OS_TYPE "SCO XENIX 2.3.x"
#define BSDtime
#else
#define OS_TYPE "SCO XENIX 3.x"
#define SysVtime
#endif
#endif

/* V1.12 -- test for POSIX-conformant operating system; requires limits.h */
/*
V1.15 -- I have been told that there is a problem in the times() POSIX 
function in Ultrix V4.1 and greater.  It seems to return the time in
whole seconds (expressed in terms of clock ticks) instead of correctly
returning the elapsed time in clock ticks ALTHOUGH POSIX 1003.1-1988 
and ISO 9945 CLEARLY STATE that "times() shall return the elapsed real 
time, in clock ticks."  

This means calling times() twice in the same second and subtracting
the two values, produces an answer of 0, which IOzone detects as an error.

I thought I had a problem in V1.14 under Ultrix with loss of precision, 
i.e. taking the difference of two large floating point numbers, 
but the problem seems be that Ultrix times() *inherently* has a 
loss of precision, so my "fix" can't fix the broken system call.  
Until they fix it in Ultrix I am going to use BSD-style everything
for Ultrix
*/
#ifndef nolimits

#include <limits.h>
#ifdef _POSIX_ARG_MAX
#ifndef ultrix
#ifndef OS_TYPE
#define OS_TYPE "POSIX 1003.1-1988"
#endif
#define isposix
#undef USE_FSYNC
#else
/* It's ultrix disguised as POSIX; still use BSD calls until they fix it */
#undef OS_TYPE
#define OS_TYPE "ULTRIX 4.1 or later"
#endif
#endif
#endif

/* Tandem's GUARDIAN operating system */
#include <stdio.h>
#ifdef __TANDEM
#ifndef OS_TYPE
#define OS_TYPE "TANDEM GUARDIAN 90"
#endif
#define nosignals
#define ANSItime
#define ANSI_MAIN
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#endif
#ifndef nosignals
#include <signal.h>
#endif
#ifdef  __MSDOS__               /* Turbo C define this way for PCs... */
#define MSDOS                   /* Microsoft C defines this */
#endif
/* VMS and MS-DOS both have ANSI C compilers and use rand()/srand() */
#ifdef  VMS_POSIX
#undef   VMS
#define ANSI_RANDOM     1
#endif
#ifdef  MSDOS
#define ANSI_RANDOM     1
#endif
/* Convergent Technologies M680xx based with Unisys/AT&T Sys5r3 */
#ifdef CONVERGENT
#ifndef OS_TYPE
#define OS_TYPE "Convergent Technologies" 
#endif
#include <fcntl.h>
#define SysVtime
#endif
/* Interactive UNIX System V Release 3.2 */
#ifdef isc386
#ifndef OS_TYPE
#define OS_TYPE "Interactive UNIX System V/386" 
#endif
#include <fcntl.h>
#define SysVtime
#endif

/* V1.11 -- With the following includes, AIX no longer requires -Dunix */
#ifdef _AIX
#ifndef OS_TYPE
#define OS_TYPE "AIX"
#endif
#include <fcntl.h>
#include <sys/time.h>
#endif

#if defined(VMS)
#ifndef OS_TYPE
#define OS_TYPE "VAX/VMS"
#endif
#define ANSItime
#define ANSI_RANDOM     1
#include    <math.h>
#include    <unixio.h>
#include    <ssdef.h>
#include    <file.h>
#include    <time.h>

#else
/* ... either MSDOS, POSIX, or a generic non-POSIX UNIX */
#ifdef MSDOS
#ifndef OS_TYPE
#define OS_TYPE "MS-DOS"
#endif
#define usecalloc
#include <fcntl.h>
#include <time.h>
#endif
/* nope, not MS-DOS, try POSIX */
#ifdef isposix
#include <time.h>
#include <sys/times.h>
#include <fcntl.h>
#include <unistd.h>
#else
#ifdef unix
#include <fcntl.h>
#else 
#define O_RDONLY 0
#endif
#endif
#endif

/* for systems with System V-style time, define SysVtime */
#ifdef M_SYSV
#define SysVtime
#endif

/* for systems with BSD style time, define BSDtime */
#ifdef bsd4_2
#define USE_FSYNC
#ifndef OS_TYPE
#define OS_TYPE "BSD 4.2"
#endif
#define BSDtime
#endif
#ifdef bsd4_3
#define USE_FSYNC
#ifndef OS_TYPE
#define OS_TYPE "BSD 4.3"
#endif
#define BSDtime
#endif
/*
If we made it this far and still don't know which operating system
we are running, check if we at least have ANSI C so we can do some
kind of time functions.  On the other hand, if we know what O/S
we are running use the (more precise) time routines for that O/S,
even if we do have ANSI C.  If we don't know what O/S we are and
we don't have ANSI C, but we are some flavor of UNIX, I will use
time() which any UNIX will have.  For the worst case we are not 
any form of UNIX nor a supported proprietary O/S, and we don't have 
ANSI C either,  one of two things will happen:
1. If you define NOTIMER then you will have to use a stopwatch
2. If NOTIMER not defined, we will assume we can use time()
*/
#ifndef OS_TYPE
#ifdef __STDC__
#define OS_TYPE "ANSI C"
#define ANSItime
#include    <time.h>
#endif
#endif

#ifndef OS_TYPE
#ifdef unix
#define OS_TYPE "UNIX (can't tell if System V or BSD) -- using time()"
#define ANSItime
#include    <time.h>
#endif
#endif

#ifndef OS_TYPE
#if NOTIMER
#define OS_TYPE "System type unknown & not ANSI C -- disabling timing"
#define noclock 1
#else
#define OS_TYPE "System type unknown & not ANSI C -- using time()"
#define ANSItime
#include    <time.h>
#endif
#endif

#ifdef SysVtime
#undef BSDtime
#include <sys/times.h>
#include <sys/param.h>
#ifndef CLK_TCK
#define CLK_TCK HZ
#endif
#endif

#ifdef BSDtime
#undef SysVtime
#include <sys/time.h>
#endif

/******************************************************************
  
  DEFINED CONSTANTS
  
  ******************************************************************/
/* Define NULL in case we don't have it... */
#ifndef NULL
#define NULL 0
#endif
/* 
  V1.14: Define ONETEST to run a single test at runtime as the default
  V1.14: Define AUTOTEST to run in auto test mode as the default
  i.e. the behavior of IOzone when it is invoked with no arguments.  ONETEST
  makes IOzone run a single test using a 1 MB file and 512 byte records.
  AUTOTEST causes IOzone to use auto test mode.
  For compatibility with previous versions of IOZONE, ONETEST is the default
  setting
*/
#ifndef ONETEST
#define AUTOTEST
#endif

/* 
  V1.14: Define USE_FSYNC to force writes to disk during the write phase
  BSD and BSD-derived UNIX variants and also SVR4 are known to have fsync 
  UNIX).  After the file is written and before it is closed, call fsync()
  to force the data to be written from cache to disk.  This (mostly) cancels
  the fact that systems with a lot of memory for cache buffers or memory
  mapping display artificially high transfer rates during the write phase
  of IOzone, because the data never makes it onto the disk.  

*/
#if 0
#define USE_FSYNC
#endif

#define MEGABYTES 1                     /* number of megabytes in file */
#define RECLEN 512                      /* number of bytes in a record */
#define FILESIZE 1048576                /*size of file in bytes*/
#define NUMRECS 2048                    /* number of records */
#define MAXBUFFERSIZE 16*1024           /*maximum buffer size*/
#define MINBUFFERSIZE 128
#define TOOFAST 10
#define IOZONE_USAGE \
"\tUsage:\tiozone [megabytes] [record_length_in_bytes] [[path]filename]\n\t\tiozone auto\n\t\tiozone help\n\n"
#define THISVERSION "V1.16"
#define RELEASEDATE "10/28/92"
  /* Define only one of the following two.  All modern operating systems
     have time functions so let TIME be defined */
#ifndef noclock
#define TIME 1
#endif
  
#define MAXNAMESIZE 1000                /* max # of characters in filename */
#define CONTROL_STRING1 "\t%-8ld%-8ld%-20ld%-20ld\n"
#define CONTROL_STRING2 "\t%-8s%-8s%-20s%-20s\n"
  /*
    For 'auto mode', these defines determine the number of iterations
    to perform for both the file size and the record length.
    I.e., if MEGABYTES_ITER_LIMIT = 5 use 1, 2, 4, 8 & 16 megabyte files
    if RECLEN_ITER_LIMIT = 5 use 512, 1024, 2048, 4096 & 8192 byte records
    */
#define MEGABYTES_ITER_LIMIT 5
#define RECLEN_ITER_LIMIT 5
/******************************************************************
  
  MACRO DEFINITIONS
  
  ******************************************************************/
#define abs(X) ( (X) < 0 ? (-(X)) : (X) )  /* Absolute value of X */
  /******************************************************************
    
    FUNCTION DECLARATIONS
    

    ******************************************************************/
void auto_test();               /* perform automatic test series */
void show_help();               /* show development help*/
double time_so_far();    	/* time since start of program */
void signal_handler();          /* clean up if user interrupts us */
/******************************************************************
  
  GLOBAL VARIABLES
  
  ******************************************************************/
int auto_mode;
char filename [MAXNAMESIZE];            /* name of temporary file */
/******************************************************************
  
  MAIN -- entry point
  
  ******************************************************************/
#ifdef ANSI_MAIN
int 
main(int argc, char *argv[], char *env[])    /* main body of code */
#else
int
     main(argc,argv)
     int argc;
     char *argv[];
#endif
{
#ifdef ANSI_MAIN
  char *fooenv;
#endif
  int fd;
  char *default_filename;
  
#ifdef  usecalloc
  char *buffer;
#else
  char buffer [MAXBUFFERSIZE];            /*a temporary data buffer*/
#endif
  unsigned long i;
  unsigned long megabytes = MEGABYTES;
  unsigned long reclen = RECLEN;
  unsigned long filesize;
  unsigned long numrecs;
  unsigned long thisrec;
#ifdef TIME
  unsigned long filebytes;
  unsigned long readrate, writerate;
  unsigned long goodmegs;
  unsigned long goodrecl;
  double starttime;
  double writetime, readtime;
  double totaltime;
#endif
#ifdef usecalloc
    buffer = (char *) calloc(1, MAXBUFFERSIZE);
#endif
  
#if defined (ANSI_MAIN)
  fooenv= env[0];       /* dummy so we make some use of env (to avoid warnings) */
#endif
  
#if defined (__TANDEM)
  default_filename ="IOZONET"; /* TANDEM GUARDIAN 90 has max 8 char filenames */
#else
  default_filename ="iozone.tmp"; /*default name of temporary file*/
#endif
  if (!auto_mode)
    {
      printf("\n\tIOZONE: Performance Test of Sequential File I/O  --  %s (%s)\n",
	     THISVERSION, RELEASEDATE);
      printf("\t\tBy Bill Norcott\n\n");
#ifdef USE_FSYNC
      printf("\tOperating System: %s -- using fsync()\n\n", OS_TYPE);
#else
      printf("\tOperating System: %s\n\n", OS_TYPE);
#endif
#ifndef nosignals
      signal(SIGINT, signal_handler);      /* handle user interrupt */
      signal(SIGTERM, signal_handler);     /* handle kill from shell */
#endif
    }
  strcpy(filename,default_filename);
  switch (argc) {
  case 1:     /* no args, take all defaults */ 
    printf(IOZONE_USAGE);
#ifdef AUTOTEST 
    auto_mode = 1;
    auto_test();
    printf("Completed series of tests\n");
    exit(0);
#endif
    break;
  case 2:     /* <megabytes|filename> */
    i = (unsigned) abs(atoi(argv[1])); 
    if (i) {
      megabytes = i;
    } else {
      /*
	'Auto mode' will be enabled if the first command line argument is
	the word 'auto'.  This will trigger a series of tests
	*/
      if ( (strcmp(argv[1], "auto") == 0) ||
	  (strcmp(argv[1], "AUTO") == 0) )
	{
	  auto_mode = 1;
	  auto_test();
	  printf("Completed series of tests\n");
	  exit(0);
	} else {
	  auto_mode = 0;
	}
      if ( (strcmp(argv[1], "help") == 0) ||
	  (strcmp(argv[1], "HELP") == 0) )
	{
	  show_help();
	  exit(0);
	}
      strcpy(filename,argv[1]);
    }
    break;
  case 3:     /* <megabytes> <reclen|filename> */
    megabytes = (unsigned) atoi(argv[1]);
    if (atoi(argv[2])) {
      reclen = atoi(argv[2]);
    } else {
      strcpy(filename,argv[2]);
    }
    break;
  case 4:     /* <megabytes> <reclen> <filename> */
    megabytes = (unsigned) atoi(argv[1]);
    reclen = atoi(argv[2]);
    strcpy(filename,argv[3]);
    break;
  default:
    printf("IOZONE: bad usage\n");
    printf(IOZONE_USAGE);
    exit(1);
    
  }
  if (!auto_mode)
    {
      printf("\tSend comments to:\tnorcott_bill@tandem.com\n\n");
    }
  filesize = (unsigned long) (1024L*1024L*megabytes);
  numrecs =  filesize/(unsigned long) reclen;
  if (reclen >  MAXBUFFERSIZE) {
    printf("<Error: Maximum record length is %d bytes\n", MAXBUFFERSIZE);
    exit(1);
  }
  if (reclen < MINBUFFERSIZE) {
    printf("Error: Minimum record length is %d bytes\n", MINBUFFERSIZE);
    exit(1);
  }
  if (!auto_mode)
    {
      printf("\tIOZONE writes a %ld Megabyte sequential file consisting of\n",
	     megabytes);
      printf("\t%ld records which are each %ld bytes in length.\n",
	     numrecs, reclen);
      printf("\tIt then reads the file.  It prints the bytes-per-second\n");
      printf("\trate at which the computer can read and write files.\n\n");
      printf("\nWriting the %ld Megabyte file, '%s'...", megabytes, filename);
    }
  
#if defined (__TANDEM)
  /*
    Tandem's GUARDIAN preallocates file space based on primary- and secondary extents.
    The last 2 parameters to open are the sizes of the primary- and secondary extents,
    in blocks which are 2K bytes each.  After the primary extent is filled, GUARDIAN
    allocates up to 15 additional extents, one at a time.
    */
#define SPECIAL_CREAT
#define PRI_EXT_BLOCKS 1024
#define SEC_EXT_BLOCKS 1024
  if((fd = creat(filename, 0640,
		 PRI_EXT_BLOCKS, SEC_EXT_BLOCKS))<0){
    printf("Cannot create temporary file: %s\n", filename);
    exit(1);
  }
#endif
#ifndef SPECIAL_CREAT
  if((fd = creat(filename, 0640))<0){
    printf("Cannot create temporary file: %s\n", filename);
    exit(1);
  }
#endif
#ifdef TIME
  starttime = time_so_far();
#endif
#ifndef TIME
  printf("\nstart timing\n");
#endif
  for(thisrec=0; thisrec<numrecs; thisrec++){
#ifndef DEBUG_ME
    if(write(fd, buffer, (unsigned) reclen) != reclen)
      {
	printf("Error writing block %d\n", thisrec);
	perror("iozone");
	close(fd);
#ifndef VMS
#ifndef NO_DELETE
	unlink(filename);   /* delete the file */
#endif
	/*stop timer*/
#endif
	exit(1);
      }
#endif
  }
#ifdef USE_FSYNC
  fsync(fd);
#endif
#ifdef TIME
  writetime = time_so_far() - starttime;
  if (!auto_mode)
    {
      printf("%f seconds", writetime);
    }
#endif
#ifndef TIME
  printf("\nstop timing\n");
#endif
  close(fd);
#if defined (VMS)
#define SPECIAL_OPEN_READ
  if((fd = open(filename, O_RDONLY, 0640))<0){
    printf("Cannot open temporary file for read\n");
    exit(1);
  }
#endif
  
#ifdef MSDOS
#define SPECIAL_OPEN_READ
  if((fd = open(filename, O_RDONLY, 0640))<0){
    printf("Cannot open temporary file for read\n");
    exit(1);
  }
#endif
  
  /*
    'Generic' case, compiled if no operating system-specific case was invoked
    */
#ifndef SPECIAL_OPEN_READ
  if((fd = open(filename, O_RDONLY))<0){
    printf("Cannot open temporary file for read\n");
    exit(1);
  }
#endif
  
  
  
  /*start timing*/
  if (!auto_mode)
    {
      printf("\nReading the file...");
    }
#ifndef TIME
  printf("\nstart timing\n");
#endif
#ifdef TIME
  starttime = time_so_far();
#endif
  for(thisrec=0; thisrec<numrecs; thisrec++) {
#ifndef DEBUG_ME
    if(read(fd, buffer, (unsigned) reclen) != reclen)
      {
	printf("Error reading block %d\n", thisrec);
	exit(1);
      }
#endif
  }
#ifndef TIME
  printf("\nstop timing\n");
#endif
#ifdef TIME
  readtime = time_so_far() - starttime;
  if (!auto_mode)
    {
      printf("%f seconds\n", readtime);
    }
#ifdef DEBUG_ME
  readtime = 1;
  writetime = 1;
#endif
  if(readtime!=0)
    {
      filebytes = numrecs* (unsigned long) reclen;
      readrate = (unsigned long) ((double) filebytes / readtime);
      writerate = (unsigned long) ((double) filebytes / writetime);
      if (auto_mode)
	{
	  printf(CONTROL_STRING1,
		 megabytes,
		 reclen,
		 writerate,
		 readrate);
	  
	} else {
	  printf("\nIOZONE performance measurements:\n");
	  printf("\t%ld bytes/second for writing the file\n", writerate);
	  printf("\t%ld bytes/second for reading the file\n", readrate);
	  totaltime = readtime + writetime;
	  if (totaltime < TOOFAST)
	    {
	      goodmegs = (TOOFAST/totaltime)*2*megabytes;
	      printf("\nThe test completed too quickly to give a good result\n");
	      printf("You will get a more precise measure of this machine's\n");
	      printf("performance by re-running IOZONE using the command:\n");
	      printf("\n\tiozone %ld ", goodmegs);
	      printf("\t(i.e., file size = %ld megabytes)\n", goodmegs);
	    }
	}
    } else {
      goodrecl = reclen/2;
      printf("\nI/O error during read.  Try again with the command:\n");
      printf("\n\tiozone %ld %ld ", megabytes,  goodrecl);
      printf("\t(i.e. record size = %ld bytes)\n",  goodrecl);
    }
#endif
  close(fd);
#ifndef VMS
#ifndef NO_DELETE
  unlink(filename);   /* delete the file */
#endif
  /*stop timer*/
#endif
#ifdef  usecalloc
  free(buffer);           /* deallocate the memory */
#endif
#ifdef VMS
  return SS$_NORMAL;
#else
  return 0;
#endif
}
/******************************************************************
  
  SHOW_HELP -- show development help of this program
  
  ******************************************************************/
void 
  show_help()
{
  int i;
  printf("IOZONE: help mode\n\n");
  for(i=0; strlen(help[i]); i++)
    {
      printf("%s\n", help[i]);
    }
}
/******************************************************************
  
  SIGNAL_HANDLER -- clean up if user interrupts the program
  
  ******************************************************************/
void 
  signal_handler()
{
  printf("\nIOZONE: interrupted\n\n");
#ifndef VMS
#ifndef NO_DELETE
  printf("deleting file: %s\n", filename);
  unlink(filename);   /* delete the file */
#endif
#endif
  printf("exiting IOzone\n\n");
  exit(0);
}
/******************************************************************
  
  AUTO_TEST -- perform series of tests and tabulate results
  
  ******************************************************************/
void 
  auto_test()
{
  
  int megsi, recszi;
  char megs[10];
  char recsz[10];
  int i,j;
  int autoArgc = 3;
  char *autoArgv[3];
  
  printf("IOZONE: auto-test mode\n\n");
  printf(CONTROL_STRING2,
	 "MB",
	 "reclen",
	 "bytes/sec written",
	 "bytes/sec read");
  autoArgv[0] = "IOzone auto-test";
  autoArgv[1] = megs;
  autoArgv[2] = recsz;
  /*
    Start with file size of 1 megabyte and repeat the test MEGABYTES_ITER_LIMIT
    times.  Each time we run, the file size is doubled
    */
  for(i=0,megsi=1;i<MEGABYTES_ITER_LIMIT;i++,megsi*=2)
    {
      sprintf(megs, "%d", megsi);
      /*
	Start with record size of 512 bytes and repeat the test RECLEN_ITER_LIMIT
	times.  Each time we run, the record size is doubled
	*/
      for (j=0,recszi=512;j<RECLEN_ITER_LIMIT;j++,recszi*=2)
        {
	  sprintf(recsz, "%d", recszi);
#ifdef ANSI_MAIN
	  main(autoArgc, autoArgv, NULL);
#else
	  main(autoArgc, autoArgv);
#endif
        }
    }
}

#ifdef TIME
/******************************************************************
  
  TIME_SO_FAR -- return elapsed time

  5/17/91 Bill Norcott        V1.07 -- use time() for VMS
  The times() function in VMS returns proc & user CPU time in 10-millisecond
  ticks.  Instead, use time() which lacks the precision but gives clock
  time in seconds.
  V1.14 make val of type clock_t if we are dealing with POSIX
  V1.15 first time this is called, set base to initial number of clock
  ticks, then subtract this value from all subsequent calculations.  This
  will fix a loss of precision when times returns very big numbers;
  
  Here is what we check for (in order).  Each returns so we do not
  have to nest the #ifdefs -- should satisfy even the dumbest 
  pre-processor.  Note that each has its own flavor of timekeeping
  1. ANSI C
  2. POSIX - with and without CLK_TCK defined
  3. System V variants
  4. MS-DOS
  5. BSD variants
  ******************************************************************/
double
  time_so_far()
{
#if defined(ANSItime)
  return (double) time(NULL);
#endif

#ifdef isposix
  {
    static clock_t base, val;
    struct tms tms;
    
    if (base == 0)
      {
	base = times(&tms);
	if (base == (clock_t) -1)
	  {
	    perror("times");
	  }
      }
    val = times(&tms);
    if (val == (clock_t) -1)
      {
	perror("times");
      }
    val = val - base;
#ifndef CLK_TCK
    return ((double) val) / ((double) sysconf(_SC_CLK_TCK));
#else
    return ((double) val) / ((double) CLK_TCK);
#endif
  }
#endif

#ifdef SysVtime
  {
    static long base, val;
    struct tms tms;
    
    if (base == 0)
      {
	if ((base = times(&tms)) == -1)
	  {
	    perror("times");
	  }
      }
    if ((val = times(&tms)) == -1)
      {
	perror("times");
      }
    val = val - base;
    return ((double) val) / ((double) CLK_TCK);
  }
#endif

#if defined(MSDOS)
  return ((double) clock()) / ((double) CLK_TCK);
#endif

#ifdef BSDtime
  {
    struct timeval tp;
    
    if (gettimeofday(&tp, (struct timezone *) NULL) == -1)
      perror("gettimeofday");
    return ((double) (tp.tv_sec)) +
      (((double) tp.tv_usec) / 1000000.0);
  }
#endif
}
#endif
