/* Main include file for TeX in C.  Originally by Tim Morgan,
   December 23, 1987.  These routines are also used by Metafont (with
   some name changes).  */

#ifndef TEXMF_DEBUG
/* Tell Kpathsea not to define DEBUG, which tells TeX & MF to go into
   debugging mode at fatal errors. This file (texmf.h) is included by
   {tex,mf}d.h, which is the first include in the C files output by
   web2c.  */
#define NO_DEBUG
#endif

#include "cpascal.h"

#ifdef TeX
#define dump_file fmtfile
#define dump_path TEXFORMATPATH
#define write_out writedvi
#define out_file dvifile
#define out_buf dvibuf
#else /* not TeX */
#define dump_file basefile
#define dump_path MFBASEPATH
#define write_out writegf
#define out_file gffile
#define out_buf gfbuf
#endif /* not TeX */



/* File types.  */
typedef FILE *bytefile, *wordfile;



/* Read a line of input as quickly as possible.  */
#define	inputln(stream, flag)	input_line (stream)
extern boolean input_line ();


/* We need to read an integer from stdin if we're debugging.  */
#ifdef DEBUG
#define getint()  inputint (stdin)
#else
#define getint()
#endif



/* `bopenin' (and out) is used only for reading (and writing) .tfm
   files; `wopenin' (and out) only for dump files.  The filenames are
   passed in as a global variable, `nameoffile'.  */
   
#define bopenin(f)	open_input (&(f), TFMFILEPATH, FOPEN_RBIN_MODE)
#define wopenin(f)	open_input (&(f), dump_path, FOPEN_RBIN_MODE)
#define bopenout(f)	open_output (&(f), FOPEN_WBIN_MODE)
#define wopenout	bopenout
#define bclose		aclose
#define wclose		aclose

/* This routine has to return four values.  */
#define	dateandtime(i, j, k, l)	get_date_and_time (&(i), &(j), &(k), &(l))



/* If we're running under Unix, use system calls instead of standard I/O
   to read and write the output files; also, be able to make a core dump. */ 
#ifndef unix
#define	dumpcore()	exit (1)

#ifdef TeX
#define	writedvi(a, b)							\
  (void) fwrite ((char *) &dvibuf[a], sizeof (dvibuf[a]),		\
                 (int) ((b) - (a) + 1), dvifile)
#else
#define	writegf(a, b)							\
  (void) fwrite ((char *) &gfbuf[a], sizeof (gfbuf[a]),			\
                 (int) ((b) - (a) + 1), gffile)
#endif /* not TeX */

#else /* unix */
#define	dumpcore	abort

#ifdef TeX
#define	writedvi(start, end)						\
  if (write (fileno (dvifile), (char *) &dvibuf[start],			\
             (int) ((end) - (start) + 1))				\
      != (int) ((end) - (start) + 1))					\
    FATAL_PERROR ("dvi file")
#else
#define	writegf(start, end)						\
  if (write (fileno (gffile), (char *) &gfbuf[start],			\
             (int) ((end) - (start) + 1))				\
      != (int) ((end) - (start) + 1))					\
    FATAL_PERROR ("gf file")
#endif /* not TeX */
#endif /* unix */


/* Reading and writing the dump files.  `(un)dumpthings' is called from
   the change file.*/
#define	dumpthings(base, len)						\
  do_dump ((char *) &(base), sizeof (base), (int) (len), dump_file)

#define	undumpthings(base, len)						\
  do_undump ((char *) &(base), sizeof (base), (int) (len), dump_file)

/* We define the routines to do the actual work in texmf.c.  */
extern void do_dump (), do_undump ();

/* Use the above for all the other dumping and undumping.  */
#define generic_dump(x) dumpthings (x, 1)
#define generic_undump(x) undumpthings (x, 1)

#define dumpwd		generic_dump
#define undumpwd	generic_undump
#define dumphh		generic_dump
#define undumphh	generic_undump
#define dumpqqqq   	generic_dump
#define	undumpqqqq	generic_undump

/* `dump_int' is called with constant integers, so we put them into a
   variable first.  */
#define	dumpint(x)							\
  do									\
    {									\
      integer x_val = (x);						\
      generic_dump (x_val);						\
    }									\
  while (0)

/* web2c/regfix puts variables in the format file loading into
   registers.  Some compilers aren't willing to take addresses of such
   variables.  So we must kludge.  */
#ifdef REGFIX
#define undumpint(x)							\
  do									\
    {									\
      integer x_val;							\
      generic_undump (x_val);						\
      x = x_val;							\
    }									\
  while (0)
#else
#define	undumpint	generic_undump
#endif

/* Metafont wants to write bytes to the TFM file.  The casts in these
   routines are important, since otherwise memory is clobbered in some
   strange way, which causes ``13 font metric dimensions to be
   decreased'' in the trap test, instead of 4.  */

#define bwritebyte(f, b)    putc ((char) (b), f)
#define bwrite2bytes(f, h)						\
  do									\
    {									\
      integer v = (integer) (h);					\
      putc (v >> 8, f);  putc (v & 0xff, f);				\
    }									\
  while (0)
#define bwrite4bytes(f, w)						\
  do									\
    {									\
      integer v = (integer) (w);					\
      putc (v >> 24, f); putc (v >> 16, f);				\
      putc (v >> 8, f);  putc (v & 0xff, f);				\
    }									\
  while (0)



/* If we're running on an ASCII system, there is no need to use the
   `xchr' array to convert characters to the external encoding.  */
#ifdef NONASCII
#define	Xchr(x)		xchr[x]
#else
#define	Xchr(x)		((char) (x))
#endif


/* Declare routines in texmf.c.  */
extern void get_date_and_time ();
extern void topenin ();
extern void calledit ();
extern boolean extensionirrelevantp ();
extern boolean input_line ();
extern void do_dump ();
extern void do_undump ();
#ifdef TeX
extern boolean maketextex ();
extern boolean maketextfm ();
#else
extern void main_body ();
extern boolean initscreen ();
extern boolean maketexmf ();
extern void updatescreen ();
extern void blankrectangle ();
extern void paintrow ();
#endif /* not TeX */
