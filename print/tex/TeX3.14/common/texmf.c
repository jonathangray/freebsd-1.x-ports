/* Hand-coded routines for TeX or Metafont in C.  This code was (mostly)
   written by Tim Morgan, drawing from other Unix ports of TeX.  The
   file-opening routines are also used by BibTeX.  */

/* Either `texd.h' or `mfd.h' will include `../common/texmf.h'.  */

#include "../common/endian.h"

/* Instantiate data in `texd.h' or `mfd.h' here.  */
#define	EXTERN

#ifdef TeX
#include "texd.h"
#define dump_default_var TEXformatdefault
#define dump_default " plain.fmt"
#define dump_format " %s.fmt"
#define dump_ext_length 4
#define dump_default_length formatdefaultlength
#define virgin_program "virtex"
#define main_program texbody
#define edit_value tex_edit_value
#define edit_var "TEXEDIT"
#else /* not TeX */
#include "mfd.h"
#define dump_default_var MFbasedefault
#define dump_default " plain.base"
#define dump_format " %s.base"
#define dump_ext_length 5
#define dump_default_length basedefaultlength
#define virgin_program "virmf"
#define main_program main_body
#define edit_value mf_edit_value
#define edit_var "MFEDIT"
#endif /* not TeX */

/* Catch interrupts.  */
#define	HANDLE_INTERRUPTS

#ifdef HANDLE_INTERRUPTS
#ifdef _POSIX_SOURCE
#include <sys/types.h>
#endif
#include <signal.h>
#endif

#ifdef FUNNY_CORE_DUMP
void funny_core_dump ();
#endif

#ifdef BSD
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/file.h>
#else /* not BSD */
#include <time.h>
#endif /* not BSD */

#if defined (BSD) || defined (SYSV)
#include <sgtty.h>
#endif

extern struct tm *localtime ();



/* The main program, etc.  */

/* What we were invoked as and with.  */
static char *program_name = NULL;
static int gargc;
char **gargv;


/* The entry point: set up for reading the command line, which will
   happen in `topenin', then call the main body.  */

void
main (ac, av)
  int ac;
  char *av[];
{
  char custom_default[FILENAMESIZE];

  gargc = ac;
  gargv = av;

  dump_default_var = dump_default;
  dump_default_length = strlen (dump_default + 1);

#ifndef INI
  if (readyalready != 314159)
    {
      program_name = rindex (av[0], '/');
      if (program_name == NULL)
	program_name = av[0];
      else
	program_name++;
      if (strcmp (program_name, virgin_program) != 0)
        {
          /* TeX or Metafont adds the space at the end of the name.  */
          (void) sprintf (custom_default, dump_format, program_name);
          dump_default_var = custom_default;
          dump_default_length = strlen (program_name) + dump_ext_length;
        }
    }
#endif /* not INI */

  main_program ();
  /*NOTREACHED*/
} 


/* This is supposed to ``open the terminal for input'', but what we
   really do is copy command line arguments into TeX's or Metafont's
   buffer, so they can handle them.  If nothing is available, or we've
   been called already (and hence, gargc==0), we return with
   `last=first'.  */

void
topenin ()
{
  register int i;

  buffer[first] = 0;	/* So the first `strcat' will work.  */

  if (gargc > 1)
    { /* We have command line arguments.  */
      for (i = 1; i < gargc; i++)
        {
	  (void) strcat ((char *) &buffer[first], gargv[i]);
          (void) strcat ((char *) &buffer[first], " ");
	}
      gargc = 0;	/* Don't do this again.  */
    }

  /* Make `last' be one past the last non-blank character in `buffer'.  */
  for (last = first; buffer[last]; ++last)
    ;
  for (--last; last >= first && buffer[last] == ' '; --last)
    ;
  last++;
}



#ifdef HANDLE_INTERRUPTS
/* All our interrupt handler has to do is set TeX's or Metafont's global
   variable `interrupt'; then they will do everything needed.  */

static void
catch_interrupt ()
{
  interrupt = 1;
  (void) signal (SIGINT, catch_interrupt);
}
#endif /* HANDLE_INTERRUPTS */


/* Besides getting the date and time here, we also set up the interrupt
   handler, for no particularly good reason.  It's just that since the
   `fix_date_and_time' routine is called early on (section 1337 in TeX,
   ``Get the first line of input and prepare to start''), this is as
   good a place as any.  */

void
get_date_and_time (minutes, day, month, year)
  integer *minutes, *day, *month, *year;
{
  long clock;
  struct tm *tmptr;

  clock = time ((long *) 0);
  tmptr = localtime (&clock);

  *minutes = tmptr->tm_hour * 60 + tmptr->tm_min;
  *day = tmptr->tm_mday;
  *month = tmptr->tm_mon + 1;
  *year = tmptr->tm_year + 1900;

#ifdef HANDLE_INTERRUPTS
  {
    SIGNAL_HANDLER_RETURN_TYPE (*old_handler) ();

    if ((old_handler = signal (SIGINT, catch_interrupt)) != SIG_DFL)
      (void) signal (SIGINT, old_handler);
  }
#endif
}



/* I/O for TeX and Metafont.  */

/* Read a line of input as efficiently as possible while still looking
   like Pascal.  We set `last' to `first' and return `false' if we get
   to eof.  Otherwise, we return `true' and either `last==first' or
   `buffer[last-1]!=' ''; that is, last == first + length(line except
   trailing whitespace).  */

boolean
input_line (f)
  FILE *f;
{
  register int i;

  last = first;

#ifdef BSD
  if (f == stdin) clearerr (stdin);
#endif

  while (last < bufsize && (i = getc (f)) != EOF && i != '\n')
    buffer[last++] = i;

  if (i == EOF && last == first)
      return false;

  /* We didn't get the whole line because our buffer was too small.  */
  if (i != EOF && i != '\n')
    {
      (void) fprintf (stderr,
                     "! Unable to read an entire line---bufsize=%d.\n",
                     bufsize);
      (void) fprintf (stderr, "Please ask a wizard to enlarge me.\n");
      exit (1);
    }

  buffer[last] = ' ';
  if (last >= maxbufstack)
    maxbufstack = last;

  /* Trim trailing whitespace.  */
  while (last > first && (buffer[last-1] == ' ' || buffer[last-1] == '\t'))
    --last;

  /* Now, either `last==first' or `buffer[last-1] != ' '' (or \t). */

  /* Don't bother using xord if we don't need to.  */
#ifdef NONASCII
  for (i = first; i <= last; i++)
     buffer[i] = xord[buffer[i]];
#endif

    return true;
}



/* This string specifies what the `e' option does in response to an
   error message.  */ 
static char *edit_value = EDITOR;

/* This procedure is due to sjc@s1-c.  TeX (or Metafont) calls it when
   the user types `e' in response to an error, invoking a text editor on
   the erroneous source file.  FNSTART is how far into FILENAME the
   actual filename starts; FNLENGTH is how long the filename is.
   
   See ../site.h for how to set the default, and how to override it.  */

void
calledit (filename, fnstart, fnlength, linenumber)
  ASCIIcode *filename;
  poolpointer fnstart;
  integer fnlength, linenumber;
{
  char *temp, *command;
  char c;
  int sdone, ddone, i;

  sdone = ddone = 0;
  filename += fnstart;

  /* Close any open input files, since we're going to kill the job.  */
  for (i = 1; i <= inopen; i++)
    (void) fclose (inputfile[i]);

  /* Replace the default with the value of the appropriate environment
     variable, if it's set.  */
  temp = getenv (edit_var);
  if (temp != NULL)
    edit_value = temp;

  /* Construct the command string.  The `11' is the maximum length an
     integer might be.  */
  command = xmalloc ((unsigned) (strlen (edit_value) + fnlength + 11));

  /* So we can construct it as we go.  */
  temp = command;

  while ((c = *edit_value++) != 0)
    {
      if (c == '%')
        {
          switch (c = *edit_value++)
            {
	    case 'd':
	      if (ddone)
                {
		  (void) fprintf (stderr,
                           "! `%%d' cannot appear twice in editor command.\n");
	          exit (1);
		}
              (void) sprintf (temp, "%d", linenumber);
              while (*temp != '\0')
                temp++;
              ddone = 1;
              break;

	    case 's':
              if (sdone)
                {
	          (void) fprintf(stderr,
                           "! `%%s' cannot appear twice in editor command.\n");
		  exit (1);
		}
              for (i =0; i < fnlength; i++)
		*temp++ = Xchr (filename[i]);
              sdone = 1;
              break;

	    case '\0':
              *temp++ = '%';
              /* Back up to the null to force termination.  */
	      edit_value--;
	      break;

	    default:
	      *temp++ = '%';
	      *temp++ = c;
	      break;
	    }
	}
      else
	*temp++ = c;
    }

  *temp = 0;

  /* Execute the command.  */
  if (system (command) != 0)
    (void) fprintf(stderr, "! Trouble executing `%s'.\n", command);

  /* Quit, since we found an error.  */
  exit (1);
}



/* Read and write format (for TeX) or base (for Metafont) files.  By
   default, these files are architecture dependent; specifically,
   BigEndian and LittleEndian architectures produce different files.
   These routines always output BigEndian files.  This still does not
   make the dump files architecture-independent, because it is possible
   to make a format file that dumps a glue ratio, i.e., a floating-point
   number.  Fortunately, none of the standard formats do that.  */


/* This macro is always invoked as a statement.  It assumes a variable
   `temp'.  */
   
#define SWAP(x, y) temp = (x); (x) = (y); (y) = temp;


/* Make the NITEMS items pointed at by P, each of size SIZE, be the
   opposite-endianness of whatever they are now.  */

static void
swap_items (p, nitems, size)
  char *p;
  int nitems;
  int size;
{
  char temp;
  
  while (nitems--)
    {
      switch (size)
        {
        case 8:
          SWAP (p[4], p[7]);
          SWAP (p[5], p[6]);
          /* Note fall-through, to swap the first integer.  */

        case 4:
          SWAP (p[0], p[3]);
          SWAP (p[1], p[2]);
          break;

        case 2:
          SWAP (p[0], p[1]);
          break;

        case 1:
          /* Nothing to do.  */
          break;

        default:
          fprintf (stderr, "! I can't (un)dump a %d byte item.\n", size);
          exit (1);
        }

      p += size;
    }
}


/* Here we write NITEMS items, each item being ITEM_SIZE bytes long.
   The pointer to the stuff to write is P, and we write to the file
   OUT_FILE.  */

void
do_dump (p, item_size, nitems, out_file)
  char *p;
  int item_size, nitems;
  FILE *out_file;
{
#ifndef WEB2C_BIG_ENDIAN
  swap_items (p, nitems, item_size);
#endif

  if (fwrite (p, item_size, nitems, out_file) != nitems)
    {
      fprintf (stderr, "! Could not write %d %d-byte item(s).\n",
               nitems, item_size);
      exit (1);
    }

  /* Have to restore the old contents of memory, since some of it might
     get used again.  */
#ifndef WEB2C_BIG_ENDIAN
  swap_items (p, nitems, item_size);
#endif
}


/* Here is the dual of the writing routine.  */

void
do_undump (p, item_size, nitems, in_file)
  char *p;
  int item_size, nitems;
  FILE *in_file;
{
  if (fread (p, item_size, nitems, in_file) != nitems)
    {
      fprintf (stderr, "! Could not read %d %d-byte item(s).\n",
               nitems, item_size);
      exit (1);
    }

#ifndef WEB2C_BIG_ENDIAN
  swap_items (p, nitems, item_size);
#endif
}


#ifdef FUNNY_CORE_DUMP
/* This procedure is due to chris@mimsy.umd.edu.  It makes a core dump
   without any sort of error status (abort(2) does return an error status,
   so we don't want to use that).  It is used only when making a preloaded
   TeX from virtex, and is triggered by a magic file name requested as
   input (see `open_input', above).

   This is what is known in computing circles as a hack.  */

void
funny_core_dump ()
{
  int pid, w;
  union wait status;

  switch (pid = vfork ())
    {
    case -1:		/* failed */
      perror ("vfork");
      exit (-1);
      /*NOTREACHED*/

    case 0:             /* child */
       (void) signal (SIGQUIT, SIG_DFL);
       (void) kill (getpid (), SIGQUIT);
       (void) write (2, "how did we get here?\n", 21);
       _exit (1);
       /*NOTREACHED*/

    default:		/* parent */
      while ((w = wait (&status)) != pid && w != -1)
	;
      if (status.w_coredump)
	exit (0);
      (void) write (2, "attempt to dump core failed\n", 28);
      exit (1);
    }
}
#endif /* FUNNY_CORE_DUMP */



#ifndef TeX
/* On-line display routines for Metafont.  Here we use a dispatch table
   indexed by the MFTERM or TERM environment variable to select the
   graphics routines appropriate to the user's terminal.  stdout must be
   connected to a terminal for us to do any graphics.  */

/* We don't want any other window routines screwing us up if we're
   trying to do the trap test.  We could have written another device for
   the trap test, but the terminal type conditionals in initscreen argue
   against that.  */

/* I'm hoping against hope that all compilers understand `defined'.  */
#if defined (TRAP) || defined (INI)
#undef HP2627WIN
#undef SUNWIN
#undef TEKTRONIXWIN
#undef UNITERMWIN
#undef X10WIN
#undef X11WIN
#endif


#ifdef HP2627WIN
extern mf_hp2627_initscreen (), mf_hp2627_updatescreen ();
extern mf_hp2627_blankrectangle (), mf_hp2627_paintrow ();
#endif

#ifdef SUNWIN
extern mf_sun_initscreen (), mf_sun_updatescreen ();
extern mf_sun_blankrectangle (), mf_sun_paintrow ();
#endif

#ifdef TEKTRONIXWIN
extern mf_tektronix_initscreen (), mf_tektronix_updatescreen ();
extern mf_tektronix_blankrectangle (), mf_tektronix_paintrow ();
#endif

#ifdef UNITERMWIN
extern mf_uniterm_initscreen (), mf_uniterm_updatescreen();
extern mf_uniterm_blankrectangle(), mf_uniterm_paintrow();
#endif

#ifdef X10WIN
extern mf_x10_initscreen (), mf_x10_updatescreen ();
extern mf_x10_blankrectangle (), mf_x10_paintrow ();
#endif

#ifdef X11WIN
extern mf_x11_initscreen (), mf_x11_updatescreen ();
extern mf_x11_blankrectangle (), mf_x11_paintrow ();
#endif


/* `mfwsw' contains the dispatch tables for each terminal.  We map the
   Pascal calls to the routines `init_screen', `update_screen',
   `blank_rectangle', and `paint_row' into the appropriate entry point
   for the specific terminal that MF is being run on.  */

struct mfwin_sw
{
  char *mfwsw_type;		/* Name of terminal a la TERMCAP.  */
  int (*mfwsw_initscreen) ();
  int (*mfwsw_updatescrn) ();
  int (*mfwsw_blankrect) ();
  int (*mfwsw_paintrow) ();
} mfwsw[] =

/* Now we have a long structure which initializes this array of
   ``Metafont window switches''.  */

{
#ifdef HP2627WIN
  { "hp2627", mf_hp2627_initscreen, mf_hp2627_updatescreen,
    mf_hp2627_blankrectangle, mf_hp2627_paintrow },
#endif

#ifdef SUNWIN
  { "sun", mf_sun_initscreen, mf_sun_updatescreen,
    mf_sun_blankrectangle, mf_sun_paintrow },
#endif

#ifdef TEKTRONIXWIN
  { "tek", mf_tektronix_initscreen, mf_tektronix_updatescreen,
    mf_tektronix_blankrectangle, mf_tektronix_paintrow },
#endif

#ifdef UNITERMWIN
   { "uniterm", mf_uniterm_initscreen, mf_uniterm_updatescreen,
     mf_uniterm_blankrectangle, mf_uniterm_paintrow },
#endif

#ifdef X10WIN
  { "xterm", mf_x10_initscreen, mf_x10_updatescreen,
    mf_x10_blankrectangle, mf_x10_paintrow },
#endif

#ifdef X11WIN
  { "xterm", mf_x11_initscreen, mf_x11_updatescreen, 
    mf_x11_blankrectangle, mf_x11_paintrow },
#endif

  { "Irrelevant", NULL, NULL, NULL, NULL },

/* Finally, we must have an entry with a terminal type of NULL.  */
  { NULL, NULL, NULL, NULL, NULL }

}; /* End of the array initialization.  */


/* This is a pointer to the mfwsw[] entry that we find.  */
static struct mfwin_sw *mfwp;

/* The following are routines that just jump to the correct
   terminal-specific graphics code. If none of the routines in the
   dispatch table exist, or they fail, we produce trap-compatible
   output, i.e., the same words and punctuation that the unchanged
   mf.web would produce.  */


/* This returns true if window operations legal, else false.  */

boolean
initscreen ()
{
#ifndef TRAP
  char *ttytype = getenv ("MFTERM");
  
  if (ttytype == NULL)
    ttytype = getenv ("TERM");

  /* If we don't know kind of terminal this is, or if Metafont isn't
      being run interactively, don't do any online output.  */
  if (ttytype == NULL || !isatty (fileno (stdout)))
    return 0;

  /* Test each of the terminals given in `mfwsw' against the terminal
     type, and take the first one that matches, or if the user is running
     under Emacs, the first one.  */
  for (mfwp = mfwsw; mfwp->mfwsw_type != NULL; mfwp++)
    if (!strncmp (mfwp->mfwsw_type, ttytype, strlen (mfwp->mfwsw_type))
	|| !strcmp (ttytype, "emacs"))
      if (mfwp->mfwsw_initscreen)
	return ((*mfwp->mfwsw_initscreen) ());
      else
	{
	  fprintf (stderr,
                   "! Couldn't initialize the online display for a `%s'.\n",
                   ttytype);
	  return 1;
	}
  
  /* The current terminal type wasn't found in any of the entries, so
     silently give up, assuming that the user isn't on a terminal that
     supports graphic output.  */
  return 0;
#else /* TRAP */
  return 1;
#endif /* TRAP */
}


/* Make sure everything is visible.  */

void
updatescreen ()
{
#ifndef TRAP
  if (mfwp->mfwsw_updatescrn)
    ((*mfwp->mfwsw_updatescrn) ());
  else
    {
      printf ("Updatescreen called\n");
    }
#else /* TRAP */
  fprintf (logfile, "Calling UPDATESCREEN\n");
#endif /* TRAP */
}


/* This sets the rectangle bounded by ([left,right], [top,bottom]) to
   the background color.  */

void
blankrectangle (left, right, top, bottom)
     screencol left, right;
     screenrow top, bottom;
{
#ifndef TRAP
  if (mfwp->mfwsw_blankrect)
    ((*mfwp->mfwsw_blankrect) (left, right, top, bottom));
  else
    {
      printf ("Blankrectangle l=%d  r=%d  t=%d  b=%d\n",
	      left, right, top, bottom);
    }
#else /* TRAP */
  fprintf (logfile, "\nCalling BLANKRECTANGLE(%d,%d,%d,%d)\n", left,
	   right, top, bottom);
#endif /* TRAP */
}


/* This paints ROW, starting with the color INIT_COLOR. 
   TRANSITION_VECTOR then specifies the length of the run; then we
   switch colors.  This goes on for VECTOR_SIZE transitions.  */

void
paintrow (row, init_color, transition_vector, vector_size)
     screenrow row;
     pixelcolor init_color;
     transspec transition_vector;
     screencol vector_size;
{
#ifndef TRAP
  if (mfwp->mfwsw_paintrow)
    ((*mfwp->mfwsw_paintrow) (row, init_color,
			      transition_vector, vector_size));
  else
    {
      printf ("Paintrow r=%d  c=%d  v=");
      while (vector_size-- > 0)
	printf ("%d  ", transition_vector++);
      printf ("\n");
    }
#else /* TRAP */
  unsigned k;

  fprintf (logfile, "Calling PAINTROW(%d,%d;", row, init_color);
  for (k = 0; k <= vector_size; k++)
    {
      fprintf (logfile, "%d", transition_vector[k]);
      if (k != vector_size)
	fprintf (logfile, ",");
    }
  fprintf (logfile, ")\n");
#endif /* TRAP */
}
#endif /* not TeX */
