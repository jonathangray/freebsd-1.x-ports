/* tex-make.c: Run external programs to make TeX files.

Copyright (C) 1993 Karl Berry.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <kpathsea/config.h>

#include <kpathsea/c-fopen.h>
#include <kpathsea/concatn.h>
#include <kpathsea/fn.h>
#include <kpathsea/readable.h>
#include <kpathsea/tex-make.h>
#include <kpathsea/variable.h>


/* We never throw away stdout, since that is supposed to be the filename
   found, if all is successful.  This variable controls whether stderr
   is thrown away.  */
boolean kpse_make_tex_discard_errors = false;


#define MAKE_TEX_PK_SPEC \
  "$KPATHSEA_DPI $MAKETEX_BASE_DPI $MAKETEX_MAG $MAKETEX_MODE"

/* We could generalize this to the idea of a ``path spec'', and collect
   the default paths, environment variables, and whatever else in the
   same structure.  `kpse_var_expand' could understand ${PARM-word} to
   go along with it, then the envvar search list itself would be
   overridable.  Is it worth it?  */
kpse_make_spec_type kpse_make_specs[] = {
  /* kpse_gf_format */		{ false, NULL, NULL },
  /* kpse_pk_format */		{ false, "MakeTeXPK", MAKE_TEX_PK_SPEC },
  /* kpse_any_glyph_format */	{ false, "MakeTeXPK", MAKE_TEX_PK_SPEC },
  /* kpse_bib_format */		{ false, NULL, NULL }, 
  /* kpse_bst_format */		{ false, NULL, NULL }, 
  /* kpse_mf_format */		{ false, "MakeTeXMF", NULL },
  /* kpse_tex_format */		{ false, "MakeTeXTeX", NULL },
  /* kpse_tfm_format */		{ false, "MakeTeXTFM", NULL },
  /* kpse_vf_format */		{ false, NULL, NULL },
};


/* We assume the script will output the filename it creates (and nothing
   else) on standard output, and hence run the script with `popen'.  */

string
kpse_make_tex P2C(kpse_file_format_type, format,  const_string, base_file)
{
  string ret;
  kpse_make_spec_type spec;
  spec = kpse_make_specs[format];
  
  if (KPSE_MAKE_SPEC_ENABLED (spec))
    {
      FILE *f;
      const_string prog = KPSE_MAKE_SPEC_PROGRAM (spec);
      string PROG = uppercasify (prog);
      string progenv = getenv (PROG);
      const_string arg_spec = progenv ? progenv : KPSE_MAKE_SPEC_ARGS (spec);
      string args = arg_spec ? kpse_var_expand (arg_spec) : (string) "";
      string cmd = concatn (prog, " ", base_file, " ", args, NULL);
      
      /* Only way to discard errors is redirect stderr inside another
         shell; otherwise, if the MakeTeX... script doesn't exist, we
         will see the `sh: MakeTeX...: not found' error.  */
      if (kpse_make_tex_discard_errors)
        {
          string old_cmd = cmd;
          cmd = concat3 ("sh -c \"", cmd, "\" 2>/dev/null");
          free (old_cmd);
        }

      /* Run the script and prepare to read the output.  */
      f = popen (cmd, FOPEN_R_MODE);
      
      free (PROG);
      free (cmd);
      if (strlen (args) > 0)
        free (args);
      
      if (f)
        {
          int c;
          string fn;             /* The final filename.  */
          unsigned len;          /* And its length.  */
          fn_type output;
          output = fn_init ();   /* Collect the script output.  */
          
          /* Read all the output and terminate with a null.  */
          while ((c = getc (f)) != EOF)
            fn_1grow (&output, c);
          fn_1grow (&output, 0);
          
          /* Maybe should check for `EXIT_SUCCESS' status before even
             looking at the output?  */
          if (pclose (f) == -1)
            FATAL_PERROR (cmd);
          
          len = FN_LENGTH (output);
          fn = FN_STRING (output);
          
          /* Remove trailing newlines and returns.  */
          while (len > 1 && (fn[len - 2] == '\n' || fn[len - 2] == '\r'))
            {
              fn[len - 2] = 0;
              len--;
            }

          /* If no output from script, return NULL.  Otherwise check
             what it output.  */
          ret = len == 1 ? NULL : kpse_readable_file (fn);
          
          /* Free the name if we're not returning it.  */
          if (fn != ret)
            free (fn);
        }
      else
        /* popen failed.  Maybe should give error (optionally), but for
           now be silent, to avoid annoying people who purposefully
           don't have the script installed. */
        ret = NULL;
    }
  else
    ret = NULL; /* This MakeTeX... program was disabled.  */
  
  return ret;
}

/* Return true magstep N for resolution BDPI.  From dvips.  */

static int
magstep P2C(int, n,  int, bdpi)
{
   register double t;
   int step;
   int neg = 0;

   if (n < 0) {
      neg = 1;
      n = -n;
   }
   if (n & 1) {
      n &= ~1;
      t = 1.095445115;
   } else
      t = 1.0;
   while (n > 8) {
      n -= 8;
      t = t * 2.0736;
   }
   while (n > 0) {
      n -= 2;
      t = t * 1.2;
   }

   step = 0.5 + (neg ? bdpi / t : bdpi * t);
   return step;
}

/* Tom Rokicki wrote this code for dvips.  It is in kpathsea now so that
   we can be sure xdvik and dvipsk use the same code to compute this.

      His comments:

      Here we want to return a string.  If we can find some integer
      m such that floor(0.5 + bdpi * 1.2 ^ (m/2)) = dpi, we write out
         magstep(m/2)
      where m/2 is a decimal number; else we write out
         dpi/bdpi
      We do this for the very slight improvement in accuracy that
      magstep() gives us over the rounded dpi/bdpi.

   Instead of returning a string, we set the envvar MAKETEX_MAG, which
   is part of the default spec for MakeTeXPK above. (That's why this
   routine is in this source file.)
   
   We allow +-1 when checking if DPI is a given magstep, so that DPI
   values of 328, 329, and 330 will all translate to magstep(.5).
   Floating-point computation with the DVI scale factors leads to 328.  */

/* Don't bother trying to use fabs or some other ``standard'' routine
   which can only cause trouble; just roll our own simple-minded
   absolute-value function that is all we need.  */
#define ABS(expr) ((expr) < 0 ? -(expr) : (expr))

void
kpse_set_maketex_mag P2C(int, dpi,  int, bdpi)
{
  int m, n;
  char q[1000];

  m = 0;
  if (dpi < bdpi) {
     for (;;) {
        m--;
        n = magstep(m, bdpi);
        if (ABS (n - dpi) <= 1)
           break;
        if (n < dpi || m < -40) {
           m = 9999;
           break;
        }
     }
  } else if (dpi > bdpi) {
     for (;;) {
        m++;
        n = magstep(m, bdpi);
        if (ABS (n - dpi) <= 1)
           break;
        if (n > dpi || m > 40) {
           m = 9999;
           break;
        }
     }
  }
#if defined (DOS) || defined (OS2)
  {
    double t;

  /* write out magnification as decimal number ... why? --karl */
    if (m == 9999) {
       t = (double)dpi/bdpi;
    } else {
       if (m < 0)
            n = -m;
       else
            n = m;
       if (n & 1) {
            n &= ~1;
            t = 1.095445115;
       } else
            t = 1.0;
       while (n > 0) {
            n -= 2;
            t = t * 1.2;
       }
       if (m < 0)
            t = 1 / t;
    }
    sprintf(q, "%12.9f", t);
  }
#else /* not DOS or OS2 */
  if (m == 9999) {
     sprintf(q, "%d+%d/%d", dpi/bdpi, dpi%bdpi, bdpi);
  } else if (m >= 0) {
     sprintf(q, "magstep\\(%d.%d\\)", m/2, (m&1)*5);
  } else {
     sprintf(q, "magstep\\(-%d.%d\\)", (-m)/2, (m&1)*5);
  }
#endif /* not DOS or OS2 */

  xputenv ("MAKETEX_MAG", q);
}

/* I don't think it makes much difference whether we check if DPI2 can
   tolerate DPI1 or the reverse.  */

boolean
kpse_check_bitmap_tolerance P2C(double, dpi1,  double, dpi2)
{
  unsigned tolerance = KPSE_BITMAP_TOLERANCE (dpi2);
  unsigned lower_bound = (int) (dpi2 - tolerance) < 0 ? 0 : dpi2 - tolerance;
  unsigned upper_bound = dpi2 + tolerance;

  return lower_bound <= dpi1 && dpi1 <= upper_bound;
}

#ifdef TEST

void
test_make_tex (kpse_file_format_type fmt, const_string base_file)
{
  string answer;
  
  printf ("\nAttempting %s in format %d:\n", base_file, fmt);

  answer = kpse_make_tex (fmt, base_file);
  puts (answer ? answer : "(null)");
}


int
main ()
{
  xputenv ("MAKETEX_DPI", "781"); /* call MakeTeXPK */
  xputenv ("MAKETEX_BASE_DPI", "300"); /* call MakeTeXPK */
  xputenv ("MAKETEX_MAG", "781/300"); /* call MakeTeXPK */
  KPSE_MAKE_SPEC_ENABLED (kpse_make_specs[kpse_pk_format]) = true;
  test_make_tex (kpse_pk_format, "cmr10");

  /* Fail with MakeTeXTFM.  */
  KPSE_MAKE_SPEC_ENABLED (kpse_make_specs[kpse_tfm_format]) = true;
  test_make_tex (kpse_tfm_format, "foozler99");
  
  /* Call something disabled.  */
  test_make_tex (kpse_bst_format, "no-way");
  
  return 0;
}

#endif /* TEST */


/*
Local variables:
test-compile-command: "gcc -posix -g -I. -I.. -DTEST tex-make.c kpathsea.a"
End:
*/
