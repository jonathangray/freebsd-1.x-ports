/*   For use with emTeX set FONTPATH to "TEXTFM"
 */
#ifndef FONTPATH
#define FONTPATH "TEXFONTS"
#endif

/*
 *   This code reads in and handles the defaults for the program from the
 *   file config.sw.  This entire file is a bit kludgy, sorry.
 */
#include "dvips.h" /* The copyright notice in that file is included too! */
#include "paths.h"
#include <kpathsea/c-pathch.h>
#include <kpathsea/default.h>
#include <kpathsea/init-path.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/tex-make.h>

/*
 *   This is the structure definition for resident fonts.  We use
 *   a small and simple hash table to handle these.  We don't need
 *   a big hash table.
 */
struct resfont *reshash[RESHASHPRIME] ;
/*
 *   These are the external routines we use.
 */
extern void error() ;
extern integer scalewidth() ;
extern int tfmload() ;
extern FILE *search() ;
extern shalfword pkbyte() ;
extern integer pkquad() ;
extern integer pktrio() ;
extern Boolean pkopen() ;
extern char *getenv() ;
extern char *newstring() ;
extern int add_header() ;
extern int add_name() ;
extern char *get_name() ;
extern int system() ;
extern void handlepapersize() ;
extern void checkstrings() ;
void getpsinfo() ;
extern void *revlist() ;
/*
 *   These are the external variables we use.
 */
#ifdef DEBUG
extern integer debug_flag;
#endif  /* DEBUG */
extern integer pagecopies ;
extern int overridemag ;
extern long bytesleft ;
extern quarterword *raster ;
extern FILE *pkfile ;
extern char *oname ;
extern Boolean downloadpspk ;
extern integer swmem, fontmem ;
extern string tfmpath, pictpath, pkpath, vfpath, figpath, configpath ;
extern Boolean noenv ;
#ifdef FONTLIB
extern char *flipath, *fliname ;
#endif
extern string headerpath ;
extern char *paperfmt ; 
extern char *nextstring ;
extern char *maxstring ;
extern char *warningmsg ;
extern Boolean disablecomments ;
extern Boolean compressed ;
extern int quiet ;
extern int filter ;
extern Boolean reverse ;
extern Boolean usesPSfonts ;
extern Boolean nosmallchars ;
extern Boolean removecomments ;
extern Boolean safetyenclose ;
extern Boolean dopprescan ;
extern integer maxsecsize ;
extern integer mag ;
extern Boolean sepfiles ;
extern int actualdpi ;
extern int vactualdpi ;
extern int maxdrift ;
extern int vmaxdrift ;
extern char *printer ;
extern char *mfmode ;
extern Boolean sendcontrolD ;
extern unsigned lastresortsizes[] ;
extern integer hoff, voff ;
extern struct papsiz *papsizes ;
extern Boolean secure ;
extern integer hpapersize, vpapersize ;
extern int landscape ;
/*
 *   To maintain a list of document fonts, we use the following
 *   pointer.
 */
struct header_list *ps_fonts_used ;
/*
 *   Our hash routine.
 */
int
hash(s)
   char *s ;
{
   int h = 12 ;

   while (*s != 0)
      h = (h + h + *s++) % RESHASHPRIME ;
   return(h) ;
}
/*
 *   Reverse the hash chains.
 */
void
revpslists() {
   register int i ;
   for (i=0; i<RESHASHPRIME; i++)
      reshash[i] = (struct resfont *)revlist(reshash[i]) ;
}
/*
 *   cleanres() marks all resident fonts as not being yet sent.
 */
void
cleanres() {
   register int i ;
   register struct resfont *p ;
   for (i=0; i<RESHASHPRIME; i++)
      for (p=reshash[i]; p; p=p->next)
         p->sent = 0 ;
}
/*
 *   The routine that looks up a font name.
 */
struct resfont *
lookup(name)
   char *name ;
{
   struct resfont *p ;

   for (p=reshash[hash(name)]; p!=NULL; p=p->next)
      if (strcmp(p->Keyname, name)==0)
         return(p) ;
   return(NULL) ;
}
/*
 *   This routine adds an entry.
 */
void
add_entry(TeXname, PSname, specinfo, downloadinfo)
   char *TeXname, *PSname, *specinfo, *downloadinfo ;
{
   struct resfont *p ;
   int h ;

   if (PSname == NULL)
      PSname = TeXname ;
   p = (struct resfont *)mymalloc((integer)sizeof(struct resfont)) ;
   p->Keyname = TeXname ;
   p->PSname = PSname ;
   p->TeXname = TeXname ;
   p->specialinstructions = specinfo ;
   if (downloadinfo && *downloadinfo)
      p->downloadheader = downloadinfo ;
   else
      p->downloadheader = 0 ;
   h = hash(TeXname) ;
   p->next = reshash[h] ;
   p->sent = 0 ;
   reshash[h] = p ;
}
/*
 *   Now our residentfont routine.  Returns the number of characters in
 *   this font, based on the TFM file.
 */
extern char *infont ;
int
residentfont(curfnt)
        register fontdesctype *curfnt ;
{
   register shalfword i ;
   struct resfont *p ;

/*
 *   First we determine if we can find this font in the resident list.
 */
   if (*curfnt->area)
      return 0 ; /* resident fonts never have a nonstandard font area */
   if ((p=lookup(curfnt->name))==NULL)
      return 0 ;
/*
 *   This is not yet the correct way to do things, but it is useful as it
 *   is so we leave it in.  The problem:  if resident Times-Roman is
 *   re-encoded, then it will be downloaded as bitmaps; this is not
 *   right.  The solution will be to introduce two types of `<'
 *   directives, one that downloads fonts and one that downloads
 *   short headers that are innocuous.
 */
   if (p->downloadheader && downloadpspk) {
#ifdef DEBUG
      if (dd(D_FONTS))
         (void)fprintf(stderr,"Using PK font %s for <%s>.\n",
                                     curfnt->name, p->PSname) ;
#endif  /* DEBUG */
      return 0 ;
   }
/*
 *   We clear out some pointers:
 */
#ifdef DEBUG
   if (dd(D_FONTS))
        (void)fprintf(stderr,"Font %s <%s> is resident.\n",
                                     curfnt->name, p->PSname) ;
#endif  /* DEBUG */
   curfnt->resfont = p ;
   curfnt->name = p->TeXname ;
   for (i=0; i<256; i++) {
      curfnt->chardesc[i].TFMwidth = 0 ;
      curfnt->chardesc[i].packptr = NULL ;
      curfnt->chardesc[i].pixelwidth = 0 ;
      curfnt->chardesc[i].flags = 0 ;
   }
   add_name(p->PSname, &ps_fonts_used) ;
/*
 *   We include the font here.  But we only should need to include the
 *   font if we have a stupid spooler; smart spoolers should be able
 *   to supply it automatically.
 */
   if (p->downloadheader) {
      char *cp = p->downloadheader ;
      char *q ;

      infont = p->PSname ;
      while (1) {
         q = cp ;
         while (*cp && *cp != ' ')
            cp++ ;
         if (*cp) {
            *cp = 0 ;
            add_header(q) ;
            *cp++ = ' ' ;
         } else {
            add_header(q) ;
            break ;
         }
         infont = 0 ;
      }
      infont = 0 ;
   }
   i = tfmload(curfnt) ;
   if (i < 0)
      i = 1 ;
   usesPSfonts = 1 ;
   return(i) ;
}
#define INLINE_SIZE (500)
static char was_inline[INLINE_SIZE] ;
void
bad_config() {
   error("Error in config file:") ;
   (void)fprintf(stderr, "%s\n", was_inline) ;
   exit(1) ;
}

/* WHO is the path from the config file we just read. WHAT is the
   default path.  Return path in new memory.  */
static char *getpath(who, what)
char *who, *what ;
{
  string ret = kpse_expand_default (who, what);
  if (ret == who)
    ret = xstrdup (who);
  return ret;
}
/*
 *   We use this function so we can support strings delimited by
 *   double quotes with spaces in them.  We also accept strings
 *   with spaces in them, but kill off any spaces at the end.
 */
char *configstring(s, nullok)
char *s ;
int nullok ;
{
   char tstr[300] ;
   char *p = tstr ;

   while (*s && *s <= ' ')
      s++ ;
   if (*s == '"') {
      s++ ;
      while (*s != 10 && *s != 0 && *s != '"' && p < tstr+290)
         *p++ = *s++ ;
   } else {
      while (*s && p < tstr+290)
         *p++ = *s++ ;
      while (*(p-1) <= ' ' && p > tstr)
         p-- ;
   }
   *p = 0 ;
   if (p == tstr && ! nullok)
      bad_config() ;
   return newstring(tstr) ;
}
/*
 *   Now we have the getdefaults routine.
 */
static char *psmapfile = PSMAPFILE ;
void
getdefaults(s)
char *s ;
{
   FILE *deffile ;
   char PSname[300] ;
   register char *p ;
   int i, j ;
   integer hsiz, vsiz ;
   int canaddtopaper = 0 ;

   if (printer == NULL) {
      if (s) {
         strcpy(PSname, s) ;
      } else {
         strcpy(PSname, DVIPSRC) ;
      }
   } else {
#if defined(MSDOS) || defined(OS2)
      strcpy(PSname, printer) ;
      strcat(PSname, ".cfg") ;
#else
      strcpy(PSname, "config.") ;
      strcat(PSname, printer) ;
#endif
   }
   if ((deffile=search(configpath,PSname,READ))!=NULL) {
      while (fgets(was_inline, INLINE_SIZE, deffile)!=NULL) {
/*
 *   We need to get rid of the newline.
 */
       for (p=was_inline; *p; p++) ;
       if (p > was_inline) *(p-1) = 0 ;
       if (was_inline[0] != '@')
          canaddtopaper = 0 ;
       switch (was_inline[0]) {
/*
 *   Handling paper size information:
 *
 *      If line is empty, then we clear out the paper size information
 *      we have so far.
 *
 *      If it is `@+', then we add to the current paper size info.
 *
 *      If it is `name hsize vsize', then we start a new definition.
 */
case '@' :
         p = was_inline + 1 ;
         while (*p && *p <= ' ') p++ ;
         if (*p == 0) {
            papsizes = 0 ; /* throw away memory */
         } else if (*p == '+') {
            if (canaddtopaper == 0)
               error(
      "can't have @+ in config file not immediately following a non-@ line") ;
            else {
               *(nextstring-1) = '\n' ;/* IBM: VM/CMS - changed 10 to "\n" */
               p++ ;
               while (*p && *p == ' ') p++ ;
               strcpy(nextstring, p) ;
               nextstring += strlen(p) + 1 ;
            }
         } else {
            struct papsiz *ps ;
            
            ps = (struct papsiz *)mymalloc((integer)sizeof(struct papsiz)) ;
            ps->next = papsizes ;
            papsizes = ps ;
            ps->name = p ;
            while (*p && *p > ' ')
               p++ ;
            *p++ = 0 ;
            ps->name = newstring(ps->name) ;
            while (*p && *p <= ' ') p++ ;
            handlepapersize(p, &hsiz, &vsiz) ;
            ps->xsize = hsiz ;
            ps->ysize = vsiz ;
            ps->specdat = nextstring++ ;
            canaddtopaper = 1 ;
         }
         break ;
case 'a' :
         dopprescan = (was_inline[1] != '0') ;
         break ;
case 'b':
#ifdef SHORTINT
         if (sscanf(was_inline+1, "%ld", &pagecopies) != 1) bad_config() ;
#else
         if (sscanf(was_inline+1, "%d", &pagecopies) != 1) bad_config() ;
#endif
         if (pagecopies < 1 || pagecopies > 1000)
            bad_config() ;
         break ;
case 'm' :
#ifdef SHORTINT
         if (sscanf(was_inline+1, "%ld", &swmem) != 1) bad_config() ;
#else   /* ~SHORTINT */
         if (sscanf(was_inline+1, "%d", &swmem) != 1) bad_config() ;
#endif  /* ~SHORTINT */
         swmem += fontmem ; /* grab headers we've seen already */
         break ;
case 'M' :
         mfmode = configstring(was_inline+1, 0) ;
         xputenv ("MAKETEX_MODE", mfmode);
         break ;
case 'o' :
         oname = configstring(was_inline+1, 1) ;
         if (*oname && oname[strlen(oname)-1] == ':')
            sendcontrolD = 1 ; /* if we send to a device, *we* are spooler */
         break ;
case 'O' :
         p = was_inline + 1 ;
         handlepapersize(p, &hoff, &voff) ;
         break ;
#ifdef FONTLIB
case 'L' : 
         {
            char tempname[300] ;
            extern char *fliparse() ;
            if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
            else {
               flipath = getpath(fliparse(PSname,tempname), flipath);
               fliname = newstring(tempname) ;
            }
	 }
         break ;
#endif
case 'T' : 
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else tfmpath = getpath(PSname, DEFAULT_TFM_PATH) ;
         break ;
case 'P' :
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else pkpath = getpath(PSname, DEFAULT_PK_PATH) ;
         break ;
case 'p' :
         p = was_inline + 1 ;
         while (*p && *p <= ' ')
            p++ ;
         if (*p == '+') {
            if (sscanf(p+1, "%s", PSname) != 1) bad_config() ;
            getpsinfo(PSname) ;
         } else {
            psmapfile = configstring(was_inline+1, 0) ;
         }
         break ;
case 'v' : case 'V' :
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else vfpath = getpath(PSname, DEFAULT_VF_PATH) ;
         break ;
case 'S' :
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else figpath = getpath(PSname, DEFAULT_FIG_PATH) ;
         break ;
case 's':
         safetyenclose = 1 ;
         break ;
case 'H' : 
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else headerpath = getpath(PSname, DEFAULT_HEADER_PATH) ;
         break ;
case '%': case ' ' : case '*' : case '#' : case ';' :
case '=' : case 0 : case '\n' :
         break ;
case 'r' :
         reverse = (was_inline[1] != '0') ;
         break ;
/*
 *   This case is for last resort font scaling; I hate this, but enough
 *   people have in no uncertain terms demanded it that I'll go ahead and
 *   add it.
 *
 *   This line must have numbers on it, resolutions, to search for the
 *   font as a last resort, and then the font will be scaled.  These
 *   resolutions should be in increasing order.
 *
 *   For most machines, just `300' is sufficient here; on the NeXT,
 *   `300 400' may be more appropriate.
 */
case 'R':
         i = 0 ;
         p = was_inline + 1 ;
         while (*p) {
            while (*p && *p <= ' ')
               p++ ;
            if ('0' <= *p && *p <= '9') {
               j = 0 ;
               while ('0' <= *p && *p <= '9')
                  j = 10 * j + (*p++ - '0') ;
               if (i > 0)
                  if (lastresortsizes[i-1] > j) {
                     error("last resort sizes (R) must be sorted") ;
                     bad_config() ;
                  }
               lastresortsizes[i++] = j ;
            } else {
               if (*p == 0)
                  break ;
               error("! only numbers expected on `R' line in config!") ;
            }
         }
         lastresortsizes[i] = 0 ;
         break ;
case 'D' :
         if (sscanf(was_inline+1, "%d", &actualdpi) != 1) bad_config() ;
         if (actualdpi < 10 || actualdpi > 10000) bad_config() ;
	 vactualdpi = actualdpi;
         break ;
/*
 *   Execute a command.  This can be dangerous, but can also be very useful.
 */
case 'E' :
#ifdef SECURE
         error("dvips was compiled with SECURE, which disables E in config") ;
#else
         if (secure) {
            error("dvips -R option used, which disables E in config") ;
            break ;
         }
         (void)system(was_inline+1) ;
#endif
         break ;
case 'K':
         removecomments = (was_inline[1] != '0') ;
         break ;
case 'U':
         nosmallchars = (was_inline[1] != '0') ;
         break ;
case 'W':
         for (p=was_inline+1; *p && *p <= ' '; p++) ;
         if (*p)
            warningmsg = newstring(p) ;
         else
            warningmsg = 0 ;
         break ;
case 'X' :
         if (sscanf(was_inline+1, "%d", &actualdpi) != 1) bad_config() ;
         if (actualdpi < 10 || actualdpi > 10000) bad_config() ;
         break ;
case 'Y' :
         if (sscanf(was_inline+1, "%d", &vactualdpi) != 1) bad_config() ;
         if (vactualdpi < 10 || vactualdpi > 10000) bad_config() ;
         break ;
case 'x': case 'y':
         if (sscanf(was_inline+1, "%d", &mag) != 1) bad_config() ;
         overridemag = (was_inline[0] == 'x') ? 1 : -1 ;
         break ;
case 'e' :
         if (sscanf(was_inline+1, "%d", &maxdrift) != 1) bad_config() ;
         if (maxdrift < 0) bad_config() ;
	 vmaxdrift = maxdrift;
         break ;
case 'q' : case 'Q' :
         quiet = (was_inline[1] != '0') ;
         break ;
case 'f' : case 'F' :
         filter = (was_inline[1] != '0') ;
         break ;
case 'h' : 
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else (void)add_header(PSname) ;
         break ;
case 'i' :
         if (sscanf(was_inline+1, "%d", &maxsecsize) != 1)
            maxsecsize = 0 ;
         sepfiles = 1 ;
         break ;
case 'I':
         noenv = (was_inline[1] != '0') ;
         break ;
case 'N' :
         disablecomments = (was_inline[1] != '0') ;
         break ;
case 'Z' :
         compressed = (was_inline[1] != '0') ;
         break ;
case 't' :
         if (sscanf(was_inline+1, "%s", PSname) != 1) bad_config() ;
         else {
           if (strcmp(PSname, "landscape") == 0) {
               if (hpapersize || vpapersize)
                  error(
            "both landscape and papersize specified; ignoring landscape") ;
               else
                  landscape = 1 ;
            } else
               paperfmt = newstring(PSname) ;
         }
         break ;
default:
         bad_config() ;
      }
     }
     (void)fclose(deffile) ;
   } else {
      if (printer)
        {
          char msg[1000];
          sprintf (msg, "! no config file for printer `%s' in path `%s'",
                   printer, configpath);
          error(msg);
        }
   }
}

/*
 *   If a character pointer is passed in, use that name; else, use the
 *   default (possibly set) name.
 */
void getpsinfo(name)
char *name ;
{
   FILE *deffile ;
   register char *p ;
   char *specinfo, *downloadinfo ;
   char downbuf[200] ;

   if (name == 0)
      name = psmapfile ;
   if ((deffile=search(configpath, name, READ))!=NULL) {
      while (fgets(was_inline, INLINE_SIZE, deffile)!=NULL) {
         p = was_inline ;
         if (*p > ' ' && *p != '*' && *p != '#' && *p != ';' && *p != '%') {
            char *TeXname = NULL ;
            char *PSname = NULL ;
            specinfo = NULL ;
            downloadinfo = NULL ;
            downbuf[0] = 0 ;
            while (*p) {
               while (*p && *p <= ' ')
                  p++ ;
               if (*p) {
                  if (*p == '"')
                     specinfo = p + 1 ;
                  else if (*p == '<') {
                     if (downloadinfo) {
                        strcat(downbuf, downloadinfo) ;
                        strcat(downbuf, " ") ;
                     }
                     downloadinfo = p + 1 ;
                  } else if (TeXname)
                     PSname = p ;
                  else
                     TeXname = p ;
                  if (*p == '"') {
                     p++ ;
                     while (*p != '"' && *p)
                        p++ ;
                  } else
                     while (*p > ' ')
                        p++ ;
                  if (*p)
                     *p++ = 0 ;
               }
            }
            if (downloadinfo)
               strcat(downbuf, downloadinfo) ;
            if (TeXname) {
               TeXname = newstring(TeXname) ;
               specinfo = newstring(specinfo) ;
               PSname = newstring(PSname) ;
               downloadinfo = newstring(downbuf) ;
               add_entry(TeXname, PSname, specinfo, downloadinfo) ;
            }
   	 }
      }
      (void)fclose(deffile) ;
   }
   checkstrings() ;
}

/* Print the path P with label TITLE.  */

static void
print_path (title, p)
    string title;
    string p;
{
  fprintf (stderr, "%s path: \t", title);
  if (p == NULL)
    fprintf (stderr, "(null)\n");
  else if (p == NULL)
    fprintf (stderr, "(empty)\n");
  else
    fprintf (stderr, "%s", p);
  putc ('\n', stderr);
}

/* Set all path variables.  This is called after we've read the config
   file, and we don't want to override paths from there with the
   defaults, hence the checks for variables being null.  */

void
checkenv(which)
    int which ;
{
  extern int dontmakefont;

   if (which) {
/* PATH is either the value from the config file or the compile-time
   default from its static initalization. It is both the current value
   (in case no envvars are set) and the default value (in case an envvar
   is set, and has an extra colon).  */
#define INIT_PATH(path, font_p, envs) \
  if (font_p && kpse_font_override_path) \
    path = kpse_expand_default (kpse_font_override_path, path); \
  else \
    path = kpse_init_path (path, path, envs, NULL)

     /* If envvars are set, override the current path settings.  In any
        case, expand default values.  */
     INIT_PATH (pkpath, true, KPSE_PK_ENVS);
     INIT_PATH (tfmpath, true, KPSE_TFM_ENVS);
     INIT_PATH (vfpath, true, KPSE_VF_ENVS);
     INIT_PATH (pictpath, false, DVIPS_PICT_ENVS);
     INIT_PATH (headerpath, false, DVIPS_HEADER_ENVS);
     INIT_PATH (figpath, false, KPSE_TEX_ENVS);

#ifdef DEBUG
     if (dd (D_PATHS))
       {
         print_path ("PK", pkpath);
         print_path ("TFM", tfmpath);
         print_path ("VF", vfpath);
         print_path ("pict", pictpath);
         print_path ("header", headerpath);
         print_path ("fig", figpath);
       }
#endif /* DEBUG */

     /* Might have a program-specific path.  */
     kpse_font_override_path = getenv ("DVIPSFONTS");
     
     /* So kpse_find_glyph_format will see the value we've carefully
        computed including the config file value.  Fortunately, no other
        kpathsea routines call kpse_init_path.  This is ugly, but a real
        fix will come when Kpathsea supports config files.  */
     if (!kpse_font_override_path)
       kpse_font_override_path = pkpath;

     /* Last-resort font and resolutions.  */
     kpse_fallback_font = "cmr10";
     if (!lastresortsizes[0] || getenv ("DVIPSSIZES") || getenv ("TEXSIZES"))
       kpse_init_fallback_resolutions ("DVIPSSIZES");
     else
       kpse_fallback_resolutions = lastresortsizes;

#ifdef DEBUG
     if (dd (D_FONTS))
       {
         unsigned i;
         fputs ("Last resort sizes:", stderr);
         for (i = 0; kpse_fallback_resolutions[i]; i++)
           {
             fprintf (stderr, " %u", kpse_fallback_resolutions[i]);
           }
         putc ('\n', stderr);
       }
#endif /* DEBUG */

     if (!dontmakefont)
       {
         /* Don't enable the MakeTeX program; we do that on a per-font
            basis.  See pkopen in loadfont.c.  */
         /* Might have a program-specific path for MakeTeXPK, too.  */
         if (getenv ("DVIPSMAKEPK"))
           KPSE_MAKE_SPEC_PROGRAM (kpse_make_specs[kpse_pk_format])
             = getenv ("DVIPSMAKEPK");

         /* The base DPI won't change during the run.  */
         xputenv ("MAKETEX_BASE_DPI", itoa ((unsigned) actualdpi));
       }
   } else {
      configpath
        = kpse_init_path (NULL, DEFAULT_CONFIG_PATH, "TEXCONFIG", NULL);
#ifdef DEBUG
      if (dd (D_PATHS))
        {
          print_path ("config", configpath);
        }
#endif
  }
}
