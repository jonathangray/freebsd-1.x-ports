/*
        Tib - TeX bibliographic formatter

        Written by J.C. Alexander, University of Maryland
                based on "bib," a troff formatter,
                      written by Tim Budd, University of Arizona.  */

#include <stdio.h>
#include <ctype.h>
#include "tib.h"

#define getch(c,fd) (c = getc(fd))
#define echoc(c,ifd,ofd) (getch(c,ifd) == EOF ? c : putc(c,ofd))
#define testc(c,d,ifd,ofd) (getch(c, ifd) == d ? putc(c, ofd) : 0)

/* global variables */
   FILE *rfd;                   /* reference temporary file              */
   char reffile[120];           /* temporary file (see tib.h)            */
   long int refspos[MAXREFS];   /* reference seek positions              */
   int unlmark[MAXREFS];        /* underline flags                       */
   long int rend = 1;           /* last position in rfd (first char unused)*/
   int numrefs = -1;            /* number of references generated so far */
   FILE *tfd;                   /* output of pass 1 of file(s)           */
   char tmp_file[120];           /* output of pass 1                    */
   char common[] = COMFILE;     /* common word file                      */
   char *citestr[MAXREFS];      /* citation strings                      */
   int  findex = false;         /* can we read the file INDEX ?          */

/* global variables in tibargs */
   extern int foot, sort, personal, underline, silent, loccit, ibidflag[];
   extern int hyphen, ordcite, biblineno, stndout, locflag[], optch2[], c0;
   extern char sortstr[], pfile[], citetemplate[], bibfname[], dirsp2[];

main(argc, argv)
   int argc;
   char **argv;
{  int rcomp1();
   FILE *ofd;                   /* output file                           */
   char *outname;
   char *outfname(), *getenv(), *pcom, *pdefst, *ptemp;
   int undrline();
   char headerline[240];
   char *indx = INDXFILE;
   char *tfgets(), argchk[3];
   int i, strcmp(), clnup();

   /* header */
   for (i = 1; i < argc; i++) {
      strcpy(argchk,OPTCH);
      strcat(argchk,"z");
      if (strcmp(argv[i],argchk) == 0)
          silent = true;
      strcpy(argchk,OPTCH);
      strcat(argchk,"|");
      if (strcmp(argv[i],argchk) == 0)
         silent = true;
   }
   if (silent == false)
      fprintf (stderr, "Tib -- version %s, released %s.\n", VERSION, RDATE);

   /* the file INDEX in the current directory is the default index,
      if it is present */

   rfd = fopen( INDXFILE , "r");
   if (rfd != NULL) {
      findex = true;
      tfgets(headerline,240,rfd);      
      fclose(rfd);
      chkindex(indx, headerline, false);
      }

   /* get file names from environment */

   pcom = getenv("COMFILE");
   if (pcom != NULL)
      strcpy(common,pcom);
   ptemp = getenv("TMPDIR");
   if (ptemp != NULL) {
      strcpy(reffile,ptemp);
      strcat(reffile,dirsp2);
      strcat(reffile,"tibrXXXXXX");
      strcpy(tmp_file,ptemp);
      strcat(tmp_file,dirsp2);
      strcat(tmp_file,"tibpXXXXXX");
   }
   else {
      strcat(reffile,TMPREFFILE);
      strcat(tmp_file,TMPTEXTFILE);
   }

   /* open temporaries, reffile will contain references collected in
      pass 1, and tmp_file will contain text. */

   mktemp(reffile);
   rfd = fopen(reffile,"w+");
   if (rfd == NULL)
      error("can't open temporary reference file");
   putc('x', rfd);      /* put garbage in first position (not used) */
   mktemp(tmp_file);
   tfd = fopen(tmp_file,"w");
   if (tfd == NULL)
      error("can't open temporary output file");

    /*
       pass1 - read files, looking for citations
               arguments are read by doargs (tibargs.c)
    */

   pdefst = getenv("DEFSTYLE");
   if (pdefst == NULL) {
      if (doargs(argc, argv, DEFSTYLE ) == 0) {
         strcpy(bibfname, "<stdin>");
         rdtext(stdin);
         }
   }
   else {
      if (doargs(argc, argv, pdefst ) == 0) {
         strcpy(bibfname, "<stdin>");
         rdtext(stdin);
         }
   }

   /*
    sort references, make citations, add disambiguating characters
   */

   if (silent == 0)
      fprintf(stderr, "Processing citations ...\n");
   if (sort)
      qsort(refspos, numrefs+1, sizeof(long), rcomp1);
   makecites(citestr);
   if (underline)
      undrline();
   if (citetemplate[0] != '0')
      disambiguate();

   /*
   reopen temporaries
   */

   fclose(tfd);
   tfd = fopen(tmp_file,"r");
   if (tfd == NULL)
      error("can't open temporary output file for reading");

   /*
   pass 2 - reread files, replacing references
   */

   if (stndout == true && bibfname != "<stdin>")
      pass2(tfd, stdout);
   else {
      outname = outfname (bibfname);
      ofd = fopen (outname, "w");
      if (ofd == NULL) {
          fprintf(stderr,"can't open %s\n", outname);
          clnup();
          exit(1);
          }
      else {
          if (silent == 0)
             fprintf(stderr, "Writing output file ...\n");
          pass2 (tfd, ofd);
          fprintf(ofd,"\n");
          fclose (ofd);
          fprintf (stderr,"Output of tib in %s.\n", outname);
      }
   }
   fclose(tfd);
   fclose(rfd);
   clnup();
   exit(0);
}

   /*   clean up   */
   clnup()
{
   unlink(tmp_file);
   unlink(reffile);
   return;
}

/* rdtext - read and process a text file, looking for [. commands */
   rdtext(fd)
   FILE *fd;
{  char lastc, c, d;
   char lastlc;
   int sortdefs();
   int percentflag; /* is there % so far on line? */

   sortdefs();
   lastc = '\0';
   lastlc = '\0';
   percentflag = false;
   biblineno = 1;
   fprintf(tfd,"%cmessage {DOCUMENT TEXT}\n",'\\');
   while (getch(c, fd) != EOF) {
      if (c == '\%')
         if (percentflag == false)
            if (lastc != '\\') {
               percentflag = true;
               if (isalpha(lastc) == 0)
                  if (lastc != '\n')
                     if (lastc != '\0')
                        if (lastc != ']')
                           if (lastc != '>') {
                              lastlc = lastc;
                              lastc = '\0';
                              }
               }
      if ((c == '[' || c == '<') && lastc != '\\')
          if (getch(d, fd) == '.') { /* found a reference */
            if (c == '<') { if (lastc) fprintf(tfd,"%c",lastc);}
            else
               switch (lastc) {
                  case '\0':
                  case ' ': fprintf(tfd,"%cLspace ",'\\'); break;
                  case '.': fprintf(tfd,"%cLperiod ",'\\'); break;
                  case ',': fprintf(tfd,"%cLcomma ",'\\'); break;
                  case '?': fprintf(tfd,"%cLquest ",'\\'); break;
                  case ':': fprintf(tfd,"%cLcolon ",'\\'); break;
                  case ';': fprintf(tfd,"%cLscolon ",'\\'); break;
                  case '!': fprintf(tfd,"%cLbang ",'\\'); break;
                  case '\'': fprintf(tfd,"%cLquote ",'\\'); break;
                  case '"': fprintf(tfd,"%cLqquote ",'\\'); break;
                  case '`': fprintf(tfd,"%cLrquote ",'\\'); break;
                  default:  fprintf(tfd,"%c",lastc); break;
                  }
            rdcite(fd, c);
            if (c == '[')
               switch (lastc) {
                  case '\0': break;
                  case ' ': fprintf(tfd,"%cRspace{}",'\\'); break;
                  case '\n': fprintf(tfd,"%cRspace{}",'\\'); break;
                  case '.': fprintf(tfd,"%cRperiod{}",'\\'); break;
                  case ',': fprintf(tfd,"%cRcomma{}",'\\'); break;
                  case '?': fprintf(tfd,"%cRquest{}",'\\'); break;
                  case ':': fprintf(tfd,"%cRcolon{}",'\\'); break;
                  case ';': fprintf(tfd,"%cRscolon{}",'\\'); break;
                  case '!': fprintf(tfd,"%cRbang{}",'\\'); break;
                  case '\'': fprintf(tfd,"%cRquote{}",'\\'); break;
                  case '"': fprintf(tfd,"%cRqquote{}",'\\'); break;
                  case '`': fprintf(tfd,"%cRrquote{}",'\\'); break;
                  }
            lastc = '\0';
            }
         else {
            if (lastc != '\0') putc(lastc, tfd);
            ungetc(d, fd);
            lastc = c;
            }
      else {
         if (lastc != '\0') putc(lastc, tfd);
         lastc = c;
         if (c == '\n') {
            biblineno++;
            percentflag = false;
            if (lastlc != '\0') {
               putc(c,tfd);
               lastc = lastlc;
               lastlc = '\0';
               }
            }
         }
      }
   if (lastc != '\0') putc(lastc, tfd);
}

/* rdcite - read citation information inside a [. command */
   rdcite(fd, ch)
   FILE *fd;
   char ch;
{  long int n, getref();
   char huntstr[HUNTSIZE], c, info[HUNTSIZE], footch[2], c0ch[2];

   if (foot)
     strcpy(footch,"\n");
   else
     strcpy(footch,"");
   if (c0)
     strcpy(c0ch,"\n");
   else
     strcpy(c0ch,"");
   if (ch == '[')
      fprintf(tfd,"%cLcitemark ", '\\');
   else
      fprintf(tfd,"%cLAcitemark ", '\\');
   huntstr[0] = info[0] = 0;
   while (getch(c, fd) != EOF)
      switch (c) {
         case ',':
            n = getref(huntstr);
            if (n > 0)
               fprintf(tfd, "%c%ld%c%s%c", CITEMARK, n, CITEMARK, info, 
                                                         CITEEND);
            else
               fprintf(tfd, "%c0%c%s%s%c", CITEMARK, CITEMARK,
                                           huntstr, info, CITEEND);
            huntstr[0] = info[0] = 0;
            break;

         case '.':
            while (getch(c, fd) == '.') ;
            if (c == ']') {
               n = getref(huntstr);
               if (n > 0) {
                  fprintf(tfd, "%c%ld%c%s%c%s%cRcitemark %s", CITEMARK, n,
                         CITEMARK, info, CITEEND, c0ch,'\\',footch);
/*                  if (foot) {
                     getch(c, fd);
                     if (c == ' '|| c== '\n')
                        fprintf(tfd, "%c ", '\\');
                     else
                        putc(c, tfd);
                     fprintf(tfd, "\n");
                     }*/
                  }
               else {
                  fprintf(tfd, "%c0%c%s%s%c%s%cRcitemark%s ", CITEMARK, CITEMARK,
                                huntstr, info, CITEEND,c0ch, '\\',footch);
/*                  if (foot) {
                     getch(c, fd);
                     if (c == ' '|| c== '\n')
                        fprintf(tfd, "%c ", '\\');
                     else
                        putc(c, tfd);
                     fprintf(tfd, "\n");
                     }*/
                  }                 
               return;
               }
            else if (c == '>') {
               n = getref(huntstr);
               if (n > 0) {
                  fprintf(tfd, "%c%ld%c%s%c%s%cRAcitemark{}%s", CITEMARK, n,
                               CITEMARK, info, CITEEND,c0ch,'\\',footch);
                  }
               else {
                  fprintf(tfd, "%c0%c%s%s%c%s%cRAcitemark{}%s", CITEMARK, 
                             CITEMARK,huntstr, info, CITEEND,c0ch,'\\',footch);
                  }
               return;
               }
            else
               addc(huntstr, c);
            break;

         case '<':
            strcat(info, "\\LIcitemark{}");
            while (getch(c, fd) != '>')
               if (c == EOF) {
                  fprintf(stderr, "Error: ill formed reference\n");
                  clnup();
                  exit(1);
                  }
               else
                  addc(info, c);
            strcat(info, "\\RIcitemark ");
            break;

         case '\n':
            biblineno++;
         case '\t':
            c = ' ';   /* fall through */

         default:
            addc(huntstr,c);
         }
   error("end of file reading citation");
}

/* addc - add a character to hunt string */
   addc(huntstr, c)
   char huntstr[HUNTSIZE], c;
{  int  i;

   i = strlen(huntstr);
   if (i > HUNTSIZE)
      error("citation too long");
   huntstr[i] = c;
   huntstr[i+1] = 0;
}

/* getref - if an item was already referenced, return its pointer in
                the reference file, otherwise create a new entry */
   long int getref(huntstr)
   char huntstr[HUNTSIZE];
{  char rf[REFSIZE], ref[REFSIZE], *r, *hunt();
   int  i, match(), getwrd();

   r = hunt(huntstr);
   if (r != NULL) {
      /* expand defined string */
      strcpy(rf, r);
      free(r);
      expnd2(rf);

      /* see if reference has already been cited */

      if (foot == false)
         for (i = 0; i <= numrefs; i++) {
             rdref(refspos[i], ref);
             if (strcmp(ref, rf) == 0)
                return(refspos[i]);
          }

      if (loccit) {
         ibidflag[numrefs+1] = false;
         locflag[numrefs+1] = false;
         for (i = 0; i <= numrefs; i++) {
             rdref(refspos[i], ref);
             if (strcmp(ref, rf) == 0) {
                locflag[numrefs+1] = true;
                break;
             }		   
         }
      rdref(refspos[numrefs], ref);
      if (strcmp(ref, rf) == 0)
         ibidflag[numrefs+1] = true;
      }

      /* didn't match any existing reference, create new one */

      numrefs++;
      refspos[numrefs] = rend;
#ifdef READWRITE
      fixrfd( WRITE );                 /* fix access mode of rfd, if nec. */
#else
      fseek(rfd, rend, 0);             /* go to end of rfd */
#endif
      i = strlen(rf) + 1;
      fwrite(rf, 1, i, rfd);
      rend = rend + i;
      return(refspos[numrefs]);
      }
   else {
      bibwarning("no reference matching %s\n", huntstr);
      return( (long) -1 );
      }
}

/* hunt - hunt for reference from either personal or system index */
   char *hunt(huntstr)
   char huntstr[];
{  char *fhunt(), *r, *p, *q, fname[120];
   char *getenv(), *pindx;

   if (personal) {
      for (p = fname, q = pfile; ; q++)
         if (*q == ',' || *q == 0) {
            *p = 0;
            if ((r = fhunt(fname, huntstr)) != NULL)
               return(r);
            else if (*q == 0)
               break;
            p = fname;
            }
         else *p++ = *q;
      }
   else if (findex) {
      if ((r = fhunt( INDXFILE , huntstr)) != NULL)
         return(r);
      }
   pindx = getenv("SYSINDEX");
   if (pindx == NULL) {
      if ((r = fhunt(SYSINDEX , huntstr)) != NULL)
         return(r);
      }
   else {
      if ((r = fhunt(pindx , huntstr)) != NULL)
         return(r);
      }
   return(NULL);
}

/* fhunt - hunt from a specific file */
   char *fhunt(file, huntstr)
   char file[], huntstr[];
{  char *p, *r, *locate();

   r = locate(huntstr, file, 6, common);

   if (r == NULL)
      return(NULL);  /* error */
   if (*r == 0)
      return(NULL);  /* no match */

   for (p = r; *p; p++)
      if (*p == '\n')
         if (*(p+1) == '\n') { /* end */
            if (*(p+2) != 0)
               bibwarning("multiple references match %s\n",huntstr);
            *(p+1) = 0;
            break;
            }
         else if (*(p+1) != '%' && *(p+1) != '.') /* unnecessary newline */
            *p = ' ';
   return(r);
}

/* putrefs - gather contiguous references together, sort them if called
   for, hyphenate if necessary, and dump them out */
int putrefs(ifd, ofd, footrefs, fn)
FILE *ifd, *ofd;
int  fn, footrefs[];
{  int  citenums[MAXATONCE];   /* reference numbers */
   char *citeinfo[MAXATONCE];  /* reference information */
   char infoword[HUNTSIZE];    /* information line */
   int  rtop, n, i, j;         /* number of citations being dumped */
   char c, *p, *walloc();

/* first gather contiguous references together, and order them if
   required      */

   rtop = -1;
   do {
      n = 0;
      while (isdigit(getch(c, ifd)))
         n = 10 * n + (c - '0');
      if (c ^= CITEMARK)
         error("inconsistant citation found in pass two");
      if (n == 0) {     /* reference not found */
         rtop++;
         j = rtop;
         citenums[j] = -1;
         citeinfo[j] = 0;
         }
      else {
         for (i = 0; i <= numrefs; i++)
            if (refspos[i] == n) { /* its the ith item in reference list */
               rtop++;
               j = rtop;
               if (ordcite)
                  for ( ; j > 0 && citenums[j-1] > i; j--) {
                     citenums[j] = citenums[j-1];
                     citeinfo[j] = citeinfo[j-1];
                     }
               citenums[j] = i;
               citeinfo[j] = 0;
               break;
               }
         if (i > numrefs)
            error("citation not found in pass two");
         }
      if (getch(c, ifd) != CITEEND) {
         for (p = infoword; c != CITEEND ; ) {
            *p++ = c;
            getch(c, ifd);
            }
         *p = 0;
         citeinfo[j] = walloc(infoword);
         }
      getch(c, ifd);
      }  while (c == CITEMARK);
   ungetc(c, ifd);

   /* now dump out values */
   for (i = 0; i <= rtop; i++) {
      if (citenums[i] >= 0)
         fputs(citestr[citenums[i]], ofd);
      if (citeinfo[i]) {
         fputs(citeinfo[i], ofd);
         free(citeinfo[i]);
         }
      if (hyphen) {
         for (j = 1; j + i <= rtop && citenums[i+j] == citenums[i] + j; j++);
         if (j + i > rtop) j = rtop;
         else j = j + i - 1;
         }
      else
         j = i;
      if (j > i + 1) {
         fputs("\\Citehyphen ", ofd);
         i = j - 1;
         }
      else if (i != rtop)
         if (!c0) fputs("\\Citecomma\n", ofd);
      if ((foot) || (c0)) {
         fn++;
         footrefs[fn] = citenums[i];
         }
      }
   return(fn);
}

/* pass2 - read pass 1 files entering citation */
   pass2(ifd, ofd)
   FILE *ifd, *ofd;
{
   char c, d, e;
   int  i, fn, footrefs[25], dumped;

   fn = -1;
   if ((foot) || (c0)) dumped = true;
      else dumped = false;
   while (getch(c, ifd) != EOF) {
      while (c == '\n') {
         putc(c, ofd);
         if (((foot) || (c0)) && fn >= 0) {
            for (i = 0; i <= fn; i++)
                dumpref(footrefs[i], ofd);
            fn = -1;
            }
         if (getch(c,ifd) != '.')
            ;
         else if (getch(d,ifd) != '[')
            ungetc(d,ifd);
         else if (getch(e,ifd) != ']') {
            ungetc(e,ifd);
            ungetc(d,ifd);}
         else if ((foot == false) && (c0 == false)) {
            while (echoc(c, ifd, ofd) != '\n')
               ;
            dumped = true;
            fprintf(ofd,"\\message{REFERENCE LIST}\n\n");
            for (i = 0; i <= numrefs; i++)
               dumpref(i, ofd);
            getch(c, ifd);
            }
         }
      if (c == CITEMARK)
         fn = putrefs(ifd, ofd, footrefs, fn);
      else if (c != EOF)
         putc(c, ofd);
      }
   if (dumped == false)
      bibwarning("Warning: reference list not made -- .[] not encountered\n","");
}
