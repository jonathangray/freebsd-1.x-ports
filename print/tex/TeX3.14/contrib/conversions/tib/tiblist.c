/*
        Tiblist - list references for tib system

                                                        */
#include <stdio.h>
#include <ctype.h>
#include "tib.h"
#define MAXLIST 2000  /* maximum number of references that can be listed */
#define getch(c,fd) (c = getc(fd))

FILE *tfd;                      /* temporary file position */
char tmp_file[120];              /* temporary file (see tib.h) */
FILE *rfd;                      /* reference file position */
char reffile[120];              /* temporary file (see tib.h) */
long int refspos[MAXLIST];      /* references temporary file, seek positions */
long int rend = 1;              /* last used position in reference file */
int numrefs = -1;               /* number of references */
char *citestr[MAXLIST];         /* citation strings */
extern int sort;                /* see if things are to be sorted */
int unlmark[MAXLIST];           /* underline flags */
extern char bibfname[], citetemplate[];
extern int biblineno;
extern int underline, silent, stndout;
extern char dirsp2[], optch2[], suffix[];

main(argc, argv)
   int argc;
   char **argv;
{  int  i, rcomp1();
   char *outname;
   char *outfname();
   int undrline();
   FILE *ofd;
   int c;
   int strcmp();
   char *pdefst, *getenv(), *ptemp, argchk[3];

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
      fprintf (stderr, "Tiblist -- version %s, released %s.\n",
             VERSION, RDATE);

   /* get file names from environment */

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

   mktemp(reffile);
   rfd = fopen(reffile,"w+");
   if (rfd == NULL)
      error("can't open temporary reference file");
   putc('x', rfd);      /* put garbage in first position */
   mktemp(tmp_file);
   tfd = fopen(tmp_file,"w");
   if (tfd == NULL)
      error("can't open temporary output file");

   strcpy(suffix,".ref");
   pdefst = getenv("DEFSTYLE");
   if (pdefst == NULL)
      doargs(argc, argv, DEFSTYLE);
   else
      doargs(argc, argv, pdefst);

   if (silent == 0)
      fprintf(stderr, "Processing reference list ...\n");
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

   if (stndout == true && bibfname != "<stdin>") {
      while (getch(c,tfd) != EOF)
         putc(c, stdout);
      for (i = 0; i <= numrefs; i++)
         dumpref(i, stdout);
      fprintf(stdout, "%cbye\n", '\\');
      }
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
          while (getch(c,tfd) != EOF)
             putc(c, ofd);
          for (i = 0; i <= numrefs; i++)
             dumpref(i, ofd);
          fprintf(ofd, "%cbye\n", '\\');
          fclose (ofd);
          fprintf (stderr,"Output of tiblist in %s.\n", outname);
      }
   }
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

/* rdtext - process a file */
   rdtext(ifile)
   FILE *ifile;
{  char c, *p, rec[REFSIZE];
   int i;
   int sortdefs();

   sortdefs();
   biblineno = 1;
   for (;;) {
      while (getch(c, ifile) == '\n')
         biblineno++;   /* skip leading newlines */
      if (c == EOF)
         return;

      p = rec;          /* read a reference */
      for (;;) {
         for (*p++ = c; getch(c, ifile) != '\n'; )
            if (c == EOF)
               error("ill formed reference file");
            else
               *p++ = c;
         if (getch(c, ifile) == '\n' || c == EOF) {
            biblineno++;
            *p++ = '\n';
            break;
            }
         if (c == '.' || c == '%')
            *p++ = '\n';
         else
            *p++ = ' ';
         }

      *p = 0;
      expnd2(rec);

      if (numrefs++ > MAXLIST)
         error("too many references");
      refspos[numrefs] = rend;
#ifdef READWRITE
      fixrfd( WRITE );          /* fix access mode of rfd, if nec. */
#else
      fseek(rfd, rend, 0);
#endif
      i = strlen(rec) + 1;
      fwrite(rec, 1, i, rfd);
      rend = rend + i;
      }
}

