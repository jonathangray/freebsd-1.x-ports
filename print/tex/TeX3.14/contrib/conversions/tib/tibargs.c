/*
        tibargs

        read argument strings for tib and tiblist
        do name formatting, printing lines, other actions common to both
                                                        */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "tib.h"

/* global variables */
   char bibfname[120];          /* file name currently being read            */
   int  biblineno;              /* line number currently being referenced    */
   int  abbrev       = false;   /* automatically abbreviate names            */
   int  capsmcap     = false;   /* print names in caps small caps (CACM form)*/
   int  numrev       = 0;       /* number of authors names to reverse        */
   int  edabbrev     = false;   /* abbreviate editors names ?                */
   int  edcapsmcap   = false;   /* print editors in cap small caps           */
   int  ednumrev     = 0;       /* number of editors to reverse              */
   int  sort         = false;   /* sort references ? (default no)            */
   int  foot         = false;   /* footnoted references ? (default endnotes) */
   int  c0           = false;   /* references in text ? (default no)         */
   int  hyphen       = false;   /* hypenate contiguous references            */
   int  ordcite      = true;    /* order multiple citations                  */
   int  specflag     = false;   /* use special flags                         */
   int  loccit       = false;   /* use ibid and loccit in footnotes          */
   int  onepage      = false;   /* print only first page of reference        */
   int  underline    = false;   /* underline multiple references             */
   int  silent       = false;   /* run silently                              */
   int  stndout      = false;   /* output on stdout                          */
   char sortstr[80]  = "1";     /* sorting template                          */
   char trailstr[80] = "";      /* trailing characters to output             */
   char pfile[120];             /* private file name                         */
   char dirname[120] ="";       /* private directory name                    */
   int  personal = false;       /* personal file given ? (default no)        */
   char citetemplate[80] = "1"; /* citation template                         */
   char *words[MAXDEFS];        /* defined words                             */
   char *defs[MAXDEFS];         /* defined word definitions                  */
   long int defpos[MAXDEFS];    /* position of defined words                 */
   long int link[MAXDEFS];      /* link for linking defined words on input   */
   long int jtop = 0;           /* entry for linked list                     */
   long int wordtop = -1;       /* top of defined words array                */
   int locflag[MAXREFS];        /* loc cit ?                                 */
   int ibidflag[MAXREFS];       /* ibid ?                                    */
   char dirsp2[]=DIRSEP;        /* directory separator character             */
   char optch2[]=OPTCH;         /* option character on program calls         */
   char suffix[5] = ".tex";	/* for appending to file names               */
   int startflag = 1;		/* starting count of Flags */
   
/* where output goes */
   extern FILE *tfd;
/* reference file information */
   extern long int refspos[];
   extern int unlmark[];
   extern char reffile[];
   extern FILE *rfd;
   extern char *citestr[];
   extern int numrefs;

/* doargs - read command argument line for both tib and tiblist
            set switch values
            call rdtext on file arguments, after dumping
            default style file if no alternative style is given
*/
   int doargs(argc, argv, defstyle)
   int argc;
   char **argv, defstyle[];
{  int numfiles, i, style;
   char bibfnamet[120];
   int pipeflag = false;
   int fileflag = false;
   char *p, *q, *walloc();
   FILE *fd, *np;
   char headerline[240], fname[240];
   char *tfgets(), *ptmac, *getenv();

   if (silent == false)
      fprintf(stderr, "Processing format commands ...\n");
   numfiles = 0;
   style = true;
   ptmac = getenv("TMACLIB");
   if (ptmac == NULL)
      ptmac = TMACLIB;
   words[0] = walloc("TMACLIB");
   strcpy(headerline,ptmac);
   strcat(headerline,dirsp2);
   defs[0]  = walloc(headerline);
   wordtop++;
   link[0] = -1;
   fprintf(tfd, "%cmessage {REFERENCE FORMATTING FILES:}\n", '\\');
   fprintf(tfd, "%cdef%cTMACLIB{%s%c}\n", '\\', '\\', ptmac, dirsp2[0]);
   fprintf(tfd, "%cinput %s%cMacros.ttx\n", '\\', ptmac, dirsp2[0]);

   for (i = 1; i < argc; i++)
      if (argv[i][0] == optch2[0] && argv[i][1] == 'd') {
         if (argv[i][2])
         p = &argv[i][2];
      else { /* take next arg */
         i++;
         if (i == argc) {
            fprintf(stderr,"command line: illegal switch %s\n", argv[i-1]);
            clnup();
            exit(1);
            }
         p = argv[i];
         }
      strcpy(dirname,p);
      break;
      }

   for (i = 1; i < argc; i++)
      if (argv[i][0] == optch2[0]) {
         switch(argv[i][1]) {

            case 'd':  if (argv[i][2])
                          p = &argv[i][2];
                       else { /* take next arg */
                          i++;
                          if (i == argc) {
                             fprintf(stderr,
                               "command line: illegal switch %s\n", argv[i-1]);
                             clnup();
                             exit(1);
                             }
                          p = argv[i];
                          }
                       strcpy(dirname,p);
                       break;
  
            case 'n':  if (argv[i][2])
                          startflag = atoi(&argv[i][2]);
                       else { /* take next arg */
                          i++;
                          if (i == argc) {
                             fprintf(stderr,
                               "command line: illegal switch %s\n", argv[i-1]);
                             clnup();
                             exit(1);
                             }
                          startflag = atoi(argv[i]);
                          }
                       break;

            case 'p':  if (argv[i][2])
                          p = &argv[i][2];
                       else {  /* take next arg */
                          i++;
                          if (i == argc) {
                             fprintf(stderr,
                               "command line: illegal switch %s\n", argv[i-1]);
                             clnup();
                             exit(1);
                             }
                          p = argv[i];
                          }
                       strcpy(pfile, p);
                       personal = true;
                       for (p = fname, q = pfile; ; q++)
                          if (*q == ',' || *q == 0) {
                             *p = 0;
                             np = fopen( fname, "r");
                             if (np == NULL) {
                                fprintf(stderr, "Can't open index %s\n",fname);
                                clnup();
                                exit(1);
                                }
                             else {
                                tfgets(headerline,240,np);      
                                fclose(np);
                                chkindex(fname, headerline, true);
                                }
                             if (*q == 0)
                                break;
                             p = fname;
                             }
                          else *p++ = *q;
                       break;

            case 's':  style = false;    /*fall through*/
            case 'i':  if (argv[i][2])
                          p = &argv[i][2];
                       else { /* take next arg */
                          i++;
                          if (i == argc) {
                             fprintf(stderr,
                               "command line: illegal switch %s\n", argv[i-1]);
                             clnup();
                             exit(1);
                             }
                          p = argv[i];
                          }
                       incfile(p);
                       break;

            case 'x':  stndout = true;
                       break;

            case 'z':  silent = true;
                       break;

            case '|':  pipeflag = true;
                       stndout = true;
                       break;

            default:   fprintf(stderr, "command line: invalid switch %s\n",
                                  argv[i]);
                       clnup();
                       exit(1);
            }
         }
      else { /* file name */
         numfiles++;
         fileflag = true;
         if (style) {
            style = false;
            incfile( defstyle );
            }
         strcpy(bibfname, argv[i]);
         fd = fopen(argv[i], "r");
         if (fd == NULL) {
               strcat(bibfname,suffix);
            fd=fopen(bibfname, "r");
            if (fd == NULL) {
               fprintf(stderr, "can't open %s or %s\n", argv[i], bibfname);
               clnup();
               exit(1);
               }
            }
         if (stndout == false && silent == false) {
            fprintf(stderr, "Reading input document file %s and looking up references ...\n", bibfname);
            }
         rdtext(fd);
         fclose(fd);
      }
   if (style) incfile( defstyle );
   if (fileflag == false)          /* no input file on command line */
      if (pipeflag == true) {
         strcpy(bibfname, "<stdin>");
         rdtext(stdin);
         numfiles++;
         }
      else {
         fprintf(stderr, "Enter input file: ");
         scanf("%s", bibfnamet);
         numfiles++;
         strcpy(bibfname, bibfnamet);
         fd = fopen(bibfnamet, "r");
         if (fd == NULL) {
            strcat(bibfname, ".tex");
            fd=fopen(bibfname, "r");
            if (fd == NULL) {
               fprintf(stderr, "can't open %s or %s\n",
                    bibfnamet, bibfname);
               clnup();
               exit(1);
               }
            }
         if (stndout == false && silent == false) {
            fprintf(stderr, "Reading input document file %s and looking up references ...\n", bibfname);
            }
         rdtext(fd);
         fclose(fd);
         }
   return(numfiles);
}

/* chkindex - check if index is up to date */
   int chkindex(indx, header, pflag)
   char *indx;
   char header[240];
   int pflag;
{  char *c, *q, *p;
   char indx0[240], indx00[80], *indx1, indx11[240];
   char *rindex();
   struct stat buf;
   FILE *np;
   int i, j, qflag;
   time_t timefile, timeindx;

   stat(indx, &buf);
   timeindx = buf.st_mtime;
   strcpy (indx0,indx);
   c = rindex(indx0, dirsp2[0]);
   if (c == NULL)
      indx00[0] = '\0';
   else {
      *++c = '\0';
      strcpy (indx00, indx0);
      }
   strcpy (indx0,indx00);
   qflag = true;
   for (q = header ; *q;) {
      for (;*q == ' ';q++);
      if (*q == '\0') break;
      for (indx1 = q; *q && *q != ' '; q++);
      if (*q)
         *q = '\0';
      else
         qflag = false;
      if (*indx1 != dirsp2[0]) {
         strcat (indx0,indx1);
         strcpy (indx11,indx0);
         }
      np = fopen(indx11,"r");
      if (np == NULL) {
         fprintf(stderr,"Cannot locate reference file %s\n", indx11);
         clnup();
         exit(1);
         }
      strcpy (indx0, indx00);
      stat(indx11, &buf);
      timefile = buf.st_mtime;
      if (qflag == true)
         *q = ' ';
      if (timeindx < timefile) {
         fprintf(stderr,"Index %s not up to date\n", indx);
         if (pflag)
            fprintf(stderr,"Use tibdex (in appropriate directory) with reference file(s)%s\n",header);
         else
            fprintf(stderr,"   Use: tibdex%s\n",header);
         clnup();
         exit(1);
         }
      }
   }
 
/* outfname - make output file name */
   char *outfname(mp)
   char *mp;
{  char *q;

   for (q=mp; *q; q++);
   if (*--q == 'x')
      if (*--q == 'e')
         if (*--q == 't')
            if (*--q == '.')
                *q='\0';
    strcat (mp, "-t.tex");
    return (mp);
}

/* incfile - read in an included file  */
incfile(np)
   char *np;
{  char name[120];
   FILE *fd;
   char *p, line[LINELENGTH], dline[LINELENGTH], word[80], *tfgets();
   int  i, j, k, getwrd();
   int located, j1;
   char *ptmac, *getenv();

   ptmac = getenv("TMACLIB");
   if (ptmac == NULL)
      ptmac = TMACLIB;
   strcpy(bibfname, np);
   fd = fopen(np, "r");
   if (fd == NULL && *np != dirsp2[0]) {
      strcpy(name, np);
      strcat(name, ".tib");
      strcpy(bibfname, name);
      fd = fopen(name, "r");
      }
   if (fd == NULL && *np != dirsp2[0])
      if (dirname[0] != '\0') {
         strcpy(name, dirname);
         strcat(name, dirsp2);
         strcat(name, np);
         strcpy(bibfname, name);
         fd = fopen(name, "r");
         }
   if (fd == NULL && *np != dirsp2[0])
      if (dirname[0] != '\0') {
         strcpy(name, dirname);
         strcat(name, dirsp2);
         strcat(name, np);
         strcat(name, ".tib");
         strcpy(bibfname, name);
         fd = fopen(name, "r");
         }
   if (fd == NULL && *np != dirsp2[0]) {
      strcpy(name, ptmac);
      strcat(name, dirsp2);
      strcat(name, np);
      strcpy(bibfname, name);
      fd = fopen(name, "r");
      }
   if (fd == NULL && *np != dirsp2[0]) {
      strcpy(name, ptmac);
      strcat(name, dirsp2);
      strcat(name, np);
      strcat(name, ".tib");
      strcpy(bibfname, name);
      fd = fopen(name, "r");
      }
   if (fd == NULL) {
      bibwarning("%s: can't open\n", np);
      clnup();
      exit(1);
      }

   /* now go off and process file */
   fprintf(tfd, "%cmessage {(%s}\n", '\\', bibfname);
   biblineno = 1;
   while (tfgets(line, LINELENGTH, fd) != NULL) {
      biblineno++;
      switch(line[0]) {

         case '#': break;

         case 'A': for (p = &line[1]; *p && *p != ' '; p++) {
                      if (*p == 'A')
                         abbrev = true;
                      else if (*p == 'X')
                         capsmcap = true;
                      else if (*p == 'R') {
                         if (isdigit(*(p+1)))
                            numrev = atoi(p+1);
                         else
                            numrev = 1000;
                         }
                      }
                   break;

         case 'C': for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(citetemplate, p);
                   for (p=citetemplate; *p && *p != ' '; p++);
                   *p=0;
                   if (citetemplate[0] == '0') c0 = true;
                   break;

         case 'D': if ((k = getwrd(line, 1, word)) == 0)
                      error("word expected in definition");

/* this creates a linked list for use in sortdefs and expnd2
   but it is fortran style, not good c style -- fix later     */

                   j=jtop;
                   for (located = false; located == false; ) {
                      i=strcmp(word, words[jtop]);
                      if (i == 0) {
                         located = true;
                         break;
                      }
                      if (i > 0) {
                         wordtop++;
                         if (wordtop > MAXDEFS)
                            error("too many definitions:");
                         link[wordtop] = jtop;
                         jtop = wordtop;
                         j = jtop;
                         words[wordtop] = walloc(word);
                         located = true;
                         break;
                      }
                      while (link[j] >= 0) {
                         j1 = link[j];
                         i = strcmp(word, words[j1]);
                         if (i == 0) {
                            j = j1;
                            located = true;
                            break;
                         }
                         if (i > 0) {
                            wordtop++;
                            if (wordtop > MAXDEFS)
                               error("too many definitions:");
                            link[wordtop] = j1;
                            link[j] = wordtop;
                            j = wordtop;
                            words[wordtop] = walloc(word);
                            located = true;
                            break;
                         }
                         j = j1;
                      }
                      if (located == false) {
                         wordtop++;
                         if (wordtop > MAXDEFS)
                            error("too many definitions:");
                         link[wordtop] = -1;
                         link[j] = wordtop;
                         j = wordtop;
                         words[wordtop] = walloc(word);
                         located = true;
                         break;
                      }
                   }
                   for (p = &line[k]; *p == ' '; p++) ;
                   for (strcpy(dline, p); dline[strlen(dline)-1] == '\\'; ){
                       dline[strlen(dline)-1] = '\n';
                       if (tfgets(line, LINELENGTH, fd) == NULL) break;
                       strcat(dline, line);
                       }
                   defs[j] = walloc(dline);
                   break;

         case 'E': for (p = &line[1]; *p && *p != ' '; p++) {
                      if (*p == 'A')
                         edabbrev = true;
                      else if (*p == 'X')
                         edcapsmcap = true;
                      else if (*p == 'R') {
                         if (isdigit(*(p+1)))
                            ednumrev = atoi(p+1);
                         else
                            ednumrev = 1000;
                         }
                      }
                   break;

         case 'f': foot = true;
                   hyphen = false;
                   break;

         case 'F': specflag = true;
                   break;

         case 'H': hyphen = ordcite = true;
                   break;

         case 'I': for (p = &line[1]; *p == ' '; p++);
                   expnd1(p);
                   incfile(p);
                   break;

         case 'L': loccit = true;
                   break;

         case 'N': for (p = &line[1]; *p == ' '; p++);
                      if (*p == 'A')
                         abbrev = false;
                      else if (*p == 'F')
                         specflag = false;
                      else if (*p == 'H')
                         hyphen = false;
                      else if (*p == 'O')
                         ordcite = false;
                      else if (*p == 'R')
                         numrev = 0;
                      else if (*p == 'S')
                         sort = false;
                      else if (*p == 'X')
                         capsmcap = false;
                   break;
         case 'O': ordcite = true;
                   break;

         case 'P': onepage = true;
                   break;

         case 'S': sort = true;
                   for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(sortstr, p);
                   for (p=sortstr; *p && *p != ' '; p++);
                   *p=0;
                   break;

         case 'T': for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(trailstr, p);
                   for (p=trailstr; *p && *p != ' '; p++);
                   *p=0;
                   break;

         case 'U': underline = true;
                   break;

         default:  fprintf(tfd, "%s\n", line);
                   while (fgets(line, LINELENGTH, fd) != NULL)
                      fputs(line, tfd);
                   fprintf(tfd, "%cmessage {)}", '\\');
                   return;
         }

   }
   /* close up */
   fprintf(tfd, "%cmessage{)}", '\\');
   fclose(fd);
}

/* bibwarning - print out a warning message */
  bibwarning(msg, arg)
  char *msg, *arg;
{
  fprintf(stderr, "`%s', line %d: ", bibfname, biblineno);
  fprintf(stderr, msg, arg);
}

/* error - report unrecoverable error message */
  error(str)
  char str[];
{
  bibwarning("%s\n", str);
  clnup();
  exit(1);
}

#ifdef READWRITE
/*
** fixrfd( mode ) -- re-opens the rfd file to be read or write, 
**      depending on the mode.  Uses a static int to save the current mode
**      and avoid unnecessary re-openings.
*/
fixrfd( mode )
register int mode;
{
	static int cur_mode = WRITE;    /* rfd open for writing initially */

	if (mode != cur_mode)
	{
		rfd = freopen(reffile, ((mode == READ)? "r" : "a"), rfd);
		cur_mode = mode;
		if (rfd == NULL)
		      error("Hell!  Couldn't re-open reference file");
	}
}
#endif


/* tfgets - fgets which trims off newline */
   char *tfgets(line, n, ptr)
   char line[];
   int  n;
   FILE *ptr;
{  char *p;

   p = fgets(line, n, ptr);
   if (p == NULL)
      return(NULL);
   else
      for (p = line; *p; p++)
         if (*p == '\n')
            *p = 0;
   return(line);
}

/* getwrd - place next word from in[i] into out */
int getwrd(in, i, out)
   char in[], out[];
   int i;
{  int j;

   j = 0;
   while (in[i] == ' ' || in[i] == '\n' || in[i] == '\t')
      i++;
   if (in[i])
      while (in[i] && in[i] != ' ' && in[i] != '\t' && in[i] != '\n')
         out[j++] = in[i++];
   else
      i = 0;    /* signals end of in[i..]   */
   out[j] = 0;
   return (i);
}

/* walloc - allocate enough space for a word */
char *walloc(word)
   char *word;
{  char *i, *malloc();
   i = malloc(1 + strlen(word));
   if (i == NULL)
      error("out of storage");
   strcpy(i, word);
   return(i);
}

/* isword - see if character is legit word char */
int iswordc(c)
char c;
{
   if (isalnum(c) || c == '&' || c == '_')
      return(true);
   return(false);
}

/* expnd1 - expand reference, replacing defined words, scrunching spaces */
   expnd1(line)
   char *line;
{  char line2[REFSIZE], word[LINELENGTH], *p, *q, *w;
   int  replaced, i;

   replaced  = true;
   while (replaced) {
      replaced = false;
      p = line;
      q = line2;
      while (*p) {
         if (isalnum(*p)) {
            for (w = word; *p && iswordc(*p); )
               *w++ = *p++;
            *w = 0;
            for (i = 0; i <= wordtop; i++)
               if (strcmp(word, words[i]) == 0) {
                  strcpy(word, defs[i]);
                  replaced = true;
                  break;
                  }
            for (w = word; *w; )
               *q++ = *w++;
            }
         else
            *q++ = *p++;
         }
      *q = 0;
      p = line;
      q = line2;
      while (*q != 0) {
         if (*q != ' ')
            *p++ = *q++;
         else
            q++;
         }
      *p = 0;
      }
}

/* expnd2 - expand reference, replacing defined words */
   expnd2(line)
   char *line;
{  char line2[REFSIZE], word[LINELENGTH], *p, *q, *w;
   int  replaced, i;

   replaced  = true;
   while (replaced) {
      replaced = false;
      p = line;
      q = line2;
      while (*p) {
         if (*p != '|')
            *q++ = *p++;
         else {
            *p++;
            for (w = word; *p && *p != '|'; *w++ = *p++);
            *w = 0;
            if (locdef(word) == false) 
               fprintf(stderr,"word %s not defined in definition list\n",
                  word);
            else
               replaced = true;
            if (*p == '|')
               p++;
            for (w = word; *w; *q++ = *w++);
         }
      }
   *q = 0;
   p = line;
   q = line2;
   while (*p++ = *q++);
   }
}

/* locdef - locate a definition */
   int locdef(word)
   char *word;
{  long int lower, upper, mid, i;
   int strcmp();
   char *w, *w1;

   lower = 0;
   upper = wordtop;
   if (strcmp(word, words[defpos[lower]]) == 0) {
      for (w = word, w1 = defs[defpos[lower]]; *w1; *w++ = *w1++);
      *w = 0;
      return(true);
      }
   if (strcmp(word, words[defpos[upper]]) == 0) {
      for (w = word, w1 = defs[defpos[upper]]; *w1; *w++ = *w1++);
      *w = 0;
      return(true);
      }
   while (lower+1 < upper) {
      mid = (lower + upper)/2;
      i = strcmp(word, words[defpos[mid]]);
         if (i == 0) {
         for (w = word, w1 = defs[defpos[mid]]; *w1; *w++ = *w1++);
         *w = 0;
         return(true);
         }
      if (i > 0)
         lower = mid;
      else
         upper = mid;
      }
   return (false);
}    

/* rdref - read text for an already cited reference */
   rdref(i, ref)
   long int  i;
   char ref[REFSIZE];
{
   ref[0] = 0;
#ifdef READWRITE
   fixrfd( READ );                      /* fix access mode of rfd, if nec. */
#endif
   fseek(rfd, i, 0);
   fread(ref, 1, REFSIZE, rfd);
}

/* breakname - break a name into first and last name */
   breakname(line, first, last)
   char line[], first[], last[];
{  char *p, *q, *r, *t, *f;

   for (t = line; *t != '\n'; t++);
   for (t--; isspace(*t); t--);

   /* now strip off last name */
   for (q = t; isspace(*q) == 0 || ((*q == ' ') & (*(q-1) == '\\')); q--) {
      if (*q == '}') {
         while (*q != '{') {
            q--;
            if (q == line) {
               fprintf (stderr, "bad brackets in string %s in references\n",
                   line);
               clnup();
               exit(1);
            }
         }
      }
      if (q == line)
         break;
   }
   f = q;
   if (q != line) {
      q++;
      for (; isspace(*f); f--);
      f++;
      }

   /* first name is start to f, last name is q to t */

   for (r = first, p = line; p != f; )
      *r++ = *p++;
   *r = 0;
   for (r = last, p = q, t++; q != t; )
      *r++ = *q++;
   *r = 0;

}

/* match - see if string1 is a substring of string2 (case independent)*/
   int match(str1, str2)
   char str1[], str2[];
{  int  i, j;
   char a, b;

   for (i = 0; str2[i]; i++) {
      for (j = 0; str1[j]; j++) {
         if (isupper(a = str2[i+j]))
            a = (a - 'A') + 'a';
         if (isupper(b = str1[j]))
            b = (b - 'A') + 'a';
         if (a != b)
            break;
         }
      if (str1[j] == 0)
         return(true);
      }
   return(false);
}

/* scopy - append a copy of one string to another */
   char *scopy(p, q)
   char *p, *q;
{
   while (*p++ = *q++)
      ;
   return(--p);
}

/* makecites - make citation strings */
   makecites(citestr)
   char *citestr[];
{  char ref[REFSIZE], tempcite[100], *malloc();
   int  i;

   for (i = 0; i <= numrefs; i++) {
      rdref(refspos[i], ref);
      bldcite(tempcite, i, ref);
      citestr[i] = malloc(2 + strlen(tempcite)); /* leave room for disambig */
      if (citestr[i] == NULL)
         error("out of storage");
      strcpy(citestr[i], tempcite);
      }
}

/* sortdefs - make array of pointers to defined words */
   sortdefs()
{  long int i, j;

   i = wordtop;
   for (j = jtop; j >= 0; j = link[j]) {
      defpos[i] = j;
      i--;
   }
}

/* rcomp1 - revised reference comparison routine for qsort utility */
   int rcomp1(ap, bp)
   long int *ap, *bp;
{  char ref1[REFSIZE], ref2[REFSIZE], field1[MAXFIELD], field2[MAXFIELD];
   char alpha1[REFSIZE], alpha2[REFSIZE];
   char *p, *q, *getfield();
   int  neg, res;
   int n, m1, m2, num;
   int getname();
   char last1[MAXFIELD], last2[MAXFIELD], first1[MAXFIELD], first2[MAXFIELD];
   int alphabits();
   char *fstcap();

   rdref(*ap, ref1);
   rdref(*bp, ref2);
   for (p = sortstr, q=p; *p ; p=q) {
      if (*p == '-') {
         p++;
         neg = true;
         }
      else
         neg = false;
      res=0;
      if (*p == 'A') {
         p++;
         if (isdigit(*p))
             for (num=0; isdigit(*p); p++)
                num=10*num+(*p-'0');
         else
             num=100;
         q=p;
         for (n=1; n <= num; n++) {
             m1=getname (n, last1, first1, ref1);
             m2=getname (n, last2, first2, ref2);
             if (m1 == 0 && m2 == 0)
                break;
             alphabits(alpha1, last1);
             alphabits(alpha2, last2);
/*             res = strcmp (fstcap(alphabits(last1)), fstcap(alphabits(last2)));
             alpha11 = fstcap(alphastring1);
             alpha21 = fstcap(alphastring2);*/
             res = strcmp (fstcap(alpha1), fstcap(alpha2));
             if (res) break;
             alphabits(alpha1, first1);
             alphabits(alpha2, first2);
             res = strcmp (fstcap(alpha1), fstcap(alpha2));
             if (res != 0) break;
          }
      }
      else {
          q++;
          getfield(p, field1, ref1);
          getfield(p, field2, ref2);
          res = strcmp (field1, field2);
      }
      if (neg)
         res = - res;
      if (res != 0)
         break;
   }
   if (res == 0) {
      if (ap < bp)
         res = -1;
      else
         res = 1;
   }
   return(res);
}

/* alphabits - reduce string to alpha characters */
   alphabits(stringout, stringin)
   char *stringin, *stringout;
{  char *p, *q;

   q = stringout;
   p = stringin;
   while (*p) {
      if (isalpha(*p) || isspace(*p))
         *q++ = *p++;
      else if (*p == '{') {
         for (; *p && *p != '}'; p++);
         if (*p == 0) {
            fprintf(stderr, "bad brackets in string %s in references\n", 
               stringin);
            clnup();
            exit(1);
            }
         else {
            for (p--; isalpha(*p); p--);
            for (p++; isalpha(*p); *q++ = *p++);
            }
         }
       else p++;
       }
   *q = 0;
}

/* fstcap - find first capital letter in string */
   char *fstcap(string)
   char *string;
{  char *ptr;

   for(ptr=string; *ptr && (isupper(*ptr) == 0); ptr++);
   return (ptr);
}

/* capbrackets - find if string in brackets is capital, also capitalize it */
   capbrackets(stringout, stringin)
   char *stringout, *stringin;
{  char *p, *q;
   int cap;

   cap = false;
   q = stringout;
   p = stringin;
   for (; *p && *p != '}'; *q++ = *p++);
   if (*p == 0) {
      fprintf(stderr, "bad brackets in string %s in references\n", 
         stringin);
      clnup();
      exit(1);
      }
   *q++ = '}';
   *q = 0;
   q--;
   for (q--; isalpha(*q); q--) {
      if (isupper (*q))
         cap = true;
      else {
         cap = false;
         *q = (*q - 'a') + 'A';
         }
      }
   return (cap);
}

/* undrline - underline successive identical authors */
   undrline()
{  char line[REFSIZE], line1[REFSIZE], last[REFSIZE], first[REFSIZE];
   char ref[REFSIZE];
   int m, i;

   line1[0] = 0;
   for (m=numrefs; m >= 0; m--) {
      rdref(refspos[m], ref);
      line[0] = 0;
      for (i=1; getname(i, last, first, ref); i++) {
         strcat(line, last);
         strcat(line, first);
         }
      if (strcmp(line, line1) == 0)
         unlmark[m] = true;
      else
         unlmark[m] = false;
      strcpy(line1, line);
   }
}


/* bldcite - build a single citation string */
   bldcite(cp, i, ref)
   char *cp, ref[];
   int  i;
{  char *p, *q, c, *fp, field[REFSIZE], *getfield(), *aabet(), *nmdt();

   getfield("F", field, ref);
   if (field[0] != 0 && specflag)
      for (p = field; *p; p++)
         *cp++ = *p;
   else {
      p = citetemplate;
      field[0] = 0;
      while (c = *p++) {
         if (isalpha(c)) {                      /* field name   */
            q = getfield(p-1, field, ref);
            if (q != 0) {
               p = q;
               cp=scopy(cp, "\\Citebreak ");
               for (fp = field; *fp; )
                  *cp++ = *fp++;
               }
            }
         else if (c == '0') {                   /* empty citation */
            *cp = '\0';
	  }
         else if (c == '1') {                   /* numeric  order */
            sprintf(field, "%d", startflag + i);
            for (fp = field; *fp; )
               *cp++ = *fp++;
            }
         else if (c == '2')                     /* alternate alphabetic */
            cp = aabet(cp, ref);
         else if (c == '3')                     /* names */
            cp = nmdt(cp, ref);
/*       else if (c == '4')          here is how to add new styles */
/*       else if (c == '{') { 
            while (*p != '}')
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
            p++;
            }    */
         else if (c == '<') {
            while (*p != '>') {
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
               }
            p++;
            }
         else if (c != '@')
            *cp++ = c;
         }
      }
   *cp++ = 0;
}

/* alternate alphabetic citation style */
   char *aabet(cp, ref)
   char *cp, ref[];
{  char field[REFSIZE], temp[100], *np, *fp;
   char string11[80], string12[80], string13[80], string2[80], string3[80];
   char bracketstring[80];
   int getname(), i;

   if (getname(1, field, temp, ref)) {
      fp = field;
      for (np = string11; *fp; fp++) {
         if (isupper(*fp)) {
            *np++ = *fp;
            break;
            }
         else if (*fp == '{') {
            if (capbrackets(bracketstring, fp) == 0)
               for (; *fp != '}'; fp++);
            else {
               for (; *fp != '}'; *np++ = *fp++);
               *np++ = '}';
               break;
               }
            }
         }
      if (*fp) fp++;
      *np = 0;
      for (np = string12; *fp && isalpha(*fp) == 0 && *fp != '{'; 
         *np++ = *fp++);
      if (*fp != '{')
         *np++ = *fp;
      else {
         capbrackets(bracketstring, fp);
         for (; *fp != '}'; *np++ = *fp++);
         *np++ = '}';
         }
      if (*fp) fp++;
      *np = 0;
      for (np = string13; *fp && isalpha(*fp) == 0 && *fp != '{'; 
         *np++ = *fp++);
      if (*fp != '{')
         *np++ = *fp;
      else {
         capbrackets(bracketstring, fp);
         for (; *fp != '}'; *np++ = *fp++);
         *np++ = '}';
         }
      *np = 0;
      }
   if (getname(2, field, temp, ref) == 0) {
      strcpy (field, string11);
      strcat (field, string12);
      strcat (field, string13);
      }
   else {
      fp = field;
      for (np = string2; *fp; fp++) {
         if (isupper(*fp)) {
            *np++ = *fp;
            break;
            }
         if (*fp == '{') {
            if (capbrackets(bracketstring, fp) == 0)
               for (; *fp != '}'; fp++);
            else {
               for (; *fp != '}'; *np++ = *fp++);
               *np++ = '}';
               break;
               }
            }
         }
      *np = 0;
      if (getname(3, field, temp, ref) == 0) {
         strcpy (field, string11);
         strcat (field, string12);
         strcat (field, string2);
         }
      else {
         fp = field;
         for (np = string3; *fp; fp++) {
            if (isupper(*fp)) {
               *np++ = *fp;
               break;
               }
            if (*fp == '{') {
               if (capbrackets(bracketstring, fp) == 0)
                  for (; *fp != '}'; fp++);
               else {
                  for (; *fp != '}'; *np++ = *fp++);
                  *np++ = '}';
                  break;
                  }
               }
            }
         *np = 0;
         strcpy (field, string11);
         strcat (field, string2);
         strcat (field, string3);
         }
      }
   for (i=0; field[i]; i++)
      *cp++ = field[i];
   return (cp);
}

/* names style
        if 1 author - last name date
        if 2 authors - last name and last name date
        if 3 authors - last name, last name and last name date
        if 4 or more authors - last name et al. date */
   char *nmdt(cp, ref)
   char *cp, ref[];
{  char name1[100], name2[100], name3[100], temp[100], *fp;
   int getname();

   if (getname(1, name1, temp, ref)) {
      for (fp = name1; *fp; )
         *cp++ = *fp++;
      if (getname(4, name3, temp, ref)) {
         for (fp = " et al."; *fp; )
            *cp++ = *fp++;
         }
      else if (getname(2, name2, temp, ref)) {
         if (getname(3, name3, temp, ref)) {
            for (fp = "\\Namecomma "; *fp; )
               *cp++ = *fp++;
            for (fp = name2; *fp; )
               *cp++ = *fp++;
            for (fp = "\\Nameandd "; *fp; )
               *cp++ = *fp++;
            for (fp = name3; *fp; )
               *cp++ = *fp++;
            }
         else {
            for (fp = "\\Nameand "; *fp; )
               *cp++ = *fp++;
            for (fp = name2; *fp; )
               *cp++ = *fp++;
            }
         }
    }
return(cp);
}

/* getfield - get a single field from reference */
   char *getfield(ptr, field, ref)
   char *ptr, field[], ref[];
{  char *p, *q, temp[100];
   int  n, len, i, getname();

   field[0] = 0;
   if (*ptr == 'A')
      getname(1, field, temp, ref);
   else
      for (p = ref; *p; p++)
         if (*p == '%' && *(p+1) == *ptr) {
            for (p = p + 2; *p == ' '; p++)
               ;
            for (q = field; (*p != '\n') && (*p != '\0'); )
               *q++ = *p++;
            *q = 0;
            break;
            }
   n = 0;
   len = strlen(field);
   if (*++ptr == '-') {
      for (ptr++; isdigit(*ptr); ptr++)
         n = 10 * n + (*ptr - '0');
      if (n > len)
         n = 0;
      else
         n = len - n;
      for (i = 0; field[i] = field[i+n]; i++)
         ;
      }
   else if (isdigit(*ptr)) {
      for (; isdigit(*ptr); ptr++)
         n = 10 * n + (*ptr - '0');
      if (n > len)
         n = len;
      field[n] = 0;
      }

   if (*ptr == 'u') {
      ptr++;
      for (p = field; *p; p++)
         if (islower(*p))
            *p = (*p - 'a') + 'A';
      }
   else if (*ptr == 'l') {
      ptr++;
      for (p = field; *p; p++)
         if (isupper(*p))
            *p = (*p - 'A') + 'a';
      }
   return(ptr);
}

/* getname - get the nth name field from reference, breaking into
             first and last names */
   int getname(n, last, first, ref)
   int  n;
   char last[], first[], ref[];
{  char *p;
   int  m;

   last[0]='\0';
   first[0]='\0';
   m = n;
   for (p = ref; *p; p++)
      if (*p == '%' & *(p+1) == 'A') {
         n--;
         if (n == 0) {
            for (p = p + 2; *p == ' '; p++) ;
            breakname(p, first, last) ;
            return(true);
            }
         }

   if (n == m)          /* no authors, try editors */
      for (p = ref; *p; p++)
         if (*p == '%' & *(p+1) == 'E') {
            n--;
            if (n == 0) {
               for (p = p + 2; *p == ' '; p++) ;
               breakname(p, first, last) ;
               return(true);
               }
            }

   if (n == m && n == 1) {        /* no editors, either, try institution */
      first[0] = last[0] = '\0';
      getfield("I", last, ref);
      if (last[0] != '\0')
         return(true);
      }

   return(false);
}

/* disambiguate - compare adjacent citation strings, and if equal, add
                  single character disambiguators */
   disambiguate()
{  int i, j;
   char adstr[2];

   for (i = 0; i < numrefs; i = j) {
      j = i + 1;
      if (strcmp(citestr[i], citestr[j])==0) {
         adstr[0] = 'a'; adstr[1] = 0;
         for (j = i+1; strcmp(citestr[i], citestr[j]) == 0; j++) {
            adstr[0] = 'a' + (j-i);
            strcat(citestr[j], adstr);
            if (j == numrefs)
               break;
            }
         adstr[0] = 'a';
         strcat(citestr[i], adstr);
         }
     }
}


/* bldname - build a name field
             doing abbreviations, reversals, and caps/small caps
*/
   bldnm1(first, last, name, reverse)
   char *first, *last, name[];
   int reverse;
{
   char newfirst[120], newlast[120], *p, *q, *f, *l, *scopy();
   int  flag;

   if (abbrev) {
      p = first;
      q = newfirst;
      flag = false;
      while (*p) {
         while (*p == ' ')
            p++;
         if (*p == 0)
            break;
         if (isupper(*p)) {
            if (flag)           /* between initial gap */
               q = scopy(q, "\\Initgap ");
            flag = true;
            *q++ = *p;
            q = scopy(q, "\\Initper ");
            }
         if (*++p == '.')
            p++;
         else while (*p != 0 && ! isspace(*p))
            p++;
         }
      *q = 0;
      f = newfirst;
      }
   else
      f = first;

   if (capsmcap) {
      p = last;
      q = newlast;
      flag = 0;  /* 1 - printing cap, 2 - printing small */
      while (*p)
         if (islower(*p)) {
            if (flag != 2)
               q = scopy(q, "\\bgroup\\Smallcapsfont ");
            flag = 2;
            *q++ = (*p++ - 'a') + 'A';
            }
         else {
            if (flag == 2)
               q = scopy(q, "\\egroup ");
            flag = 1;
            *q++ = *p++;
            }
      if (flag == 2)
         q = scopy(q, "\\egroup ");
      *q = 0;
      l = newlast;
      }
   else
      l = last;

   if (f[0] == 0)
      sprintf(name, "%s\n", l);
   else if (reverse)
      sprintf(name, "%s%cRevcomma %s\n", l, '\\', f);
   else
      sprintf(name, "%s %s\n", f, l);
}

/* capssmallcaps - put field in capssmallcaps */
   capssmallcaps(stringout, stringin)
   char *stringout, *stringin;
{  char *p, *q, *scopy();
   int flag, bflag;
   char bracketfield[REFSIZE];

   p = stringin;
   q = stringout;
   *q = 0;
   flag = 0;  /* 1 - printing cap, 2 - printing small */
   while (*p) {
      if (*p == '\\')
         *p++;
      if (*p == ' ') {
         if (flag == 2)
            q = scopy(q, "\\egroup{}");
         flag = 1;
/*         *q++ = '\\';*/
         *q++ = ' ';
         p++;
         }
      if (*p == '~') {
         if (flag == 2)
            q = scopy(q, "\\egroup{}");
         flag = 1;
         *q++ = *p++;
         }
      if (*p == '{')
         bflag = capbrackets(bracketfield, p);
      else
         bflag = 1;
      if (islower(*p) || bflag == 0) {
         if (flag != 2)
            q = scopy(q, "\\bgroup\\Smallcapsfont ");
         flag = 2;
         if (*p == '{') {
            q = scopy(q, bracketfield);
            for (; *p != '}'; p++);
            p++;
            }
         else
            *q++ = (*p++ - 'a') + 'A';
         }
      else {
         if (flag == 2)
            q = scopy(q, "\\egroup{}");
         flag = 1;
         if (*p == '{') {
            q = scopy(q, bracketfield);
            for (; *p != '}'; p++);
            p++;
            }
         else
            *q++ = *p++;
         }
      }
   if (flag == 2)
      q = scopy(q, "\\egroup{}");
   *q = 0;
}

   bldname(first, last, name, reverse, capsmcap)
   char *first, *last, name[];
   int reverse, capsmcap;
{
   char newfirst[120], newlast[120], *p, *q, *f, *f1, *l, *scopy();
   char newnewfirst[120];
   char bracketfield[20];
   int  flag;
   int hflag, bflag;

   if (abbrev) {
      p = first;
      q = newfirst;
      flag = false;
      hflag = false;
      bflag = false;
      while (*p) {
         while (*p == ' ')
            p++;
         if (*p == '{')
            bflag = capbrackets(bracketfield, p);
         if (isupper(*p) || bflag) {
            if (flag && hflag)           /* between initial gap */
               q = scopy(q, "\\Initgap ");
            flag = true;
            if (bflag) {
               for (; *p != '}'; *q++ = *p++);
               bflag = false;
               }
            *q++ = *p;
            for (p++;; p++) {
               if (isupper(*p)) {
                  hflag = true;
                  break;
                  }
               else if (*p == '-' || *p == '\'') {
                  hflag = false;
                  *q++ = *p++;
                  break;
                  }
               else if (*p == '.' ) {
                  hflag = true;
                  q = scopy(q, "\\Initper ");
                  p++;
                  break;
                  }
               else if (*p == ' ' ) {
                  hflag = true;
                  q = scopy(q, "\\Initper ");
                  p++;
                  break;
                  }
               else if (*p == '~') {
                  hflag = true;
                  q = scopy(q, "\\Initper ");
                  p++;
                  break;
                  }
               else if (*p == 0) {
                  q = scopy(q, "\\Initper ");
                  break;
                  }
               else if (*p == '{') {
                  bflag = capbrackets(bracketfield, p);
                     if (bflag) break;
                     else
                        for (; *p != '}'; p++);
                  }
               }
            }
         else
            *q++ = *p++;
         }
      *q = 0;
      f1 = newfirst;
      }
   else
      f1 = first;

   if (capsmcap) {
      capssmallcaps(newlast,last);
      l = newlast;
      if (abbrev == 0) {
         capssmallcaps(newnewfirst,f1);
         f = newnewfirst;
         }
      else
         f = f1;
      }
   else {
      f = f1;
      l = last;
      }
   if (f[0] == 0)
      sprintf(name, "%s\n", l);
   else if (reverse)
      sprintf(name, "%s%cRevcomma %s\n", l, '\\', f);
   else
      sprintf(name, "%s %s\n", f, l);
}

/* prtauth - print author or editor field */
   prtauth(c, line, num, max, ofd, abbrev, capsmcap, numrev)
   char c, *line;
   int  num, max, abbrev, capsmcap, numrev;
   FILE *ofd;
{  char first[LINELENGTH], last[LINELENGTH];
   char *s1;
  
   if (num <= numrev || abbrev || capsmcap) {
      breakname(line, first, last);
      bldname(first, last, line, num <= numrev, capsmcap);
      }
   for (s1=line;*s1!='\0';s1++) if (*s1=='\n') *s1='\0';
   if (num == 1) {
      fprintf(ofd, "}%c\n%cdef%c%ctest{ }", '\%', '\\', '\\', c);
      if (index(trailstr, c))
         fprintf(ofd, "%cdef%c%ctrail{%c}",
            '\\', '\\', c, line[strlen(line)-2]);
      fprintf(ofd, "%cdef%c%cstr{%s", 
         '\\', '\\', c, line);
      }
   else if (num < max)
      fprintf(ofd, "%c\n  %c%ccomma %s", '\%', '\\', c, line);
   else if (max == 2)
      fprintf(ofd, "%c\n  %c%cand %s", '\%', '\\', c, line);
   else
      fprintf(ofd, "%c\n  %c%candd %s", '\%', '\\', c, line);
}

/* doline - actually print out a line of reference information */
   doline(c, line, numauths, maxauths, numeds, maxeds, 
               numrevs, maxrevs, numtrans, maxtrans, ofd)
   char c, *line;
   int numauths, maxauths, numeds, maxeds;
   int numrevs, maxrevs, numtrans, maxtrans;
   FILE *ofd;
{
   char *s1;
   char *dbldash();
   char line1[REFSIZE];

   switch(c) {
      case '\\':
      case '\%':
          for (s1=line;*s1!='\0';s1++)
              if (*s1=='\n') *s1='\0';
          fprintf(ofd, "}%c\n%c%s{", '\%', c , line);
          break;

      case 'A':
          prtauth(c, line, numauths, maxauths, ofd, abbrev, capsmcap, numrev);
          break;

       case 'E':
          if (maxauths)
             prtauth(c, line, numeds, maxeds, ofd, edabbrev, edcapsmcap, 
                 ednumrev);
          else
             prtauth(c, line, numeds, maxeds, ofd, abbrev, capsmcap, numrev);
          break;

       case 'a':
          prtauth(c, line, numtrans, maxtrans, ofd, edabbrev, edcapsmcap,
             ednumrev);
         break;

       case 'e':
          prtauth(c, line, numrevs, maxrevs, ofd, edabbrev, edcapsmcap, 
             ednumrev);
          break;

       case 'P':
       case 'p':
          fprintf(ofd, "}%c\n%cdef%c%ctest{ }", '\%', '\\', '\\', c);
          if (onepage) {
             opage(line);
             fprintf(ofd, "%cdef%c%ccnt{}", '\\', '\\', c);
             }
          else {
             if (index (line, '-')) {
                line = dbldash(line);
                fprintf(ofd, "%cdef%c%ccnt{ }", '\\', '\\', c);
             }
             else
                fprintf(ofd, "%cdef%c%ccnt{}", '\\', '\\', c);
          }
          if (index(trailstr, c))
             fprintf(ofd, "%cdef%c%ctrail{%c}%c\n", '\\', '\\', c, 
                     line[strlen(line)-2], '\%');
          for (s1 = line; *s1 != '\0'; s1++)
              if (*s1=='\n') *s1='\0';
          fprintf(ofd, "%cdef%c%cstr{%s", '\\', '\\', c, 
              line);
          break;

       case 'F':
       case 'K': break;

       case 'I':
          if (numauths == 0 && numeds == 0 && capsmcap) {
             capssmallcaps(line1, line);
             strcpy(line, line1);
             }

       default:
          fprintf(ofd, "}%c\n%cdef%c%ctest{ }", '\%', '\\', '\\', c);
          if (index(trailstr, c))
             fprintf(ofd, "%cdef%c%ctrail{%c}", '\\', '\\', c, 
             line[strlen(line)-2]);
          for (s1=line;*s1!='\0';s1++)
              if (*s1=='\n') *s1='\0';
          fprintf(ofd, "%cdef%c%cstr{%s", 
              '\\', '\\', c, line);
          }
}

/* dbldash - double the hyphen in the P field (see below)*/
   char *dbldash(line)
   char *line;
{  char *p;
   char newline[120];

   for (p = newline; *line && *line != '-'; *p++ = *line++);
   if (*line == 0) return (line);
   if (*++line != '-')
      *p++ = '-';
   *p++ = '-';
   for (*p = *line; *line; *p++ = *line++);
   *p='\0';
   return (newline);
}

/* following works on some machines where above fails
char *dbldash(line)
char *line;
{  char *p;
   static char newline[120];

   for (p = newline; *p = *line; ++p, ++line)
      if (line[0] == '-' && line[1] != '-')
          *++p = '-';
   return(newline);
} */

/* opage - print only first page number */
   int opage(line)
   char *line;
{  char *p;

   for (p = line; *p && *p != '-'; *p++);
   *p=0;
}

/* dumpref - dump reference number i */
   dumpref(i, ofd)
   int i;
   FILE *ofd;
{  char ref[REFSIZE], *p, line[REFSIZE];
   int numauths, maxauths, numeds, maxeds;
   int numrevs, maxrevs, numtrans, maxtrans;

   fprintf(ofd, "%cbgroup%cResetstrings%c\n", '\\', '\\' , '\%');
   rdref(refspos[i], ref);
   maxauths = maxeds = maxrevs = maxtrans = 0;
   numauths = numeds = numrevs = numtrans = 0;
   for (p = ref; *p; p++)
      if (*p == '%')
         if (*(p+1) == 'A') maxauths++;
         else if (*(p+1) == 'E') maxeds++;
         else if (*(p+1) == 'a') maxtrans++;
         else if (*(p+1) == 'e') maxrevs++;
   if (loccit) {
      if (locflag[i])
         fprintf(ofd, "%cdef%cLoccittest{ }", '\\', '\\');
      else
         fprintf(ofd, "%cdef%cLoccittest{}", '\\', '\\');
      if (ibidflag[i])
         fprintf(ofd, "%cdef%cIbidtest{ }", '\\', '\\');
      else
         fprintf(ofd, "%cdef%cIbidtest{}", '\\', '\\');
   }
   else
      fprintf(ofd, "%cdef%cLoccittest{}", '\\', '\\');
   if (abbrev) 
      fprintf(ofd, "%cdef%cAbbtest{ }", '\\', '\\');
   else
      fprintf(ofd, "%cdef%cAbbtest{}", '\\', '\\');
   if (capsmcap) 
      fprintf(ofd, "%cdef%cCapssmallcapstest{ }", '\\', '\\');
   else
      fprintf(ofd, "%cdef%cCapssmallcapstest{}", '\\', '\\');
   if (edabbrev) 
      fprintf(ofd, "%cdef%cEdabbtest{ }", '\\', '\\');
   else
      fprintf(ofd, "%cdef%cEdabbtest{}", '\\', '\\');
   if (edcapsmcap) 
      fprintf(ofd, "%cdef%cEdcapsmallcapstest{ }", '\\', '\\');
   else
      fprintf(ofd, "%cdef%cEdcapsmallcapstest{}", '\\', '\\');
   if (underline) 
      fprintf(ofd, "%cdef%cUnderlinetest{ }%c\n", '\\', '\\', '\%');
   else
      fprintf(ofd, "%cdef%cUnderlinetest{}%c\n", '\\', '\\', '\%');
   fprintf(ofd, "%cdef%cNoArev{%d}%cdef%cNoErev{%d}%cdef%cAcnt{%d}", 
      '\\', '\\', numrev, '\\', '\\', ednumrev, '\\', '\\', maxauths);
   fprintf(ofd, "%cdef%cEcnt{%d}%cdef%cacnt{%d}%cdef%cecnt{%d}%c\n", 
      '\\', '\\', maxeds, '\\', '\\', 
      maxtrans, '\\', '\\', maxrevs, '\%');
   fprintf(ofd, "%cdef%cFtest{ }", '\\', '\\');
          if (index(trailstr, 'F'))
             fprintf(ofd, "%cdef%c%ctrail{%c}", '\\', '\\', 'F', 
             citestr[i][strlen(citestr[i])-1]);
   fprintf(ofd, "%cdef%cFstr{%s", '\\', '\\', 
      citestr[i]);
   fseek(rfd, (long) refspos[i], 0);
   while (fgets(line, REFSIZE, rfd) != NULL) {
      if (line[0] == 0)        break;
      else if (line[0] == '%') {
            for (p = &line[2]; *p == ' '; p++);
            if (line[1] == 'A')       numauths++;
            else if (line[1] == 'E')  numeds++;
            else if (line[1] == 'a')  numtrans++;
            else if (line[1] == 'e')  numrevs++;

            doline(line[1], p, numauths, maxauths, numeds, maxeds, 
               numrevs, maxrevs, numtrans, maxtrans, ofd);
            }
      else if (line[0] == '\\')
         fprintf(ofd, "}%c\n%s{", '\%', line);
      else fprintf(ofd, "%s", line);
      }
   if (underline && unlmark[i-1]) {
      if (maxauths) fprintf (ofd, "}%c\n%cdef%cAstr{%cUnderlinemark", '\%', 
         '\\', '\\', '\\');
      else if (maxeds) fprintf (ofd, "}%c\n%cdef%cEstr{%cUnderlinemark", '\%', 
         '\\', '\\', '\\');
      else fprintf (ofd, "}%c\n%cdef%cIstr{%cUnderlinemark", '\%', '\\', 
         '\\', '\\');
      }
   fprintf(ofd, "}%c\n%cRefformat%cegroup%c\n", '\%', '\\', '\\', '\%');
   if ((!foot) && (!c0)) fprintf(ofd, "\n");
}
