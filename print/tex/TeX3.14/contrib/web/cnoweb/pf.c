/*
      pf.c in c-no-web format.  (8/10/89)
      by Jim Fox, University of Washington
 
 \input cnoweb   % load the c-no-web system
%
% \nweb ... \bewn = \cnoweb commentary
  \def\nweb{\leavevmode\begingroup\codetype$\lbrack\!\lbrack\;$}
  \def\bewn{$\;\rbrack\!\rbrack$\ \endgroup}
%
 \title{Printing factorials: a demonstration of \cnoweb}
 \synopsis{You are reading a program listing that was formatted
  with \par\item{}{\tt \% tex pf.c}\par
  The same program is compiled
  with \par\item{}{\tt \% cc pf.c}\par
  The key to this dual function source file is a \TeX\ macro
  package called \cnoweb\ that treats all comments as `\TeX-text'
  and all else as `verbatim-text.' The C compiler naturally
  does the opposite and interprets only the text outside the comments.

  \nweb In this program comments about 
     \cnoweb\ itself look like this.\bewn}
 \section{Introduction}
 
  This program prints factorials of positive integers.
  It uses this definition of $n!$
  $$ n! = \prod_{i=1}^n i$$
 
  {\narrower
  \nweb \"pf.c" begins with a comment which
  is treated as \TeX-text by \cnoweb.
 
  The first comment contains the
   string *<\input cnoweb>*.  This begins the \cnoweb\ listing.
   Included also in this leading comment section is
   *<\title{Printing ...}>*,
   *<\synopsis{You are reading ...}>*,
   and
   *<\section{Introduction}>*.\bewn\par}

  The program is invoked with the command
  \item{}\"pf $[$-l$]$ $[$-e$]$ $n$"

  where
   \item{\bf -l} prints the factorials of all numbers
     up to and including the limit $n$.
   \item{\bf -e} includes an exponential notation approximation.
   \item{$n$} is the number whose factorial will be printed.
  */

#define GOODEXIT 0          /* exit with this if factorial OK */
#define BADEXIT  1          /* exit with this if error */

#include <stdio.h>          /* standard io stuff */
#include <math.h>           /* math stuff */
char     *malloc();         /* memory allocator */

/* \section{Big-integer routines}
 
  \"pf" would be trivial were it not for the rapid
  growth of $n!$, which is much greater than exponential
  and quickly surpasses even the real number capacity of most machines.
  We therefore define a new data type (\"Int") to hold big integers.
  */

  typedef struct Int_ {
     int radix;                /* $2^1 \le \hbox{radix} \le 2^8$ */
     char *digits;             /* the \"Int"'s digits */
     int length;               /* number of digits available */
     int limit;                /* number of digits in use */
  } Int;
  
/* We do not need many operations on \"Int"s to compute factorials:
   only `set' and `multiply'.
  */
 
/* \subsection{\"Set(I,\ i)"}   
  Return \"I" initialized to \"i". */    
    
Set(I,i) 
Int *I; 
int i;  
{   
   int d;   
   I->digits[0] = (i?1:0);
   for (d=1; d<I->length; I->digits[d++]=0); 
   I->limit = 0;   
   if ((i!=0)&&(i!=1)) Product(I,i);
   return (0);
}   
 
/* \subsection{\"Product(I,\ i)"}   
  Return \"I" multiplied by \"i".
  Returns true if the product is OK. */    
    
Product(I,i) 
Int *I; 
int i;  
{   
   int d,n;   
   int l = I->limit+1;
   int r = I->radix;
   int carry = 0;
 
   for (d=0; ((d<l)||(carry!=0)); d++) { 
     if (d>=I->length) return (0);       /* not enough digits */
     n = I->digits[d] * i + carry;
     if ((I->digits[d] = n % r)>0) I->limit = d;
     carry = n / r;
   }
   return (1);
}   

 
 
/* \section{\"main()"} Print the factorial of the integer
   found on the command line.

   \item{1.} Parse command line arguments.
   \item{2.} Compute number of digits needed and allocate space.
   \item{3.} Calculate the factorial.
   \item{4.} Print the factorial.

     \nweb \cnoweb\ breaks pages only before comments.\hfill\break
     You can control pagination with this knowledge.\bewn

     The higher the radix the fewer digits needed, but
     we use an internal radix of 10
     to save a lot of division when printing the number. 
     Also, we already have a log$_{10}$ function.

   */

#define RADIX 10
 
Int nfactorial;      /* place for the factorial */
Int *nf = &nfactorial;
 
/* command line parameters */

 int limit_mode = 0;        /* true if printing all factorials */
 int expon_mode = 0;        /* true if printing exponential notation */
 
/* start of the program */

main(argc,argv)
int  argc;
char *argv[];
{
 int n;
  int i;
 
  /* 1. Parse the command line */

  n = parse_args(argc,argv);  /* we will print $n!$ */
 
  /* 2. Compute digits needed and allocate space */

  nf->radix = RADIX;
  nf->length = digits(n,RADIX);

  if ((nf->digits = malloc(nf->length))==NULL) {
     printf("Sorry, cannot allocate space for %d digits\n",
         nf->length);
     exit (BADEXIT);
  }
  
  /* 3. Calculate the factorial.  */

  Set(nf,1);         /* initialize \"nf" */
 
  for (i=1; i<=n; i++) {
     if (Product(nf,i)==0) {
        printf("Sorry, miscalculated the number of digits.\n");
        exit (BADEXIT);
     }
     if (limit_mode) print_Int(nf,i);
  }
 
  /* 4. Print the result, if it hasn't been printed yet. */
 
  if (!limit_mode) print_Int(nf,n);
  exit (GOODEXIT);
}
 
/* \subsection{\"parse_args(argc,argv)"}

     This is a standard UNIX idiom.
     See Kernighan \& Ritchie.

    \nweb \cnoweb\ automatically indents after 
      {\tt\char`\{} and {\tt\char`\(}.\bewn */

parse_args(argc,argv)
int argc;
char *argv[];
{
  int n = (-1);

  while (--argc > 0) {
    argv++;
    if (argv[0][0]=='-') {   /* is an option  flag */
       switch (argv[0][1]) {
          case 'l': {
            limit_mode = 1;
            break;
          }
          case 'e': {
            expon_mode = 1;
            break;
          }
          default: show_usage();
       }
    } else {                 /* is the number */
       sscanf(argv[0],"%d",&n);
    }
  }
 
  if (n<=0) show_usage();
  return (n);
}

/* \subsection{\"digits(n,r)"} Returns number of digits in \"n"
   using a radix of \"r".

     A factorial can be approximated by the Sterling formula
     $$ n! \approx e^{-n} n^{n} \sqrt{2\pi n}$$
     What we want is the number of digits, which is
     $$ \hbox{\# digits} \approx \log_r(n!)=\log(n!)/\log(r) \approx
        (-n \log e + n \log n + {1\over2}\log (2\pi n))/\log(r)$$

     We will add a couple of digits to this value
     to allow for the approximations and assure that we
     have enough.
  */

#define PI    3.141592653589793238462643   /* $\pi$ */
#define E     2.718281828459045235360287   /* $e$ */

digits(n,r)
int n;
int r;
{
  double dn = n;
  int nd;

  nd = (int)((-dn * log10(E)) + 
     (dn * log10(dn)) + (.5 * log10(2*PI*dn)))/log10((double)r) + 2;
  /* *<printf("requires %d digits\n",nd);>* */

  /* \nweb The
  `Commented out' code above was typed: 
  {\tt/{}* *{}<printf...;>{}* *{}/}.\bewn */

  return (nd);
}

/* \subsection{\"print_Int(I,\ i)"}   
  Print {\tt{\it value(}i{\it)} = {\it value(}I{\it)}}.
  The internal radix that matches the display radix
  clearly makes this procedure easier.

  This display uses the convention that separates each
   three-digit set with commas.
*/    
    
#define MAX_WIDTH 80            /* maximum width of the output */

print_Int(I,i) 
Int *I; 
int i;  
{   
   int d;   
   int lp,bp;
   char line[MAX_WIDTH];
 
   sprintf(line,"%d! = ",i);
   bp = lp = strlen(line);
   for (d=I->limit; d>=0; d--) {
      line[lp++] = I->digits[d] + '0';  /* assumes radix $\le 10$ */
      if ((d%3)==0) {
         line[lp++] = (d?',':'.');
         if ((d==0) || (lp>MAX_WIDTH-5)) {
           line[lp] = '\0';
           printf("%s\n",line);
           for (lp=0; lp<bp; line[lp++]=' ');
         }
      }  
   }

   /* If the exponential notation was desired, print it now.
      */

   if (expon_mode) {
      int d = I->limit;       /* the most significant digit */
      sprintf(line+bp,"approximately = %d.%d x 10e%d",
        I->digits[d],d?I->digits[d-1]:0,d);
      printf("%s\n",line);
   }
   return (0);
}
 
/* \section{\"show_usage()"} invocation syntax error---show 
   correct usage */ 
    
int show_usage()
{   
   printf("usage: pf [-l] [-e] positive_integer\n"); 
   exit (BADEXIT);
}   

/* \section{Summary of \cnoweb\ commands}

   These control sequences are defined in the \cnoweb\ macro package.

   \begingroup
     \def\{{{\tt\char`\{}}
     \def\}{{\tt\char`\}}}
     \def\cs#1{\smallskip\noindent\hangindent7\blockindent\hangafter1
        \hbox to7\blockindent{\bf#1\qquad\hss}\ignorespaces}

   \cs{\\title\{ ... \}} Titles the program.
   \cs{\\job\{ ... \}} Another title area. Defaults to input filename.
   \cs{\\section\{ ... \}} Begins a section.  The section title 
    is also included in the table of contents and 
    in the page header.
   \cs{\\subsection\{ ... \}} Begins a subsection.  The subsection title 
    is also included in the table of contents.
   \cs{\\subsubsection\{ ... \}} Begins a subsubsection.  
    The subsubsection title 
    is also included in the table of contents.
   \cs{\\newpage} Causes a page eject after the current line.  This 
    is usually used in a comment by itself, e.g.,
    \hbox{\tt /{}* \\newpage *{}/}.
   \cs{\\endc} Ends the \cnoweb\ listing.  This
    is usually the last line in the file, e.g.,
    \hbox{\tt /{}* \\endc *{}/}.
   \cs{\\{\tt"} ... {\tt"}} Prints {\bf bold} text.
   \cs{\\{\tt'} ... {\tt'}} Prints {\it italic} text.
   \cs{\\{\tt|} ... {\tt|}} Prints {\tt typewriter} text.
   \cs{*{\tt<} ... {\tt>}*} Allows C code to be included in comments.
      You can nest comments within the `commented out' C code, e.g.,
      \hfill\break 
      {\tt/{}* comment out this section *{}<} \hfill\break
      \hglue\blockindent{\tt i = 0; /{}* initialize \"i" *{}/} \hfill\break
      \hglue\blockindent{\tt ...}  \hfill\break
      \hglue\blockindent{\tt >{}* end of the commented out section *{}/}
      
   \cs{\\item, \\hang, {\rm etc.}} Work as you hope they would.
  \endgroup

  */
    
/* \section{How to obtain \cnoweb}

  You may obtain \cnoweb\ by anonymous ftp to
  {\tt u.washington.edu}.

  It is in the directory:  {\tt pub/tex/cnoweb}

  \bigskip
    \rightline{--- Jim Fox,
     University of Washington}
    \rightline{\strut \tt fox@cac.washington.edu}

   \medskip

   The file ends with the comment
    `{\tt /{}* \\endc *{}/}' */
   
/* \endc */ 
