/********************************************************************
 * lindner
 * 3.5
 * 1993/07/29 17:21:27
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/html2.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: html2.c
 * More lame code for html
 *********************************************************************
 * Revision History:
 * html2.c,v
 * Revision 3.5  1993/07/29  17:21:27  lindner
 * eliminate non-used variables
 *
 * Revision 3.4  1993/07/27  05:28:53  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.3  1993/07/27  00:32:46  lindner
 * HTML patch from Mitra
 *
 * Revision 3.2  1993/04/15  21:19:43  lindner
 * Debug line added (Mitra)
 *
 * Revision 3.1.1.1  1993/02/11  18:02:58  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.2  1992/12/31  03:59:42  lindner
 * Renamed to html2.c for case insensitive VMS. Fixed State: line.
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/


#include "gopher.h"
#include "Malloc.h"
#include "HTML.h"
#include "Debug.h"

Ourpager(){ ;}

char *process_tag();
static char *parastorage = NULL;
static int  parasize=0;
static int  paraptr=0;

static int  HeadingLevel=1;
static int  Linknum =0;

/*** State information for the HTML parser, ick  ***/

boolean     inANCHOR  = FALSE;
boolean     inXMP     = FALSE;
boolean     inLISTING = FALSE;
boolean     inADDRESS = FALSE;
boolean     inDL      = FALSE;
boolean     inDT      = FALSE;
boolean     inMENU    = FALSE;
boolean     inPLAINTXT= FALSE;
boolean     inLIST    = FALSE;
boolean     inPAREN   = FALSE;
boolean     inH1      = FALSE;
boolean     inHeading = FALSE;
boolean     isIndex   = FALSE;

/*** Different Types of justification ***/
#define JUSTIFY_LEFT 1
#define JUSTIFY_CENTER 2
#define JUSTIFY_RIGHT 3

/*** Different types of text styles ***/
#define STYLE_NL 1 /** Normal **/
#define STYLE_UL 2 /** Underline **/

/** Link delimiters **/
#define START_LINK '\177'
#define END_LINK   '\013'

int CurrentLine = 0;
int CurrentLinepos = 0;
extern int COLS;

HTMLObj *CurrentHTML;
int anchoroffset = -1;

int HTMLfilenum =0;

/*
 * Add a character to the paragraph buffer
 */

static buffer_ch(ch)
  char ch;
{
     /** check for overflow **/
     if (inANCHOR==FALSE)
	  anchoroffset = -1;

     if ((inANCHOR == TRUE) && (anchoroffset == -1))
	  anchoroffset = paraptr;

     if (paraptr == parasize-1) {
	  char *temp;

	  parasize *=2;
	  temp = (char*) realloc(parastorage, parasize);

	  if (temp != parastorage) {
	       free(parastorage);
	       parastorage = temp;
	  }
     }

     *(parastorage + paraptr++) = ch;
     *(parastorage + paraptr) = '\0';
}


/*
 * addch outputs a character to the file
 * It also notes the location of links in the file
 */

static add_ch(ch, zefile)
  char ch;
  FILE *zefile;
{
     if (ch == '\t') {
	  fprintf(zefile, "        ");
	  CurrentLinepos += 8;
     }
     else if (ch == '\n') {
	  CurrentLine++;
	  CurrentLinepos = 0;
	  fputc('\n', zefile);
     }
     else if (ch == START_LINK) {
	  HTMLSetLinepos(CurrentHTML, Linknum, CurrentLinepos+1);
	  HTMLSetLinenum(CurrentHTML, Linknum, CurrentLine);	  
	  Linknum++;
	  
	  CurrentLinepos++;
	  fputc('[', zefile);
     }
     else if (ch == END_LINK) {
	  CurrentLinepos++;
	  fputc(']', zefile);
     }
     else {
	  CurrentLinepos++;
	  fputc(ch, zefile);
     }
}

/*
 * This is the same as add_ch, except it underlines
 */

static addul_ch(ch, zefile)
  char ch;
  FILE *zefile;
{
     fputc('_', zefile);
     fputc('\b', zefile);
     add_ch(ch, zefile);
}

/*
 * Add a string to the file using add_ch
 */

static add_string(cp, zefile)
  FILE *zefile;
  char *cp;
{
     
     while (*cp != '\0')
	  add_ch(*cp++, zefile);
}

/*
 * Add an underlined string
 */

static addul_string(cp, zefile)
  FILE *zefile;
  char *cp;
{
     
     while (*cp != '\0')
	  addul_ch(*cp++, zefile);
}

/*
 * Flush the paragraph buffer
 */
flush_null(zefile)
  FILE *zefile;
{
     paraptr = 0;
}

/*
 * Output the buffer as is
 */

flush_raw(zefile)
  FILE *zefile;
{
     if (paraptr == 0)
	  return;

     *(parastorage +paraptr) = '\0';

     if (inXMP == TRUE || inLISTING==TRUE) {
	  add_string(parastorage, zefile);
	  paraptr = 0;
	  return;
     }

     add_ch('\n', zefile);
     
}


char *
strip_crap_begin(cp)
  char *cp;
{
     while (*cp == ' ' || *cp == '\n') {
	  cp++;
     }
     return(cp);
}





/*
 * Add a generalized paragraph.  Does word wrapping, justification,
 * leading space, total indentation level, bullets, and underlining
 */
static flush_it(zefile, leading, indent, bullet, justify, style)
  FILE *zefile;
  int leading;
  int indent;
  char *bullet;
  int justify;
  int style;
{
     char *lastspace = NULL;
     register char *cp;
     char *beginline = parastorage;
     int i;
     int testlinepos=0;
     
     while (CurrentLinepos < indent) {
	  add_ch(' ', zefile);
     }		    
     
     /** Add leading space **/
     while (leading-- != 0)
	  add_ch(' ', zefile);
     
     /** Add bullet space **/
     if (bullet != NULL)
	  add_string(bullet, zefile);

     testlinepos = CurrentLinepos;
     cp = beginline = strip_crap_begin(parastorage);

     for (; cp < parastorage+paraptr; cp++, testlinepos++) {

	  if (*cp == '\n')
	       *cp = ' ';
	  if (*cp == ' ')
	       lastspace = cp;
	  
	  if (testlinepos == COLS-1) {
	       if (lastspace == NULL)  {  /** Big long line, chop it. **/
		    char tempch = *cp;

		    *cp = '\0';
		    if (style == STYLE_UL)
			 addul_string(beginline, zefile);
		    else
			 add_string(beginline, zefile);
		    add_ch('\n', zefile);
		    *cp = tempch;
		    cp = strip_crap_begin(cp);
		    beginline = cp;
	       } else {
		    *lastspace = '\0';
		    
		    /** Justify here **/
		    if (justify == JUSTIFY_RIGHT) {
			 for (i=CurrentLinepos; i<(COLS -(lastspace-beginline)); i++)
			      add_ch(' ', zefile);
		    }
		    if (style == STYLE_UL)
			 addul_string(beginline, zefile);
		    else
			 add_string(beginline, zefile);

			 add_ch('\n', zefile);
		    cp = strip_crap_begin(lastspace+1);
		    beginline = cp;
	       }

	       lastspace = NULL;
	       
	       while (CurrentLinepos < indent) {
		    add_ch(' ', zefile);
	       }

	       if (bullet != NULL) {
		    for (i=0; i <strlen(bullet); i++)
			 add_ch(' ', zefile);
	       }
	       
	       testlinepos = CurrentLinepos;
	  }
     }
     if (justify == JUSTIFY_RIGHT) {
	  while (CurrentLinepos < (COLS-strlen(beginline)-1))
	       add_ch(' ', zefile);
     } else if (justify == JUSTIFY_CENTER) {
	  while (CurrentLinepos < ((COLS-strlen(beginline)-1)/2))
	       add_ch(' ', zefile);
     }
     if (style == STYLE_UL)
	  addul_string(beginline, zefile);
     else
	  add_string(beginline, zefile);

     if (inDT == FALSE) {
	  add_ch('\n', zefile);
	  if (inADDRESS == FALSE)
	       add_ch('\n', zefile);
     }

     paraptr = 0;
}

/*
 * Output the paragraph  take note of our state and add different styles.
 */

static flush_paragraph(zefile)
  FILE *zefile;
{
     char *cp;

     if (paraptr == 0)
	  return;
     
     /*** If it's a bunch of '\n''s skip it... ***/
     for (cp = parastorage; (*cp == '\n'); cp++)
	  ;
     if (*cp == '\0')
	  return;

     if (inADDRESS==TRUE)
	  flush_it(zefile, 0, HeadingLevel*3, NULL, JUSTIFY_RIGHT, STYLE_NL);
     else if (inDL==TRUE) {
	  if (inDT)
	       flush_it(zefile, 0, HeadingLevel*3, NULL, JUSTIFY_LEFT, STYLE_UL);
	  else
	       flush_it(zefile, 1, (HeadingLevel+1)*3, NULL, JUSTIFY_LEFT, STYLE_NL);
     }
     else if (inLIST==TRUE) {
	  flush_it(zefile, 0, HeadingLevel*3, "* ", JUSTIFY_LEFT,STYLE_NL);
     }
     else if (inH1 == TRUE)
	  flush_it(zefile, 0, 0, NULL, JUSTIFY_CENTER, STYLE_UL);
     else if (inHeading == TRUE)
	  flush_it(zefile, 0, (HeadingLevel-1)*3, NULL, JUSTIFY_LEFT, STYLE_UL);
     else {
	  flush_it(zefile, 3, HeadingLevel*3, NULL, JUSTIFY_LEFT, STYLE_NL);
     }
}

HTMLfromNet(html, sockfd)
  HTMLObj *html;
  int sockfd;
{
     ;
}

do_html(ZeGopher)
  GopherObj *ZeGopher;
{
     FILE *tmpfile;
     char tmpfilename[256];
     char inputline[512];
     char outputline[512];
     char *cp;
     int sockfd, iLength;

     DebugGSplusPrint(ZeGopher,"do_html start");
     if (parastorage == NULL) {
	  parasize = 4096;
	  paraptr = 0;
	  parastorage = (char *) malloc(parasize);
	  bzero(parastorage, 4096);
     }

     CurrentHTML = HTMLnew(32);

     if ((sockfd = GSconnect(ZeGopher)) <0) {
	  check_sock(sockfd, GSgetHost(ZeGopher), GSgetPort(ZeGopher));
	  return;
     }

     /** Send out the request **/

     writestring(sockfd, GSgetPath(ZeGopher));
     writestring(sockfd, "\r\n");

     /** Open a temporary file **/

     sprintf(tmpfilename, "/tmp/gopherhtml.%d.%d",getpid(),HTMLfilenum++);

     if ((tmpfile = fopen(tmpfilename, "w")) == NULL)
	  fprintf(stderr, "Couldn't make a tmp file!\n"), exit(-1);

     for(;;) {

	  iLength = readline(sockfd, inputline, 512);
	  outputline[0] = '\0';
	  if (iLength == 0)
	       break;

	  ZapCRLF(inputline);

	  for (cp=inputline; *cp != '\0'; cp++) {

	       if (*cp == '<') { /** Start of tag? **/

		    cp = process_tag(cp, tmpfile);
	       } 
	       else
		    buffer_ch(*cp);
	  } 
	  buffer_ch('\n');
     }

     flush_paragraph(tmpfile);

     if (isIndex)
	  ;/*     add_index_entry(zefile);*/

     (void)fclose(tmpfile);

     
/*     display_file(tmpfilename, GSgetTitle(ZeGopher));*/

     CURexit(CursesScreen);

     HTML_pager(tmpfilename, CurrentHTML);

     /** Good little clients clean up after themselves..**/

     if (unlink(tmpfilename)!=0)
	  fprintf(stderr, "Couldn't unlink!!!\n"), exit(-1);

     CURenter(CursesScreen);
}



char *
process_tag(cp, tmpfile)
  char *cp;
  FILE *tmpfile;
{
     char *cp2;
     char *endtag;
     static GopherObj *Anchorgs=NULL;

     
     for (cp2 = cp+1;
	  *cp2 != '>' && *cp2 != '<' && *cp2 != '\0';
	  cp2++)
	  if (*cp2 == ' ')
	       endtag = cp2;
     
     if (endtag==NULL) endtag=cp2;

     if (*cp2 != '>')   /** Not a tag **/
	  return(cp);

     /** It's a tag.. */


     /*** Check for these cases first, they're weird ***/

     if (inXMP == TRUE) {
	  if (strncasecmp(cp, "</XMP",5)==0) {
	       flush_raw(tmpfile);
	       inXMP = FALSE;
	       return(cp2);
	  }
	  inPAREN = TRUE;
	  buffer_ch(*cp);
	  return(cp);
     }
     else if (inLISTING == TRUE) {
	  if (strncasecmp(cp, "</LISTING",5)==0) {
	       flush_raw(tmpfile);
	       inLISTING = FALSE;
		    return(cp2);
	  }
	  inPAREN = TRUE;
	  buffer_ch(*cp);
	  return(cp);
     }



     /** Check for tags that are embedded in paragraphs **/
     if (strncasecmp(cp, "<A ", 3)==0) {
	  char *href;

	  href = strstr(cp, "HREF=");
	  if (href == NULL)
	       return(cp2);
	       
	  href += 5;
	  if (Anchorgs == NULL)
	       Anchorgs = GSnew();
	  else
	       GSinit(Anchorgs);

	  *cp2 = '\0';
	  GSfromHREF(Anchorgs, href);

	  /** An anchor link **/
	  buffer_ch(START_LINK);
	       
	  inANCHOR = TRUE;
	  return(cp2);

     }
     else if (strncasecmp(cp, "</A>", 4)==0) {
	  if (inANCHOR == TRUE) {
	       GSsetTitle(Anchorgs, parastorage+anchoroffset); /** Ick **/
	       buffer_ch(END_LINK);
	       buffer_ch(' ');
	       inANCHOR = FALSE;
	  }
	  HTMLaddLink(CurrentHTML, Anchorgs, 42,42);

	  return(cp2);
     }

     /** Okay, anything else ends a paragraph, if we're in it... **/

     if (inPAREN == TRUE) {
	  flush_paragraph(tmpfile);
	  inPAREN = FALSE;
     }

     /*** Assume that we're in a paragraph ***/
     inPAREN = TRUE;

     /** Any other tag is the start of something new **/
     
     if (strncasecmp(cp, "<P", 2)==0) {
	  inPAREN = TRUE;
     }

     else if (strncasecmp(cp, "<ADDRESS", 8)==0) {
	  inADDRESS =TRUE;
	  add_ch('\n', tmpfile);
     }

     else if (strncasecmp(cp, "</ADDRESS",9)==0) {
	  inADDRESS = FALSE;
     }
     
     else if (strncasecmp(cp, "<TITLE",6)==0) {
	  inPAREN = FALSE;
     }
     else if (strncasecmp(cp, "</TITLE",7)==0) {
	  flush_null(tmpfile);
     }
     else if (strncasecmp(cp, "<H1", 3) ==0) {
	  inH1 = TRUE;
     }
     else if (strncasecmp(cp, "<H",2) ==0) {
	  HeadingLevel = *(cp+2) - '0' - 1;
	  inHeading = TRUE;
     }
     else if (strncasecmp(cp, "</H1", 4) ==0) {
	  inH1=FALSE;
     }
     else if (strncasecmp(cp, "</H", 3) ==0) {
	  inHeading = FALSE;
     }
     else if (strncasecmp(cp, "<XMP", 4)==0) {
	  add_ch('\n',tmpfile);
	  inXMP = TRUE;
	  inPAREN = FALSE;
     }
     else if (strncasecmp(cp, "<LISTING",7)==0) {
	  add_ch('\n',tmpfile);
	  inLISTING = TRUE;
	  inPAREN = FALSE;
     }
     else if (strncasecmp(cp, "<DL",3)==0) {
	  inDL = TRUE;
	  inPAREN = FALSE;
     }
     else if (strncasecmp(cp, "<DD", 3)==0) {
	  inPAREN = TRUE;
	  inDT = FALSE;

	  /*flush_term(tmpfile);*/
     }

     else if (strncasecmp(cp, "<DT", 3)==0) {
	  inDT = TRUE;
     }
     
     else if ( strncasecmp(cp, "</DL",4)==0) {
	  inDL = FALSE;
     }
     
     else if (strncasecmp(cp, "<UL",3)==0) {
	  inLIST = TRUE;
     }
     else if (strncasecmp(cp, "<MENU",5)==0) {
	  inMENU =TRUE;
     }
     else if (strncasecmp(cp, "</MENU",6)==0) {
	  inMENU=FALSE;
     }
     else if (strncasecmp(cp, "<LI", 3)==0) {
	  ;
     }
     else if (strncasecmp(cp, "</UL", 4)==0) {
	  inLIST = FALSE;
     }

     else if (strncasecmp(cp, "</ISINDEX", 9)==0) {
	  isIndex = TRUE;
     }
     
     else if (strncasecmp(cp, "<PLAINTEXT", 10)==0) {
	  inPLAINTXT = TRUE;
     }
     return(cp2);
}
