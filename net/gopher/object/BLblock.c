/********************************************************************
 * lindner
 * 3.7
 * 1993/07/29 19:59:23
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/BLblock.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: BLblock.c
 * Implementation of Block handling routines.
 *********************************************************************
 * Revision History:
 * BLblock.c,v
 * Revision 3.7  1993/07/29  19:59:23  lindner
 * Removed extraneous variables
 *
 * Revision 3.6  1993/07/27  05:30:17  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.5  1993/06/15  06:10:34  lindner
 * Moved string.h include
 *
 * Revision 3.4  1993/04/27  20:54:32  lindner
 * really fixed BLtoNet()
 *
 * Revision 3.3  1993/04/23  20:14:48  lindner
 * Fix for BltoNet(), using wrong datatype
 *
 * Revision 3.2  1993/03/26  19:49:34  lindner
 * More comments, memory leak fix, etc
 *
 * Revision 3.1.1.1  1993/02/11  18:03:06  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:45:54  lindner
 * Fixes for retrieving from the net.  New BLAsearch
 *
 * Revision 1.1  1993/01/31  00:22:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "String.h"
#include "BLblock.h"
#include "Malloc.h"
#include <stdio.h>

/*
 * Make a new Block Object
 */

Blockobj *
BLnew()
{
     Blockobj *temp;

     temp = (Blockobj *) malloc(sizeof(Blockobj));
     
     temp->btype = BLOCK_UNKNOWN;
     temp->Blockname = STRnew();
     temp->datatype  = BDATA_NONE;
     
     return(temp);
}
     

/*
 * Initialize the block structure
 */

void
BLinit(bl)
  Blockobj *bl;
{
     STRinit(bl->Blockname);
     switch (BLgetDatatype(bl)) {
     case BDATA_FILE:
	  STRinit(bl->data.filename);
	  break;
     case BDATA_GREF:
	  GSinit(bl->data.gs);
	  break;
     case BDATA_TEXT:
	  STAinit(bl->data.text);
	  break;
     }
}

/*
 * Calls the right destory fcn depending on the object in the union.
 * 
 * wouldn't c++ be nice right here? :-)
 */

static void
BLdatadestroy(bl)
  Blockobj *bl;
{
     switch (BLgetDatatype(bl)) {
     case BDATA_FILE:
	  STRdestroy(bl->data.filename);
	  break;
     case BDATA_TEXT:
	  STAdestroy(bl->data.text);
	  break;
     case BDATA_GREF:
	  GSdestroy(bl->data.gs);
	  break;
     }
}

/*
 * Free memory of BlockObj
 */

void
BLdestroy(bl)
  Blockobj *bl;
{
     STRdestroy(bl->Blockname);
     BLdatadestroy(bl);
     free(bl);
}


void
BLcpy(dest, orig)
  Blockobj *dest,*orig;
{
     BLsetName(dest,BLgetName(orig));
     dest->btype = orig->btype;

     switch (BLgetDatatype(orig)) {
     case BDATA_FILE:
	  BLsetFile(dest, STRget(orig->data.filename));
	  break;
     case BDATA_GREF:
	  BLsetGref(dest, orig->data.gs);
	  break;
     case BDATA_TEXT:
	  BLsetText(dest, orig->data.text);
	  break;
     }
}


/*
 * BLgetNumLines() returns the number of lines for the Text record 
 * It returns -1 if this isn't the type of record.
 */

int
BLgetNumLines(bl)
  Blockobj *bl;
{
     switch (BLgetDatatype(bl)) {
     case BDATA_TEXT:
	  return(STAgetTop(bl->data.text));
     default:
	  return(-1);
     }
     ;
}


/*
 * BLgetLine() returns a character pointer to the line for the Text data type
 */

char*
BLgetLine(bl, lineno)
  Blockobj *bl;
  int lineno;
{
     switch (BLgetDatatype(bl)) {
     case BDATA_TEXT:
	  return(STRget(STAgetEntry(bl->data.text, lineno)));
     }
     return(NULL);
}


/*
 * BLsetFile() sets up a block to contain a file reference.
 * Really handy for the server.
 */

void
BLsetFile(bl, filename)
  Blockobj *bl;
  char *filename;
{
     /** Reset data field and put in new values **/

     BLdatadestroy(bl);

     bl->datatype      = BDATA_FILE;
     bl->data.filename = STRnew();

     STRset(bl->data.filename, filename);
}


/*
 * BLsetText sets everything up for storing text, if the optional parameter
 * sta is set, it copies it in.
 */

void
BLsetText(bl, sta)
  Blockobj *bl;
  StrArray *sta;
{
     /** Reset data field and put in new values **/
     if (BLgetDatatype(bl) != BDATA_TEXT) {
	  BLdatadestroy(bl);

	  bl->datatype = BDATA_TEXT;
	  bl->data.text = STAnew(10);
     }
     if (sta != NULL) {
	  STAcpy(bl->data.text, sta);
     }
}


/*
 * Keep data in memory, text is a line to be added to the text
 */

void
BLaddText(bl, text)
  Blockobj *bl;
  char *text;
{
     String   *tempstr;

     BLsetText(bl, NULL);

     tempstr = STRnew();
     
     STRset(tempstr, text);
     STApush(bl->data.text, tempstr);

     STRdestroy(tempstr);
}

/*
 * Make the block a gopher-reference
 */

void
BLsetGref(bl, gs)
  Blockobj *bl;
  GopherObj *gs;
{
     /** Reset data field and put in new values **/
     BLdatadestroy(bl);

     bl->datatype = BDATA_GREF;
     
     bl->data.gs = GSnew();
     
     GScpy(bl->data.gs, gs);
     
     GSdestroy(gs);
}


/*
 * BLtoNet() transmits the block as per gopher+ protocol
 * if the data field is a file, it opens it and sends it out
 */

void
BLtoNet(bl, fd)
  Blockobj *bl;
  int fd;
{
     FILE *infile;
     int i;
     char outputline[512];

     /** Switch on data type **/

     sprintf(outputline, "+%s:", BLgetName(bl));
     writestring(fd, outputline);

     switch (BLgetDatatype(bl)) {

     case BDATA_GREF:
	  writestring(fd, " ");
	  GStoNet(bl->data.gs,fd);
	  break;

     case BDATA_TEXT:
	  writestring(fd, "\r\n");
	  for (i=0; i<STAgetTop(bl->data.text); i++) {
	       writestring(fd, " ");
	       writestring(fd, STRget(STAgetEntry(bl->data.text, i)));
	       writestring(fd, "\r\n");
	  }
	  break;

     case BDATA_FILE:
	  writestring(fd, "\r\n");

	  if ((infile = fopen(STRget(bl->data.filename), "r"))==NULL)
	       break;
	  
	  while (fgets(outputline, sizeof(outputline), infile)!=NULL)  {
	       ZapCRLF(outputline);
	       writestring(fd, " ");
	       writestring(fd, outputline);
	       writestring(fd, "\r\n");
	  }
	  fclose(infile);
	  break;
     }
}


/*
 * BLfromNet() assumes that the initial '+' in the data stream has been read,
 * along with the blockname, up to the ':', but not anything after..
 * 
 * It then executes most of the state diagram, it returns when it has
 * encountered EOF, or encounters the next '+', or '.'
 *
 * Returns the following:
 *  0 for EOF encountered, block retrieved successfully
 *  1 for successful retrieve, and another block coming
 *  neg value for error conditions from read routines and botched block vals
 */
      
int
BLfromNet(bl, fd, blockname)
  Blockobj *bl;
  int fd;
  char *blockname;
{
     char inputline[512];
     int err;


     /*** State: _GotBlockName_ ***/

     BLsetName(bl, blockname);

     /** Find out if there's a gopher reference **/
     err = readrecvbuf(fd, inputline, 1);
     
     if (*inputline == ' ') {
	  /** GREF coming up, State: _GotBnameSpace_ **/
	  GopherObj *gs;

	  gs = GSnew();
	  GSfromNet(gs, fd);
	  BLsetGref(bl, gs);
	  /** read up to the next \r\n+ **/
	  while (1) {
	       if ((err= readrecvbuf(fd, inputline, 1)) <= 0)
		    return(err);
	       if (*inputline == '+')
		    return(1);
	       else {
		    err = readline(fd, inputline, sizeof(inputline));
		    if (err <= 0)
			 return(err); /*** Error ***/
	       }
	  }
     }
     
     /** Okay, let's just stick the text in verbatim **/

     /** get rid of remaining newline **/
     readline(fd, inputline, sizeof(inputline));

     /** State: _FirstChar_ **/

     while (1) {
	  /** Check for plus **/
	  if ((err= readrecvbuf(fd, inputline, 1)) <=0)
	       return(err);
	  
	  if (*inputline == '+')
	       return(1);
	       /*** Return to state _NewBlock_ ***/
	  
	  if (*inputline == '.') {
	       readline(fd, inputline, sizeof(inputline));
	       return(0);
	  }

	  /** add a line otherwise   State: _Addline_ **/
	  readline(fd, inputline, sizeof(inputline));
	  ZapCRLF(inputline);
	  BLaddText(bl, inputline);
     }
}


int
BLAsearch(bla, bname)
  BlockArray *bla;
  char *bname;
{
     int i;
     
     for (i=0; i<BLAgetTop(bla); i++) {
	  if (strcmp(BLgetName(BLAgetEntry(bla,i)), bname)==0)
	       return(i);
     }
     
     return(-1);
}

