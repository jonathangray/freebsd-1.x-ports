/********************************************************************
 * lindner
 * 3.7
 * 1993/07/27 20:18:09
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/GSgopherobj.h,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: GSgopherobj.h
 * Header file, abstraction of a gopher type
 *********************************************************************
 * Revision History:
 * GSgopherobj.h,v
 * Revision 3.7  1993/07/27  20:18:09  lindner
 * Sorry can't take address of conditionals...
 *
 * Revision 3.6  1993/07/27  05:30:25  lindner
 * Mondo Debug overhaul from Mitra
 *
 * Revision 3.5  1993/07/27  00:30:11  lindner
 * plus patch from Mitra
 *
 * Revision 3.4  1993/07/23  04:50:59  lindner
 * Additional stored askdata
 *
 * Revision 3.3  1993/04/15  17:55:18  lindner
 * GSgetAdmin fix, plus more protos
 *
 * Revision 3.2  1993/03/24  17:09:42  lindner
 * Additions for Localfile for each GopherObj
 *
 * Revision 3.1.1.1  1993/02/11  18:03:03  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:47:54  lindner
 * Added new def GSSfindBlock
 *
 * Revision 1.2  1993/01/31  00:31:12  lindner
 * Gopher+ structures and macros.
 *
 * Revision 1.1  1992/12/10  23:27:52  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/


#ifndef GSGOPHEROBJ_H
#define GSGOPHEROBJ_H

#include "boolean.h"
#include "STRstring.h"
#include "STAarray.h"
#include "VIews.h"

#define A_FILE      '0'      /* Types of objects */
#define A_DIRECTORY '1'
#define A_CSO       '2'
#define A_ERROR     '3'
#define A_MACHEX    '4'
#define A_PCBIN     '5'
#define A_INDEX     '7'
#define A_TELNET    '8'
#define A_UNIXBIN   '9'
#define A_SOUND     's'
#define A_EVENT     'e'
#define A_CALENDAR  'c'
#define A_GIF       'g'
#define A_HTML      'h'
#define A_TN3270    'T'
#define A_MIME      'M'
#define A_IMAGE     'I'
#define A_EOI	    '.'

typedef struct g_struct GopherObj;
#include "BLblock.h"

/*** Our gopher+ attributes structure ***/

struct gplus_struct
{
     /** Abstract Block **/
     /* String *abstract;	Superceeded by BlockArray */

     /** Admin Block **/
     String *Admin;
     String *ModDate;

     VIewArray *Views;

     /** Others Blocks**/
     BlockArray *OtherBlocks;

     /** Filled in askdata **/
     char **Askdata;
};

typedef struct gplus_struct GplusObj;	  

/** Our Gopher Object Type **/

struct g_struct
{
     char    sFileType;     /* The type of object (A_FILE, A_CSO, etc)*/
     String  *Title;        /* User displayable title */
     String  *Selstr;       /* Selector string on host... */
     String  *Host;         /* Internet name of host                  */
     int     iPort;         /* Port number on host                    */
     int     Itemnum;       /* The number of the item in the directory*/
     int     weight;        /* The weight, expressed as a # from 0-1000 */

     String  *Localfile;    /* The local file of the item */

     boolean isgplus;       /* Item can be queried gopher+ style */
     boolean isask;         /* Item contains an ASK block */
     GplusObj *gplus;       /* Gopher + attributes */
};


typedef struct g_struct GopherStruct;

#define GSgetType(a) ((a)->sFileType)
#define GSsetType(a,b) (a)->sFileType=(b)

#define GSgetTitle(a) ((STRget((a)->Title)))
#define GSsetTitle(a,b) ((STRset((a)->Title, b)))

#define GSgetPath(a) ((STRget((a)->Selstr)))
#define GSsetPath(a,b) ((STRset((a)->Selstr, b)))

#define GSgetHost(a) ((STRget((a)->Host)))
#define GSsetHost(a,b) ((STRset((a)->Host, b)))

#define GSgetPort(a) ((a)->iPort)
#define GSsetPort(a,b) (a)->iPort=(b)

#define GSgetNum(a) ((a)->Itemnum)
#define GSsetNum(a,b) ((a)->Itemnum=(b))

#define GSgetWeight(a)   ((a)->weight)
#define GSsetWeight(a,b) ((a)->weight=(b))

#define GSgetLocalFile(a) ((STRget((a)->Localfile)))
#define GSsetLocalFile(a,b) ((STRset((a)->Localfile, (b))))

/****************** Gopher Plus attributes *******************/
#define GSisGplus(a)    ((a)->isgplus)
#define GSsetGplus(a,b) ((a)->isgplus=(b))

#define GSisAsk(a)      ((a)->isask)
#define GSsetAsk(a,b)   ((a)->isask=(b))


#define GSgplusInited(a) ((a)->gplus != NULL)

#define GSgetAdmin(a)    (((a)->gplus == NULL) ? NULL : (STRget((a)->gplus->Admin)))
#define GSgetModDate(a)  (STRget((a)->gplus->ModDate)) 

/* some compilers can't grok this... */
/*#define GSgetVIA(a) (((a)->gplus == NULL) ? NULL : ((a)->gplus->Views))*/
#define GSgetNumViews(a) (VIAgetTop((a)->gplus->Views))
#define GSgetView(a,b)   (VIAgetEntry((a)->gplus->Views,(b)))

/* Some of the code has been known to do a->gplus->OtherBlocks
   when gplus is NULL, leads to obscure errors :-) 
   
   Of course lots of compilers can't grok this...  sigh... */
#define GSgetOtherBlocks(a) (((a)->gplus->OtherBlocks))

#define GSgetNumBlocks(a) (BLAgetTop(GSgetOtherBlocks(a)))
#define GSgetBlock(a,b)   (BLAgetEntry(GSgetOtherBlocks(a),(b)))
#define GSfindBlock(a,b)  GSgetBlock(a,BLAsearch(GSgetOtherBlocks(a),(b)))

#define GSgetAskdata(a)   (GSgplusInited(a) ? (a)->gplus->Askdata : NULL)

/*** Real live functions defined in GSgopherobj.c ***/

GopherObj *GSnew();
GopherObj *GSnewSet();
void GSdestroy();
void GSinit();
void GSplusnew();
void GStoNet();
void GStoNetHTML();
int  GSfromNet();
int  GSfromLink();
void GStoLink();
void GScpy();
void GSsetBlock();

/****** Protocol transmission functions *******/
int  GSconnect();
void GStransmit();
void GSsendHeader();
void GSsendErrorHeader();
int  GSrecvHeader();
void GStransmit();

/******Gopher+ functions *******/
void GSplusnew();
void GSplusdestroy();
void GSplusInit();
void GSplustoNet();
void GSpluscpy();

char **GSsetAskdata();

/***** Debugging functions *******/
void GSplusPrint();

#endif /*GSGOPHEROBJ_H*/

