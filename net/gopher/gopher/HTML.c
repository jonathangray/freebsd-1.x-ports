/********************************************************************
 * lindner
 * 3.1.1.1
 * 1993/02/11 18:02:57
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopher/HTML.c,v
 * $Status: $
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: HTML.c
 * A lame attempt at doing HTML
 *********************************************************************
 * Revision History:
 * HTML.c,v
 * Revision 3.1.1.1  1993/02/11  18:02:57  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.1  1992/12/10  23:32:16  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  23:31:26  lindner
 * gopher 1.1 release
 *
 * Revision 1.1  1992/12/10  06:16:51  lindner
 * Initial revision
 *
 *
 *********************************************************************/


#include "HTML.h"

#include <string.h>
#include <stdio.h>


HTMLObj *
HTMLnew(size)
  int size;
{
     HTMLObj *temp;

     temp               = (HTMLObj*) malloc(sizeof(HTMLObj));
     
     temp->Linkmaxsize  = size;
     temp->Linklinenum  = (int *) malloc(size * sizeof(int));
     temp->Linklinepos  = (int *) malloc(size * sizeof(int));
     
     temp->Links        = GDnew(size);
     temp->Title        = STRnew();
     
     return(temp);
}


void
HTMLInit(html)
  HTMLObj *html;
{
     GDinit(html->Links);
     STRinit(html->Title);
}

void
HTMLDestroy(html)
  HTMLObj *html;
{
     GDdestroy(html->Links);
     STRdestroy(html->Title);

     STRdestroy(html->Filehtmlname);
     STRdestroy(html->Filetxtname);

     free(html->Linklinenum);
     free(html->Linklinepos);

     free(html);

}


void
HTMLgrow(html, size)
  HTMLObj *html;
{
     int *temp;

     if (size < html->Linkmaxsize) 
	  return;

     temp = (int *) realloc(html->Linklinenum, size);
     if (temp == NULL)
	  fprintf(stderr, "Out of memory!!!\n"), exit(-1);

     if (temp != html->Linklinenum) {
	  free(html->Linklinenum);
	  html->Linklinenum = temp;
     }

     temp = (int *) realloc(html->Linklinepos, size);
     if (temp == NULL)
	  fprintf(stderr, "Out of memory!!!\n"), exit(-1);

     if (temp != html->Linklinepos) {
	  free(html->Linklinepos);
	  html->Linklinepos = temp;
     }

}
     

void
HTMLaddLink(html, gs, linenum, offset)
  HTMLObj   *html;
  GopherObj *gs;
  int       linenum;
  int       offset;
{
     int top;

     if (html->Linkmaxsize == GDgetNumitems(html->Links)) {
	  html->Linkmaxsize *=2;
	  HTMLgrow(html->Linkmaxsize);
     }
     
     GDaddGS(html->Links, gs);

     top = GDgetNumitems(html->Links);

     HTMLSetLinepos(html, top-1, offset);
     HTMLSetLinenum(html, top-1, linenum);     
}
	 


