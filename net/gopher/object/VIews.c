/********************************************************************
 * lindner
 * 3.4
 * 1993/07/29 20:00:06
 * /home/mudhoney/GopherSrc/CVS/gopher+/object/VIews.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992, 1993 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: VIews.c
 * Various functions to handle gopher+ views
 *********************************************************************
 * Revision History:
 * VIews.c,v
 * Revision 3.4  1993/07/29  20:00:06  lindner
 * none
 *
 * Revision 3.3  1993/07/23  04:49:06  lindner
 * Added PrettyView fcn
 *
 * Revision 3.2  1993/03/24  17:09:01  lindner
 * Fixed a memory leak
 *
 * Revision 3.1.1.1  1993/02/11  18:03:01  lindner
 * Gopher+1.2beta release
 *
 * Revision 2.1  1993/02/09  22:48:50  lindner
 * Changes for multilingual views
 *
 * Revision 1.1  1993/01/31  00:31:12  lindner
 * Initial revision
 *
 *
 *********************************************************************/

#include "VIews.h"
#include "Malloc.h"
#include "String.h"
#include "BLblock.h"


static char *ANSILangs[] = {
     "Da_DK", "Nl_BE", "Nl_NL", "En_GB", "En_US", "Fi_FI", "Fr_BE",
     "Fr_CA", "Fr_CH", "Fr_FR", "De_CH", "De_DE", "El_GR", "Is_IS",
     "It_IT", "Jp_JP", "No_NO", "Pt_PT", "Es_ES", "Sv_SE", "Tr_TR",
     NULL
     };

static char *ANSILangEn[] = {
     "Danish",
     "Dutch (Belgium)",
     "Dutch",
     "English (Great Britian)",
     "English (USA)",
     "Finnish",
     "French (Belgium)",
     "French (Canada)",
     "French (Switzerland)",
     "French",
     "German (Switzerland)",
     "German",
     "Greek",
     "Icelandic",
     "Italian",
     "Japanese",
     "Norwegian",
     "Portuguese",
     "Spanish",
     "Swedish",
     "Turkish",
     NULL
     };


char *
VIprettyLang(vi, currentlang)
  VIewobj *vi;
  char    *currentlang;
{
     int i;

     for (i=0; ;i++) {
	  if (ANSILangs[i] == NULL)
	       return(NULL);

	  if (*(VIgetLang(vi)) == *(ANSILangs[i])) {
	       if (strcmp(ANSILangs[i], VIgetLang(vi)) == 0)
		    return(ANSILangEn[i]);
	  }
     }	  
}




VIewobj *
VInew()
{
     VIewobj *temp;

     temp = (VIewobj *) malloc(sizeof(VIewobj));

     temp->Type = STRnew();
     temp->Lang = STRnew();
     temp->Size = STRnew();
     temp->comments = STRnew();
     
     VIinit(temp);
     return(temp);
}

/*** Initialize the VIewobj ***/

void
VIinit(vi)
  VIewobj *vi;
{
     STRinit(vi->Type);
     STRinit(vi->Lang);
     STRinit(vi->Size);
     STRinit(vi->comments);
}


/** Destroy the VIewobj ***/

void
VIdestroy(vi)
  VIewobj *vi;
{
     STRdestroy(vi->Type);
     STRdestroy(vi->Lang);
     STRdestroy(vi->Size);
     STRdestroy(vi->comments);

     free(vi);
}


/** copy a VIewobj **/

void
VIcpy(videst, viorig)
  VIewobj *videst, *viorig;
{
     STRcpy(videst->Type, viorig->Type);
     STRcpy(videst->Lang, viorig->Lang);
     STRcpy(videst->Size, viorig->Size);
     STRcpy(videst->comments, viorig->comments);
}


/** Put a VIewobj in a line according to G+ protocol **/

void
VItoLine(vi, tmpstr)
  VIewobj *vi;
  char *tmpstr;
{
     char *cp;
     
     tmpstr[0] = ' ';
     tmpstr[1] = '\0';
     
     strcat(tmpstr, VIgetType(vi));

     cp = VIgetLang(vi);
     if (cp != NULL) {
	  strcat(tmpstr, " ");
	  strcat(tmpstr, cp);
     }

     /** Size **/
     strcat(tmpstr, ": <");
     strcat(tmpstr, VIgetSize(vi));
     strcat(tmpstr, ">");

     /** Comments **/
     cp = VIgetComments(vi);
     if (cp != NULL) {
	  strcat(tmpstr, " ");
	  strcat(tmpstr, cp);
     }
}


/** Siphon a G+ view line into a VIewobj **/

boolean
VIfromLine(vi, line)
  VIewobj *vi;
  char *line;
{
     char tmpstr[256], *cp;
     int i;

     /** Okay, read until the next space, put the result in tmpstr **/
     for (cp = line,i=0; (*cp != ' '&&*cp!='\0'&&*cp!=':'); cp++,i++)
	  tmpstr[i] = *cp;
     
     tmpstr[i]='\0';
     line = cp+1;
     VIsetType(vi, tmpstr);

     if (*line == '\0')
	  return(FALSE);


     /** Parse language **/
     if (*line != ':') {
	  for (cp = line,i=0; (*cp != ' '&&*cp!='\0'&& *cp!=':'); cp++,i++)
	       tmpstr[i] = *cp;
	  tmpstr[i]='\0';
	  line = cp+1;
	  VIsetLang(vi, tmpstr);
     }
	  
     if (*line == '\0')
	  return(FALSE);
     
     /** Okay, parse off the size **/
     while (*line != '<')
	  line ++;
     
     line++;

     for (cp = line,i=0; (*cp!='\0'&& *cp!='>'); cp++,i++)
	  tmpstr[i] = *cp;
     tmpstr[i]='\0';
     line = cp+1;
     VIsetSize(vi, tmpstr);

     if (*line != '\0')
	  VIsetComments(vi, line);

     return(TRUE);
}

char *
VIgetViewnLang(vi, line)
  VIewobj *vi;
  char *line;
{
     if (VIgetType(vi) == NULL)
	  return("");
     strcpy(line, VIgetType(vi));
     strcat(line, " ");
     if (VIgetLang(vi)!= NULL)
	  strcat(line, VIgetLang(vi));
     return(line);
}


/*
 * Generates a View Array from a Block
 */

void
VIAfromBL(via,bl)
  VIewArray *via;
  Blockobj *bl;
{
     int i;
     char *cp;
     VIewobj   *vi;

     if (strcasecmp(BLgetName(bl),"VIEWS")!=0)
	  return;

     vi  = VInew();
     
     for (i=0; i<BLgetNumLines(bl);i++) {
	  cp = BLgetLine(bl, i);
	  VIinit(vi);
	  
	  if (VIfromLine(vi, cp))
	       VIApush(via,vi);
	  else
	       ;
     }
     VIdestroy(vi);

}
  

char *
VIgetPrettyView(vi, line)
  VIewobj *vi;
  char *line;
{
     if (VIgetType(vi) == NULL)
	  return("");
     strcpy(line, VIgetType(vi));
     strcat(line, " ");
     if (VIgetLang(vi)!= NULL)
	  strcat(line, VIprettyLang(vi, ""));
     if (VIgetSize(vi)!= NULL)
	  sprintf(line, "%s [%s]", line, VIgetSize(vi));
     return(line);
}






