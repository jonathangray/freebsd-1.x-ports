/********************************************************************
 * lindner
 * 3.6
 * 1993/08/19 20:25:46
 * /home/mudhoney/GopherSrc/CVS/gopher+/gopherd/ext.c,v
 * Exp
 *
 * Paul Lindner, University of Minnesota CIS.
 *
 * Copyright 1991, 1992 by the Regents of the University of Minnesota
 * see the file "Copyright" in the distribution for conditions of use.
 *********************************************************************
 * MODULE: ext.c
 * These fns define mapping of file extensions to gopher objects.
 *********************************************************************
 * Revision History:
 * ext.c,v
 * Revision 3.6  1993/08/19  20:25:46  lindner
 * Mitra's Debug patch
 *
 * Revision 3.5  1993/08/03  06:03:08  lindner
 * Ignored files are now ignored again
 *
 * Revision 3.4  1993/07/31  00:15:37  lindner
 * Fixed weird extension problem
 *
 * Revision 3.3  1993/07/27  20:15:29  lindner
 * Fixed bug where filename was less than extension
 *
 * Revision 3.2  1993/07/23  03:18:46  lindner
 * Mods for using decoder:'s
 *
 * Revision 3.1.1.1  1993/02/11  18:02:51  lindner
 * Gopher+1.2beta release
 *
 * Revision 1.3  1993/02/09  22:13:19  lindner
 * many additions for multilingual gopher
 *
 * Revision 1.2  1993/01/30  23:56:55  lindner
 * Lots of new code for gopher+ stuff..  Change everything to start with EX
 *
 * Revision 1.1  1992/12/10  23:13:27  lindner
 * gopher 1.1 release
 *
 *
 *********************************************************************/

#include "ext.h"
#include "Malloc.h"
#include <ctype.h>
#include "util.h"
#include "openers.h" 
#undef stat
#include "Debug.h"

/*
 * Some functions to initialize and destroy sites and extensions..
 * (Needed for use with DynArrays...)
 */

Extobj *
EXnew()
{
     Extobj *temp;

     temp = (Extobj *) malloc(sizeof(Extobj));
     
     if (temp == NULL)
	  return(NULL);

     temp->Objtype   = '\0';
     temp->View      = STRnew();
     temp->ViewLang  = STRnew();
     temp->Prefix    = STRnew();
     temp->ext       = STRnew();
     temp->exttype   = EXT_ERROR;
     temp->GenericData = STRnew();

     STRinit(temp->View);
     STRinit(temp->ViewLang);
     STRinit(temp->Prefix);
     STRinit(temp->ext);
     STRinit(temp->GenericData);
     
     return(temp);
}


/** Destroy an extension structure **/

void 
EXdestroy(ext)
  Extobj *ext;
{
     STRdestroy(ext->View);
     STRdestroy(ext->ViewLang);
     STRdestroy(ext->Prefix);
     STRdestroy(ext->ext);
     STRdestroy(ext->GenericData);
     
     free(ext);
}


/** Copy an extension structure **/

void
EXcpy(ext1, ext2)
  Extobj *ext1, *ext2;
{
     ext1->Objtype  = ext2->Objtype;
     STRcpy(ext1->View, ext2->View);
     STRcpy(ext1->ViewLang, ext2->ViewLang);
     STRcpy(ext1->Prefix, ext2->Prefix);
     STRcpy(ext1->ext, ext2->ext);
     STRcpy(ext1->GenericData, ext2->GenericData);
     EXsetExttype(ext1, EXgetExttype(ext2));
}

char *
EXAfindFile(extarr, file, view)
  ExtArray *extarr;
  char *file;
  char *view;
{
     boolean status = TRUE;
     Extobj *ext, *temp;
     struct stat statbuf;
     static char tmpfile[512];
     int tmpfilelen = 0, i;

     if (file == NULL || *file == '\0')
	  return(file);

     strcpy(tmpfile, file);
     ext = EXnew();

     /** Find a viewext if available..
         Should be able to cycle through all of them... **/
     status = EXAviewSearch(extarr, ext, view);
     
     if (status == TRUE)
	  strcat(tmpfile, EXgetExt(ext));

     /** See if the file exists... **/
     
     if (!rstat(tmpfile, &statbuf))
	  /** Found it! **/
	  return(tmpfile);

     tmpfilelen = strlen(tmpfile);

     /** Check for decoders.. **/
     for (i=0; i< EXAgetNumEntries(extarr); i++) {
	  temp = EXAgetEntry(extarr,i);
     
	  if (EXgetExttype(temp) == EXT_DECODER) {
	       char decodertmp[256];
	       
	       strcpy(decodertmp, tmpfile);
	       strcat(decodertmp, EXgetExt(temp));
	       
	       if (!rstat(decodertmp, &statbuf)) {
		    /** Found it! **/
		    strcpy(tmpfile, decodertmp);
		    return(tmpfile);
	       }
	  }

     }
     /*** Couldn't find anything... **/
     return(file);
}

/*
 * Get the parameters associated with a particular extension
 *
 * Fills in ext with the values..
 */

boolean
EXAsearch(extarr, ext, fileext, exttype)
  ExtArray *extarr;
  Extobj *ext;
  char *fileext;
  int exttype;
{
     int i, extlen;
     Extobj *temp;

     Debug("EXAsearch:\r\n",NULL)
     extlen = strlen(fileext);

     /*** Linear search.  Ick. ***/
     
     for (i=0; i< EXAgetNumEntries(extarr); i++) {
	  temp = EXAgetEntry(extarr,i);
	  
	  if (exttype == EXgetExttype(temp)) {
	       int exlen = strlen(EXgetExt(temp));

	       if (exlen <= extlen && strcasecmp(fileext+extlen-exlen,
			      EXgetExt(temp)) == 0) {
		    EXcpy(ext,temp);
		    return(TRUE);
	       }
	  }
     }
     return(FALSE);
}


/*
 * Do an exact search...
 */

boolean
EXAcasedSearch(extarr, ext, fileext, exttype)
  ExtArray *extarr;
  Extobj *ext;
  char *fileext;
  int exttype;
{
     int i, extlen;
     Extobj *temp;

     extlen = strlen(fileext);

     /*** Linear search.  Ick. ***/
     
     for (i=0; i< EXAgetNumEntries(extarr); i++) {
	  temp = EXAgetEntry(extarr,i);
	  
	  if (exttype == EXgetExttype(temp)) {
	       if (strcmp(fileext+extlen-strlen(EXgetExt(temp)),
			      EXgetExt(temp)) == 0) {
	       
		    EXcpy(ext,temp);
		    return(TRUE);
	       }
	  }
     }
     return(FALSE);
}


/*
 * Search for a specific view extension...
 */

boolean
EXAviewSearch(extarr, ext, view)
  ExtArray *extarr;
  Extobj   *ext;
  char     *view;
{
     int    i;
     Extobj *temp;
     char   viewnlang[256];

     /*** Linear search.  Ick. ***/
     
     for (i=0; i< EXAgetNumEntries(extarr); i++) {

	  temp = EXAgetEntry(extarr,i);

	  if (EXgetExttype(temp) == EXT_VIEW) {

	       strcpy(viewnlang, EXgetView(temp));
	       if (EXgetVLang(temp) != NULL) {
		    strcat(viewnlang, " ");
		    strcat(viewnlang, EXgetVLang(temp));
	       }
	       
	       if (strcasecmp(view, viewnlang)==0) {
		    EXcpy(ext,temp);
		    return(TRUE);
	       }
	  }
     }
     return(FALSE);
     ;
}


/*
 * Process gopherd.conf lines "viewext", "ignore", "blockext" and others.
 */


boolean 
EXAprocessLine(extarr, exttype, inputline, deflang)
  ExtArray *extarr;
  int       exttype;
  char     *inputline;
  char     *deflang;
{
     int    i;
     char   ext[64];
     char   secondparm[64];
     char   prefix[64];
     char   Gplustype[64];
     char   Gpluslang[16];
     Extobj *temp;

     temp = EXnew();

     /*** Set the type of extension we've been given ***/
     EXsetExttype(temp, exttype);

     if (deflang != NULL)
	  EXsetVLang(temp, deflang);


     inputline = skip_whitespace(inputline);
     if (*inputline == '\0') return(FALSE);

     /*** The first parameter, the extension ***/
     i=0;
     while (!isspace(*inputline) && (*inputline != '\0'))
	  ext[i++] = *inputline++;

     ext[i] = '\0';
     EXsetExt(temp,ext);

     
     inputline = skip_whitespace(inputline);

     if (exttype == EXT_IGNORE) {
	  EXAadd(extarr, temp);
	  EXdestroy(temp);
	  return(TRUE);
     }

     if (*inputline == '\0') return(FALSE);

     /*** Second parameter, depends on which token we're using.. ***/
     i=0;
     while (!isspace(*inputline) && (*inputline != '\0'))
	  secondparm[i++] = *inputline++;

     secondparm[i] = '\0';
     
     if (exttype == EXT_BLOCK || exttype == EXT_BLOCKREF) {
	  EXsetBlockname(temp, secondparm);
	  EXAadd(extarr, temp);
	  EXdestroy(temp);
	  return(TRUE);
     }
     else if (exttype == EXT_DECODER) {
	  EXsetDecoder(temp, secondparm);
	  EXAadd(extarr, temp);
	  EXdestroy(temp);
	  return(TRUE);
     }
     else if (exttype == EXT_VIEW)
	  EXsetObjtype(temp, *secondparm);
     
     if (*inputline == '\0') return(FALSE);

     inputline = skip_whitespace(inputline);
     if (*inputline == '\0') return(FALSE);

     /*** Prefix ***/
     i=0;
     while (!isspace(*inputline) && (*inputline != '\0'))
	  prefix[i++] = *inputline++;
     if (*inputline == '\0') return(FALSE);
     prefix[i]='\0';
     EXsetPrefix(temp, prefix);

     inputline = skip_whitespace(inputline);
     if (*inputline == '\0') return(FALSE);

     /*** Gopher + view type ***/
     i=0;
     while (!isspace(*inputline) && (*inputline != '\0'))
	  Gplustype[i++]= *inputline++;

     Gplustype[i] = '\0';
     EXsetView(temp, Gplustype);

     inputline = skip_whitespace(inputline);

     /** Gopher+ view language **/
     i=0;
     while (!isspace(*inputline) && (*inputline != '\0'))
	  Gpluslang[i++]= *inputline++;

     Gpluslang[i] = '\0';
     if (strlen(Gpluslang) > 0)
	  EXsetVLang(temp, Gpluslang);

     EXsetExttype(temp, EXT_VIEW);

     EXAadd(extarr, temp);
     EXdestroy(temp);
     return(TRUE);
}

