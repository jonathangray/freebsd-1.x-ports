/*
 * Copyright 1991, 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#ifndef FileNominator_h
#define FileNominator_h

#define XtNinitialDirectory "initialDirectory"
#define XtNselectCallback "selectCallback"
#define XtNpathList "pathList"
#define XtNcancelCallback "cancelCallback"
#define XtNmargin "margin"
#define XtNnumberRows "numberRows"
#define XtNshowDotFiles "showDotFiles"
#define XtNbellLevel "bellLevel"
#define XtNfilter "filter"
#define XtNfilterDirectoryNames "filterDirectoryNames"
#define XtNuserData "userData"

#define XtCInitialDirectory "InitialDirectory"
#define XtCPathList "PathList"
#define XtCShowDotFiles "ShowDotFiles"
#define XtCBellLevel "BellLevel"
#define XtCFilter "Filter"
#define XtCFilterDirectoryNames "FilterDirectoryNames"
#define XtCUserData "UserData"

extern WidgetClass fileNominatorWidgetClass;

typedef struct _FileNominatorRec *FileNominatorWidget;
typedef struct {
    String  directoryPart;
    int     directoryStatus;
    String  filenamePart;
    int     filenameStatus;
} FileNominatorStruct;

#define FileNominatorNonexistent 8
#define FileNominatorReadable    4
#define FileNominatorWritable    2
#define FileNominatorExecutable  1

String FileNominatorGetDirectory();
void   FileNominatorSetDirectory();
void   FileNominatorClearName();
Widget FileNominatorViewportWidget();
Widget FileNominatorListWidget();

#endif /* FileNominator_h */
