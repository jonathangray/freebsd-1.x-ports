/*
 * Copyright 1991, 1992, 1993 The University of Newcastle upon Tyne
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

#ifndef FileNominatorP_h
#define FileNominatorP_h

#include "FileNom.h"

typedef struct {
    XtPointer		extension;
    XtTranslations      pathTranslations;
    XtTranslations      listTranslations;
    XtTranslations      FilterTranslations;
    XtTranslations      filterTranslations;
    XtTranslations      filenameTranslations;
    XtTranslations      selectTranslations;

    Pixmap              tickMark;
} FileNominatorClassPart;

typedef struct _FileNominatorClassRec {
    CoreClassPart	        core_class;
    FileNominatorClassPart	fileNominator_class;
} FileNominatorClassRec, *FileNominatorClass;

extern FileNominatorClassRec fileNominatorClassRec;

typedef struct {
    /* resources */
    String              initial_directory;
    XtCallbackList      select_callback;
    String              path_list;
    XtCallbackList      cancel_callback;
    Dimension           margin;
    int                 number_rows;
    Boolean             show_dot_files;
    int                 bell_level;
    String              filter;
    Boolean             filter_directory_names;
    
    /* private data */
    Widget              viewport_widget;
    Widget              list_widget;
    Widget              Filter_widget;
    Widget              filter_menu;
    Widget              apply_dirs;
    Widget              apply_dots;
    Widget              filter_widget;
    Widget		filename_widget;
    Widget              text_widget;
    Widget              select_widget;
    Widget              path_widget;
    Widget		cancel_widget;	
    String              *listList;
    char                currentDir[MAXPATHLEN];
    Boolean             watchingChanges;
    FileNominatorStruct nomination;
    XtPointer           user_data;
} FileNominatorPart;

typedef struct _FileNominatorRec {
    CorePart		core;
    FileNominatorPart	fileNominator;
} FileNominatorRec;

#endif /* FileNominatorP_h */

