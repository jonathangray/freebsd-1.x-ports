/*
 * Copyright 1992, 1993 The University of Newcastle upon Tyne
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

#ifndef _AxeText_h
#define _AxeText_h

#include <AxeiiText.h>

typedef struct _AxeTextClassRec	*AxeTextWidgetClass;
typedef struct _AxeTextRec	*AxeTextWidget;

extern WidgetClass axeTextWidgetClass;

#define XtNassociatedDirectory "associatedDirectory"
#define XtCAssociatedDirectory "AssociatedDirectory"
#define XtNgrabOnPopup      "grabOnPopup"
#define XtCGrabOnPopup      "GrabOnPopup"
#define XtNchangeCallback   "changeCallback"
#define XtNmessageCallback  "messageCallback"
#define XtNenableBackups    "enableBackups"
#define XtCEnableBackups    "Enablebackups"
#define XtNbackupNamePrefix "backupNamePrefix"
#define XtCBackupNamePrefix "BackupNamePrefix"
#define XtNbackupNameSuffix "backupNameSuffix"
#define XtCBackupNameSuffix "BackupNameSuffix"

#define AxeTextsaveSuccess           1

#define AxeTexterrorCodes           10
#define AxeTextnotSaved             11
#define AxeTextsaveFailure          12
#define AxeTextinvalidFilename      13
#define AxeTextinvalidDirectory     14
#define AxeTextinvalidControlCode   15
#define AxeTextreloadFailure        16
#define AxeTextunreadableFile       17
#define AxeTextnoUndo               18

extern Boolean AxeTextIsModified();
extern Widget AxeTextWidgetParentOf();
extern Widget AxeTextFileNominator();
extern String AxeTextGetAssociatedFile();
extern Boolean AxeTextSaveFile();
extern Boolean AxeTextSaveAsFile();
extern void AxeTextFileToNominatorStruct();
extern void AxeTextFreeNominatorStruct();
extern Boolean AxeTextLoadFile();
extern void AxeTextGotoLine();

#endif /* _AxeText_h */
