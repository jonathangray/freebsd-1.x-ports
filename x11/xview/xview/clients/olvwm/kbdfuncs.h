/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifndef _OLWM_KBDFUNCS_H
#define _OLWM_KBDFUNCS_H

#ident	"@(#)kbdfuncs.h	1.2 olvwm version 6/13/92"

/*
 * Based on
#ident	"@(#)kbdfuncs.h	1.5	91/09/14 SMI"
 *
 */

extern void KeyBackFocus();
extern void KeyBeep();
extern void KeyFocusToPointer();
extern void KeyRaiseLowerPointer();
extern void KeyFrontFocus();
extern void KeyFullRestore();
extern void KeyLockColormap();
extern void KeyMove();
extern void KeyNextApp();
extern void KeyNextWindow();
extern void KeyOpenClosePointer();
extern void KeyOpenCloseFocus();
extern void KeyOwner();
extern void KeyPrevApp();
extern void KeyPrevWindow();
extern void KeyProperties();
extern void KeyQuit();
extern void KeyRefresh();
extern void KeyResize();
extern void KeyToggleInput();
extern void KeyTogglePin();
extern void KeyUnlockColormap();
extern void KeyWindowMenu();
extern void KeyWorkspaceMenu();
extern void KeyToggleFullSizeZoomX();  /* Toggle resource FullSizeZoomX. */
extern void KeyToggleDragWindow();     /* Toggle resource DragWindow. */
extern void KeyToggleMoveGroups();     /* Toggle resource VirtualMoveGroups. */
extern void KeyToggleSticky();         /* Toggle Sticky State */
extern void KeySaveWorkspace();

extern void KeyMoveVDM();
extern void AddKeyBinding();

typedef struct {
    char *rsrc_name;
    char *dflt_binding;
    void (*function)();
    SemanticAction action;
    unsigned long flags;
} KeyDescriptor;

/* values for flags */
#define KD_IMMUNE       (1<<0)          /* immune to suspension */
#define KD_SUNVIEW      (1<<1)          /* active if mouseless == SunView */
#define KD_BASIC        (1<<2)          /* active if mouseless == basic */
#define KD_FULL         (1<<3)          /* active if mouseless == full */
#define KD_VIRTUAL	(1<<4)		/* active if VirtualGrabKeys == True */

#define KD_BASIC_FULL   (KD_BASIC | KD_FULL)
#define KD_ALWAYS       (KD_SUNVIEW | KD_BASIC_FULL)

#define KD_DYNAMIC	KD_IMMUNE

#endif /* _OLWM_KBDFUNCS_H */
