/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)globals.h	1.3 olvwm version 6/13/92"

/*
 * Based on
#ident	"@(#)globals.h	26.26	91/10/04 SMI"
 *
 */

#include "list.h"

typedef struct {
	unsigned int	modmask;
	KeyCode		keycode;
} KeySpec;

typedef enum { BeepAlways, BeepNever, BeepNotices } BeepStatus;

typedef enum { KbdSunView, KbdBasic, KbdFull } MouselessMode;

typedef enum { GridNone, GridInvisible, GridVisible } VirtualGridType;

typedef enum { UseNone, UseVDM, UseAll } ImageType;

typedef enum { SortYounger, SortAlpha } SortType;

typedef struct _globalResourceVariables {
	char		*WorkspaceColor;
	char		*WindowColor;
	char		*ForegroundColor;
	char		*BackgroundColor;
	char		*BorderColor;
	Bool		ReverseVideo;
	Bool		PaintWorkspace;
	Bool		F3dUsed;
	Bool		F3dFrames;
	Bool		F3dResize;
	XFontStruct    	*TitleFontInfo;
	XFontStruct	*TextFontInfo;
	XFontStruct    	*ButtonFontInfo;
	XFontStruct	*IconFontInfo;
	XFontStruct	*GlyphFontInfo;
	Cursor		BasicPointer;
	Cursor		MovePointer;
	Cursor		BusyPointer;
	Cursor		IconPointer;
	Cursor		ResizePointer;
	Cursor		MenuPointer;
	Cursor		QuestionPointer;
	Cursor		TargetPointer;
	Cursor		PanPointer;
	Bool		FocusFollowsMouse;
	Bool		ReparentFlag;
	char		*DefaultWinName;
	int		SaveWorkspaceTimeout;
	int		FlashTime;
	Bool		FShowMenuButtons;		/* XXX */
	Bool		FShowPinnedMenuButtons;		/* XXX */
	IconPreference	IconPlacement;
	Bool		FSnapToGrid;
	Bool		FocusLenience;
	Bool		DragWindow;
	Bool		AutoRaise;
	int		AutoRaiseDelay;
	Bool		PopupJumpCursor;
	Bool		ColorLocked;
	Bool		PPositionCompat;
	Bool		RefreshRecursively;
	BeepStatus	Beep;
	int		EdgeThreshold;
	int		DragRightDistance;
	int		MoveThreshold;
	int		ClickMoveThreshold;
	int		DoubleClickTime;
	int		RubberBandThickness;
	KeySpec		FrontKey;
	KeySpec		HelpKey;
	KeySpec		OpenKey;
	KeySpec		ConfirmKey;
	KeySpec		CancelKey;
	KeySpec		ColorLockKey;
	KeySpec		ColorUnlockKey;
	List		*Minimals;
	int		MouseChordTimeout;
	Bool		SingleScreen;
	Bool		AutoReReadMenuFile;
	Bool		KeepTransientsAbove;
	Bool		TransientsSaveUnder;
	Bool		TransientsTitled;
	Bool		SelectWindows;
	Bool		ShowMoveGeometry;
	Bool		ShowResizeGeometry;
	Bool		InvertFocusHighlighting;
	Bool		RunSlaveProcess;
	Bool		SelectToggleStacking;
	int		FlashCount;
	char		*DefaultIconImage;
	char		*DefaultIconMask;
	Bool		ServerGrabs;
	int		IconFlashCount;
	Bool		SelectDisplaysMenu;
	int		SelectionFuzz;
	Bool		AutoInputFocus;
	Bool		AutoColorFocus;
	Bool		ColorTracksInputFocus;
	int		IconFlashOnTime;
	int		IconFlashOffTime;
	MouselessMode	Mouseless;
	Bool		RaiseOnActivate;
	Bool		RestackWhenWithdraw;
#ifdef OW_I18N_L3
	OLLC		LC;
#endif /* OW_I18N_L3 */
/* Following are three entries are strictly for debugging purposes and 
 * are not mentioned in the usage message or doc.
 * Orphaned events are events that are associated with a window or frame 
 * has no entry in the frame hash table, or events that are not handled by the
 * various event handlers.
 * 'PrintAll' is useful for when verification of an events existance is needed.
 */
	Bool		PrintOrphans;
	Bool		PrintAll;
	Bool		Synchronize;
/*
 * Following are entries for the Virtual Desktop.
 */
	char		*VirtualDesktop;
	int		VDMScale;
	Bool		AllowMoveIntoDesktop;
	Bool		ArrowInRoot;
	char		*VirtualGeometry;
	char		*VirtualFontName;
	char		*VirtualBackgroundMap;
	char		*VirtualBackgroundColor;
	char		*VirtualPixmapColor;
	char		*VirtualIconGeometry;
	char		*VirtualForegroundColor;
	char		*VirtualFontColor;
	Bool		VirtualIconic;
	List		*StickyList;
	Bool		UseRelativePosition;
	Bool		GrabVirtualKeys;
	VirtualGridType	VirtualGrid;
	char		*VirtualGridColor;
	Bool		VirtualRaiseOnMove;
	Bool		VirtualRaiseVDM;
/*
 * Following are entries added with olvwm but which aren't strictly
 * dependent on the virtual nature of olvwm
 */
	Bool		AutoShowRootMenu;
	int		AutoRootMenuX;
	int		AutoRootMenuY;
	char		*InputFocusColor;
	Bool		FullSizeZoomX;
	List		*NoDecors;
	char		*ResizePosition;
	ImageType	UseImageMenu;
	Bool		VirtualMoveGroups;
	Bool		PointerWorkspace;
	char		*SaveWorkspaceCmd;
	SortType	VirtualMenuSort;
	SortType	VirtualDirSort;

     /* Special resize cursors */
	char            *SpecialResizePointerData;
	Bool            SpecialResizePointers;
	Cursor          CornerPointers[4];
	Cursor          ResizePointers[4];
     /* Other Cursors */
	Cursor          CloseUpPointer;
	Cursor          CloseDownPointer;
	char            *BasicPointerData;
	char            *MovePointerData;
	char            *BusyPointerData;
	char            *IconPointerData;
	char            *ResizePointerData;
	char            *MenuPointerData;
	char            *QuestionPointerData;
	char            *TargetPointerData;
	char            *PanPointerData;
	char            *CloseUpPointerData;
	char            *CloseDownPointerData;

	Bool		VirtualReRead;
	Bool		AllowSyntheticEvents;
	Bool		SyntheticEvents;
	List		*NoVirtualKey;
	List		*NoVirtualLKey;
	List		*NoVirtualFKey;
	List		*NoVirtualRKey;
} GlobalResourceVariables;

extern GlobalResourceVariables	GRV;
