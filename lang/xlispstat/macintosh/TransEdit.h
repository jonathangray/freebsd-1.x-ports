/*
	TransEdit.h - TransEdit header file
*/

#ifndef MPWC
# ifndef	_WindowMgr_
# include	"WindowMgr.h"
# endif

# ifndef	_TextEdit_
# include	"TextEdit.h"
# endif
#endif MPWC

# ifndef	nil
# define	nil		(0L)
# endif


WindowPtr	NewEWindow ();
WindowPtr	GetNewEWindow ();
TEHandle	GetEWindowTE ();
Boolean		GetEWindowFile ();
Boolean		IsEWindow ();
Boolean		IsEWindowDirty ();
Boolean		EWindowSave ();
Boolean		EWindowSaveAs ();
Boolean		EWindowSaveCopy ();
Boolean		EWindowClose ();
Boolean		EWindowRevert ();
Boolean		ClobberEWindows ();
int			EWindowEditOp ();