/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gp_mswin.h */
/* Definitions common to Ghostscript MS Windows implementation */
/* (used by both C code and Ghostscript 'resource') */
  
#define SPOOL_PORT	100
#define CANCEL_PCDONE	101

#ifndef RC_INVOKED		/* NOTA BENE */

/* system menu constants for image window */
#define M_COPY_CLIP 1

/* externals from gp_mswin.c */
extern HWND hwndtext;
extern HINSTANCE phInstance;
extern const LPSTR szAppName;
extern const LPSTR szImgName;
extern BOOL is_win31;
extern BOOL CALLBACK _export AbortProc(HDC, int);
/* imitation pipes */
extern HGLOBAL pipe_hglobal;
extern LPBYTE pipe_lpbyte;
extern UINT pipe_count;

/* for gsview.exe */
extern BOOL gsview;
extern HWND gsview_hwnd;
extern BOOL gsview_next;
extern LPSTR gsview_option;
/* messages used between gsview and gswin */
#define WM_GSVIEW WM_USER+0
/* from gswin to gsview */
#define HWND_TEXT	0
#define HWND_IMGCHILD	1
#define GSWIN_CLOSE	2
#define SYNC_OUTPUT	3
#define OUTPUT_PAGE	4
#define SCROLL_POSITION 5
#define PIPE_REQUEST	6
/* from gsview to gswin image window */
#define NEXT_PAGE	10
#define COPY_CLIPBOARD	11
/* from gsview to gswin text window */
#define PIPE_DATA	12

#endif				/* !defined(RC_INVOKED) */
