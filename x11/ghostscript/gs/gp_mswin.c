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

/* gp_mswin.c */
/*
 * Microsoft Windows 3.n platform support for Ghostscript.
 * Original version by Russell Lang and Maurice Castro with help from
 * Programming Windows, 2nd Ed., Charles Petzold, Microsoft Press;
 * initially created from gp_dosfb.c and gp_itbc.c 5th June 1992.
 */

#include "windows_.h"
#if WINVER >= 0x030a
#include <shellapi.h>
#endif
#include "stdio_.h"
#include <stdlib.h>
#include <stdarg.h>
#include "ctype_.h"
#include "dos_.h"
#include <io.h>
#include "malloc_.h"
#include "gp_mswin.h"
#include "gp_mswtx.h"

#include "memory_.h"
#include "gx.h"
#include "gp.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxdevice.h"

#include <fcntl.h>
#include <signal.h>
#include "string_.h"

/* for imitation pipes */
#include "stream.h"
#include "filedev.h"			/* must come after stream.h */
extern stream *gs_stream_stdin;		/* from zfiledev.c */
extern stream *gs_stream_stdout;	/* from zfiledev.c */
extern stream *gs_stream_stderr;	/* from zfiledev.c */

/* Library routines not declared in a standard header */
extern char *getenv(P1(const char *));

/* Imported from gsmain.c */
extern void gs_exit(P1(int exit_status));

/* Imported from gp_msdos.c */
int gp_file_is_console(P1(FILE *));

/* ------ from gnuplot winmain.c plus new stuff ------ */

/* limits */
#define MAXSTR 255

/* public handles */
HWND hwndtext;
HINSTANCE phInstance;

const LPSTR szAppName = "Ghostscript";
const LPSTR szImgName = "Ghostscript Image";
char FAR win_prntmp[MAXSTR];	/* filename of PRN temporary file */
int win_init = 0;		/* flag to know if gp_exit has been called */
int win_exit_status;
BOOL is_win31 = FALSE;

/* gsview.exe */
BOOL gsview = FALSE;
HWND gsview_hwnd = NULL;
BOOL gsview_next = FALSE;
LPSTR gsview_option = "-sGSVIEW=";

/* redirected stdio */
TW textwin;	/* text window structure */

/* imitation pipes */
HGLOBAL pipe_hglobal = NULL;
LPBYTE pipe_lpbyte = NULL;
UINT pipe_count = 0;

BOOL CALLBACK _export AbortProc(HDC, int);
int main(int argc, char *argv[], char *env[]);

/* our exit handler */
/* also called from Text Window WM_CLOSE */
void win_exit(void)
{
#if WINVER >= 0x030a
	/* disable Drag Drop */
	if (is_win31)
		DragAcceptFiles(hwndtext, FALSE);
#endif
	/* if we didn't exit through gs_exit() then do so now */
	if (win_init)
		gs_exit(0);

	fcloseall();
	if (win_exit_status) {
		/* display message box so error messages in hwndtext can be read */
		char buf[20];
		sprintf(buf, "Exit code %d",win_exit_status);
		MessageBox((HWND)NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
	}
	
	/* tell gsview that we are closing */
	if (gsview)
		SendMessage(gsview_hwnd, WM_GSVIEW, GSWIN_CLOSE, (LPARAM)NULL);

	TextClose(&textwin);
}

int PASCAL 
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int cmdShow)
{
	int i;
#if defined(_MSC_VER)    /* MSC doesn't give us _argc and _argv[] so ...   */
#define MAXCMDTOKENS 128
	int     _argc=0;
	LPSTR   _argv[MAXCMDTOKENS];
	_argv[_argc] = "gswin.exe";
	_argv[++_argc] = _fstrtok( lpszCmdLine, " ");
	while (_argv[_argc] != NULL)
		_argv[++_argc] = _fstrtok( NULL, " ");
#endif
	is_win31 = FALSE;
	{
	WORD version = LOWORD(GetVersion());
	if (((LOBYTE(version)<<8) | HIBYTE(version)) >= 0x30a)
		is_win31 = TRUE;
	}

	if (hPrevInstance) {
		MessageBox((HWND)NULL,"Can't run twice", szAppName, MB_ICONHAND | MB_OK);
		return FALSE;
	}

	/* copy the hInstance into a variable so it can be used */
	phInstance = hInstance;

        /* start up the text window */
	textwin.hInstance = hInstance;
	textwin.hPrevInstance = hPrevInstance;
	textwin.nCmdShow = cmdShow;
	textwin.Title = szAppName;
	textwin.hIcon = LoadIcon(hInstance, "texticon");
	textwin.DragPre = "(";
	textwin.DragPost = ") run\r";
	textwin.ScreenSize.x = 80;
	textwin.ScreenSize.y = 80;
	textwin.KeyBufSize = 2048;
	textwin.CursorFlag = 1;	/* scroll to cursor after \n & \r */
	textwin.shutdown = win_exit;

	if (TextInit(&textwin))
		exit(1);
	hwndtext = textwin.hWndText;

	(void) atexit((atexit_t)win_exit); /* setup exit handler */

	/* check if we are to use gsview.exe */
	for (i=0; i<_argc; i++) {
	    if (!strncmp(_argv[i], gsview_option, strlen(gsview_option))) {
		gsview_hwnd = (HWND)atoi(_argv[i]+strlen(gsview_option));
		if (gsview_hwnd != (HWND)NULL) {
			if (!IsWindow(gsview_hwnd)) {
				char buf[80];
				sprintf(buf,"GSVIEW window handle %u is invalid",(int)gsview_hwnd);
				MessageBox(hwndtext, buf, szAppName, MB_OK);
				return 0;
			}
			gsview = TRUE;
			/* give gsview the handle to our text window */
			SendMessage(gsview_hwnd, WM_GSVIEW, HWND_TEXT, (LPARAM)hwndtext);
		}
	    }
	}

	main(_argc, _argv, environ);

	/* never reached */
	win_exit(); 
	return 0;
}


BOOL CALLBACK _export
AbortProc(HDC hdcPrn, int code)
{
    (void)gp_check_interrupts();
    return(TRUE);
}
  
/* ------ Process message loop ------ */
/*
 * Check messages and interrupts; return true if interrupted.
 * This is called frequently - it must be quick!
 */
int
gp_check_interrupts(void)
{
	TextMessage();
	return 0;
}

/* ====== Generic platform procedures ====== */

/* ------ Initialization/termination (from gp_itbc.c) ------ */

/* Do platform-dependent initialization. */
void
gp_init(void)
{
	win_init = 1;
}

/* Do platform-dependent cleanup. */
void
gp_exit(int exit_status, int code)
{
	win_init = 0;
	win_exit_status = exit_status;
}

/* ------ Printer accessing ------ */
  
/* Forward references */
private int gp_printfile(P1(char *));

/* Open a connection to a printer.  A null file name means use the */
/* standard printer connected to the machine, if any. */
/* Return NULL if the connection could not be opened. */
FILE *
gp_open_printer(char *fname, int binary_mode)
{	if ( strlen(fname) == 0 || !strcmp(fname, "PRN") )
	{	FILE *pfile;
		pfile = gp_open_scratch_file(gp_scratch_file_name_prefix, 
			win_prntmp, "wb");
		return pfile;
	}
	else
		return fopen(fname, (binary_mode ? "wb" : "w"));
}

/* Close the connection to the printer. */
void
gp_close_printer(FILE *pfile, const char *fname)
{
	fclose(pfile);
	if (strlen(fname) && strcmp(fname,"PRN"))
	    return;		/* a file, not a printer */

	gp_printfile(win_prntmp);
	unlink(win_prntmp);
}

/* Windows does not provide API's in the SDK for writing directly to a */
/* printer.  Instead you are supposed to use the Windows printer drivers. */
/* Ghostscript has its own printer drivers, so we need to use some API's */
/* that are documented only in the Device Driver Adaptation Guide */
/* that comes with the DDK.  Prototypes taken from DDK <print.h> */
DECLARE_HANDLE(HPJOB);

HPJOB   WINAPI OpenJob(LPSTR, LPSTR, HPJOB);
int     WINAPI StartSpoolPage(HPJOB);
int     WINAPI EndSpoolPage(HPJOB);
int     WINAPI WriteSpool(HPJOB, LPSTR, int);
int     WINAPI CloseJob(HPJOB);
int     WINAPI DeleteJob(HPJOB, int);
int     WINAPI WriteDialog(HPJOB, LPSTR, int);
int     WINAPI DeleteSpoolPage(HPJOB);

HWND hDlgModeless;

/* Modeless dialog box - Cancel printing */
BOOL CALLBACK _export
CancelDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch(message) {
	case WM_INITDIALOG:
	    SetWindowText(hDlg, szAppName);
	    return TRUE;
	case WM_COMMAND:
	    switch(LOWORD(wParam)) {
		case IDCANCEL:
		    DestroyWindow(hDlg);
		    hDlgModeless = 0;
		    EndDialog(hDlg, 0);
		    return TRUE;
	    }
    }
    return FALSE;
}

/* Dialog box to select printer port */
BOOL CALLBACK _export
SpoolDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
LPSTR entry;
    switch(message) {
	case WM_INITDIALOG:
	    entry = (LPSTR)lParam;
	    while (*entry) {
		SendDlgItemMessage(hDlg, SPOOL_PORT, LB_ADDSTRING, 0, (LPARAM)entry);
		entry += lstrlen(entry)+1;
	    }
	    SendDlgItemMessage(hDlg, SPOOL_PORT, LB_SETCURSEL, 0, (LPARAM)0);
	    return TRUE;
	case WM_COMMAND:
	    switch(LOWORD(wParam)) {
		case SPOOL_PORT:
#ifdef WIN32
		    if (HIWORD(wParam)
#else
		    if (HIWORD(lParam)
#endif
			               == LBN_DBLCLK)
			PostMessage(hDlg, WM_COMMAND, IDOK, 0L);
		    return FALSE;
		case IDOK:
		    EndDialog(hDlg, 1+(int)SendDlgItemMessage(hDlg, SPOOL_PORT, LB_GETCURSEL, 0, 0L));
		    return TRUE;
		case IDCANCEL:
		    EndDialog(hDlg, 0);
		    return TRUE;
	    }
    }
    return FALSE;
}

/* Print File to port */
private int
gp_printfile(char *filename)
{
#define PRINT_BUF_SIZE 16384u
char *buffer;
char *portname;
DLGPROC lpfnSpoolProc;
int i, port;
HPJOB hJob;
WORD count;
FILE *f;
int error = FALSE;
DLGPROC lpfnCancelProc;
long lsize;
long ldone;
char pcdone[20];
MSG msg;

	/* get list of ports */
	if ((buffer = malloc(PRINT_BUF_SIZE)) == (char *)NULL)
	    return FALSE;
	GetProfileString("ports", NULL, "", buffer, PRINT_BUF_SIZE);
	/* select a port */
	lpfnSpoolProc = (DLGPROC)MakeProcInstance((FARPROC)SpoolDlgProc, phInstance);
	port = DialogBoxParam(phInstance, "SpoolDlgBox", hwndtext, lpfnSpoolProc, (LPARAM)buffer);
	FreeProcInstance((FARPROC)lpfnSpoolProc);
	if (!port) {
	    free(buffer);
	    return FALSE;
	}
	portname = buffer;
	for (i=1; i<port && strlen(portname)!=0; i++)
	    portname += lstrlen(portname)+1;
	
	if ((f = fopen(filename, "rb")) == (FILE *)NULL) {
	    free(buffer);
	    return FALSE;
	}
	fseek(f, 0L, SEEK_END);
	lsize = ftell(f);
	if (lsize <= 0)
	    lsize = 1;
	fseek(f, 0L, SEEK_SET);

	hJob = OpenJob(portname, filename, (HDC)NULL);
	switch ((int)hJob) {
	    case SP_APPABORT:
	    case SP_ERROR:
	    case SP_OUTOFDISK:
	    case SP_OUTOFMEMORY:
	    case SP_USERABORT:
	        fclose(f);
		free(buffer);
	        return FALSE;
	}
	if (StartSpoolPage(hJob) < 0)
	    error = TRUE;

	lpfnCancelProc = (DLGPROC)MakeProcInstance((FARPROC)CancelDlgProc, phInstance);
	hDlgModeless = CreateDialog(phInstance, "CancelDlgBox", hwndtext, lpfnCancelProc);
	ldone = 0;

	while (!error && hDlgModeless 
	  && (count = fread(buffer, 1, PRINT_BUF_SIZE, f)) != 0 ) {
	    if (WriteSpool(hJob, buffer, count) < 0)
		error = TRUE;
	    ldone += count;
	    sprintf(pcdone, "%d %%done", (int)(ldone * 100 / lsize));
	    SetWindowText(GetDlgItem(hDlgModeless, CANCEL_PCDONE), pcdone);
	    while (PeekMessage(&msg, hDlgModeless, 0, 0, PM_REMOVE)) {
	        if ((hDlgModeless == 0) || !IsDialogMessage(hDlgModeless, &msg)) {
		    TranslateMessage(&msg);
		    DispatchMessage(&msg);
  		}
  	    }
  	}
	free(buffer);
	fclose(f);

	if (!hDlgModeless)
	    error=TRUE;
	DestroyWindow(hDlgModeless);
	hDlgModeless = 0;
	FreeProcInstance((FARPROC)lpfnCancelProc);
	EndSpoolPage(hJob);
	if (error)
	    DeleteJob(hJob, 0);
	else
	    CloseJob(hJob);
	return !error;
}

/* ------ File names ------ */

/* Create and open a scratch file with a given name prefix. */
/* Write the actual file name at fname. */
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{	char *temp;
	if ( (temp = getenv("TEMP")) == NULL )
		*fname = 0;
	else
	{	strcpy(fname, temp);
		/* Prevent X's in path from being converted by mktemp. */
		for ( temp = fname; *temp; temp++ )
			*temp = tolower(*temp);
		if ( strlen(fname) && (fname[strlen(fname)-1] != '\\') )
			strcat(fname, "\\");
	}
	strcat(fname, prefix);
	strcat(fname, "XXXXXX");
	mktemp(fname);
	return fopen(fname, mode);
}

/* ------ File operations ------ */

/* If the file given by fname exists, fill in its status and return 1; */
/* otherwise return 0. */
int
gp_file_status(const char *fname, file_status *pstatus)
{	FILE *f = fopen(fname, "r");
	long flen;
	struct ftime ft;
	if ( f == NULL ) return 0;
	if ( getftime(fileno(f), &ft) < 0 )
	   {	fclose(f);
		return 0;
	   }
	fseek(f, 0, SEEK_END);
	flen = ftell(f);
	pstatus->size_pages = (flen + 1023) >> 10;
	pstatus->size_bytes = flen;
	/* Make a single long value from the ftime structure. */
	pstatus->time_referenced = pstatus->time_created =
	  ((long)((ft.ft_year << 9) + (ft.ft_month << 5) + ft.ft_day) << 16) +
	  ((ft.ft_hour << 11) + (ft.ft_min << 5) + ft.ft_tsec);
	fclose(f);
	return 1;
}

/* ====== Substitute for stdio ====== */

/* Forward references */
private void win_std_init(void);
private void win_pipe_init(void);
private int win_std_read_buf(P1(stream *s));
private int win_std_write_buf(P1(stream *s));

/* Use a pseudo file device to get win_stdio_init called at the right time. */
/* This is bad architecture; we'll fix it later. */
private fdev_proc_init(win_stdio_init);
const file_device gs_fdev_wstdio = {
	"wstdio",
	{ win_stdio_init, fdev_no_open_device,
	  fdev_no_open_file, fdev_no_fopen, fdev_no_fclose,
	  fdev_no_delete_file, fdev_no_rename_file,
	  fdev_no_enumerate_files
	}
};

/* Do one-time initialization */
private int
win_stdio_init(file_device *fdev)
{
	win_std_init();		/* redefine stdin/out/err to our window routines */
	if (gsview)
	    win_pipe_init();	/* redefine stdin to be a pipe */
	return 0;
}

/* reinitialize stdin/out/err to use our windows */
/* assume stream has already been initialized for the real stdin */
private void
win_std_init(void)
{
	if ( gp_file_is_console(gs_stream_stdin->file) )
	{	/* Allocate a real buffer for stdin. */
		/* The size must not exceed the size of the */
		/* lineedit buffer.  (This is a hack.) */
#define win_stdin_buf_size 160
		static const stream_procs pin =
		{	s_std_noavailable, s_std_noseek,
			s_std_read_flush, s_std_close,
			win_std_read_buf, NULL
		};
		byte *buf = (byte *)gs_malloc(win_stdin_buf_size, 1,
					      "win_stdin_init");
		s_std_init(gs_stream_stdin, buf, win_stdin_buf_size,
			   &pin, s_mode_read);
		gs_stream_stdin->file = NULL;
	}

	{	static const stream_procs pout =
		{	s_std_noavailable, s_std_noseek,
			s_std_write_flush, s_std_close,
			NULL, win_std_write_buf 
		};
		if ( gp_file_is_console(gs_stream_stdout->file) )
		{	gs_stream_stdout->procs = pout;
			gs_stream_stdout->file = NULL;
		}
		if ( gp_file_is_console(gs_stream_stderr->file) )
		{	gs_stream_stderr->procs = pout;
			gs_stream_stderr->file = NULL;
		}
	}

}

private int
win_std_read_buf(register stream *s)
{
	byte *buf = s->cbuf;
	/* Implement line editing here. */
#define ERASELINE 21		/* ^U */
#define ERASECHAR1 8		/* ^H */
#define ERASECHAR2 127		/* DEL */
	byte *dest = buf - 1;
	byte *limit = dest + s->bsize - 1;  /* always leave room for \n */
	uint ch;
	while ( (ch = TextGetCh(&textwin)) != '\n' )
	{	switch ( ch )
		{
		default:
			if ( dest == limit )
				MessageBeep(-1);
			else
			{	*++dest = ch;
				TextPutCh(&textwin, ch);
			}
			break;
		case ERASELINE:
			while ( dest >= buf )
			{	TextPutCh(&textwin, '\b');
				TextPutCh(&textwin, ' ');
				TextPutCh(&textwin, '\b');
				dest--;
			}
			break;
		case ERASECHAR1:
		case ERASECHAR2:
			if ( dest >= buf )
			{	TextPutCh(&textwin, '\b');
				TextPutCh(&textwin, ' ');
				TextPutCh(&textwin, '\b');
				dest--;
			}
			break;
		}
	}
	*++dest = ch;
	TextPutCh(&textwin, ch);
	s->cptr = buf - 1;
	s->end_status = 0;
	s->endptr = dest;
	return 0;
}

private int
win_std_write_buf(register stream *s)
{	uint count = s->cptr + 1 - s->cbuf;
	TextWriteBuf(&textwin, s->cbuf, count);
	s->cptr = s->cbuf - 1;
	s->endptr = s->cptr + s->bsize;
	gp_check_interrupts();
	return 0;
}

/* This is used instead of the stdio version. */
/* The declaration must be identical to that in <stdio.h>. */
int _Cdecl _FARFUNC
fprintf(FILE _FAR *file, const char *fmt, ...)
{
int count;
va_list args;
	va_start(args,fmt);
	if ( gp_file_is_console(file) ) {
		char buf[1024];
		count = vsprintf(buf,fmt,args);
/*		MessageBox((HWND)NULL, buf, szAppName, MB_OK | MB_ICONSTOP); */
		TextWriteBuf(&textwin, buf, count);
	}
	else
		count = vfprintf(file, fmt, args);
	va_end(args);
	return count;
}

/* ------ Imitation pipes (only used with gsview) ------ */

/* Forward references */
private int win_pipe_read_buf(P1(stream *));
private void win_pipe_request(void);
private int win_pipe_read(char *, unsigned int);

/* reinitialize stdin stream to read from imitation pipe */
/* assume stream has already been initialized for the windows stdin */
private void
win_pipe_init(void)
{	if ( gs_stream_stdin->procs.read_buf == win_std_read_buf )
	{	static const stream_procs p =
		{	s_std_noavailable, s_std_noseek,
			s_std_read_flush, s_std_close,
			win_pipe_read_buf, NULL
		};
		gs_stream_stdin->procs = p;
	}
}

private int
win_pipe_read_buf(register stream *s)
{	int nread;
	nread = win_pipe_read(s->cbuf, 1);
	s->cptr = s->cbuf - 1;
	s->end_status = (nread <= 0 ? EOFC : 0);
	if ( nread <= 0 ) nread = 0;
	s->endptr = s->cptr + nread;
	return 0;
}

/* get a block from gsview */
private void
win_pipe_request(void)
{
	/* request another block */
	if (pipe_lpbyte != (LPBYTE)NULL)
	        GlobalUnlock(pipe_hglobal);
	if (pipe_hglobal != (HGLOBAL)NULL)
	        GlobalFree(pipe_hglobal);
	pipe_hglobal = (HGLOBAL)NULL;
	pipe_lpbyte = (LPBYTE)NULL;

	SendMessage(gsview_hwnd, WM_GSVIEW, PIPE_REQUEST, 0L);
	/* wait for block */
	while (pipe_hglobal == (HGLOBAL)NULL)
	    gp_check_interrupts();
	pipe_lpbyte = GlobalLock(pipe_hglobal);
}

/* pipe_read - similar to fread */
private int
win_pipe_read(char *ptr, unsigned int count)
{
	int n;
	if (!pipe_count)
	    win_pipe_request();
	if (!pipe_count)
	    return -1;
	n = min(count, pipe_count);
	memcpy(ptr, pipe_lpbyte, n);
	pipe_lpbyte += n;
	pipe_count -= n;
	return n;
}
