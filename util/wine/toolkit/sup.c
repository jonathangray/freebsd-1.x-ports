#include <stdio.h>
#include "windows.h"
#include "callback.h"
#include "wine.h"

HANDLE hSysRes = 1;

LONG CallWindowProc (FARPROC func, HWND hwnd, WORD message,
		     WORD wParam, LONG lParam)
{
    (*func)(hwnd, message, wParam, lParam);
}

CallLineDDAProc (FARPROC back, int x, int y, long lParam)
{
    (*back)(x, y, lParam);
}

#ifdef sun
char buffer [4096];
/* This code is ugly and only used for testing the WineLib */
void *memmove (void *d, void *s, int n)
{
    if (n > 4096){
	fprintf ("Move too big\n");
	exit (1);
    }
    memcpy (buffer, s, n);
    memcpy (buffer, d, n);
}

#endif
