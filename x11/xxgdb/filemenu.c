/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 * 
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/* filemenu.c
 *
 *  Construct a file menu (directory browser) which allows a user to go
 *  up and down the directory tree, to select text files to display, and
 *  to select executable files to debug.  The file menu is popped up by
 *  the 'file' command button.
 *  Duane Voth (duanev@mcc.com) contributed to the layout of the file menu, 
 *  plus some code and ideas.
 *
 *  changeDir():	Record the current working directory.
 *  InList():		Select files to be displayed in the menu.
 *  ScanDir():		Scan the directory and record selected filenames.
 *  DisplayMenuFile():	Callback for the file menu.
 *  CancelFileMenu():	Pop down the file menu.
 *  SetUpFileMenu():	Create the file menu popupshell.
 *  UpdateFileMenu():	Update entries in the file menu.
 *  File():		Command callback for the 'file' command button.
 */

#include <ctype.h>
#include <X11/Xos.h>
#include <sys/stat.h>

#ifdef SYSV 
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <dirent.h>
#endif

#ifdef SUNOS4
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include "global.h"

#define MAXCOLUMNS      8               /* max number of columns in file menu */
#define FILES_PER_COL   10              /* # of files per column in file menu */

static char	fileMenuDir[MAXPATHLEN];/* current directory of file menu */
static char  	**filelist; 		/* list of file names in fileMenu */
static int	nfiles = 0;		/* number of files in filelist */
static Widget	popupshell,		/* parent of popup */
		popup, 			/* vpane widget containing file menu */
		fileMenu, 		/* list widget as file menu */
		fileMenuLabel; 		/* label widget as file menu label */

void		File(), UpdateFileMenu();

/*  Change working directory to 'dir'.
 *  For Berkeley dbx, modify static global variable, cwd, to keep track of 
 *  current working directory.
 *  For Sun dbx, change working directory of dbx.
 */
static void changeDir(dir)
char *dir;
{
    char command[LINESIZ];
    char store[LINESIZ];
    int i,j;

    if(!strcmp(dir, "./")) return;
    
#if defined(BSD)
    if (dir[0] == '/' || dir[0] == '~') 
	strcpy(cwd, dir);
    if (strcmp(dir, "../") == NULL) {
	for (i=strlen(cwd); cwd[i] != '/' && i > 0; i--);
	cwd[i] = '\0';
	if (strcmp(cwd, "") == NULL)
	    strcpy(cwd, "/");
    }
    else {
	sprintf(cwd, "%s/%s", cwd, dir);
	LASTCH(cwd) = '\0';
    }
#else	/* not BSD */
    if(!strcmp(dir,"../"))
       {
	 for(i=0,j=0; cwd[i]; i++) if(cwd[i]=='/')j++;
	 if( j == 1 ) strcpy(store,"/");
	 else
	   strcpy(store,"..");
       }
    else
      {
	if(!strcmp(cwd, "/"))cwd[0]='\0';
	sprintf(store,"%s/%s", cwd, dir);
	LASTCH(store)='\0';
      }
    sprintf(command, "cd %s\n", store);
#ifdef GDB
   /* because silly gdb 4.0 displays nothing with cd command when
      confirm is on (possibly a gdb bug) , I just reset confirm to on
      just for this command !. */
   
   if (new_gdb4()) 
     query_gdb("set confirm on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
   
    query_gdb(command, PARSE_ON | ECHO_OFF | FILTER_OFF);
    
   if (new_gdb4()) /* reset confirm to off */
     query_gdb("set confirm off\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
#else
    query_dbx(command);
#endif	/* not GDB */
#endif	/* BSD */
}


/*  Determines if a directory entry should appear in the file menu. 
 *  The files included in the menu are :
 *    ..  (parent directory)
 *    directories
 *    text files 
 *    executable files
 */
#ifndef SYSV
static int InList(entry)
Directory *entry;
{
    char pathname[LINESIZ];
    struct stat statbuf;

    if (strcmp(entry->d_name, ".") == NULL || 	/* ignore current directory */
	LASTCH(entry->d_name) == '~' ||		/* ignore Emacs backup files */
	(LASTCH(entry->d_name) == 'o' && SECLASTCH(entry->d_name) == '.'))
						/* ignore object files */
	return False;
    if (entry->d_name[0] == '.' && entry->d_name[1] != '.')
	return False;				/* ignore hidden files */
    if (strcmp(cwd, "")) 			/* give full path name */
    	sprintf(pathname, "%s/%s", cwd, entry->d_name);
    else
    	strcpy(pathname, entry->d_name);
    if (stat(pathname, &statbuf) == -1) 
	return False;
    if (statbuf.st_mode & S_IFDIR) {		/* is directory */
	strcat(entry->d_name, "/");
	++(entry->d_namlen);
	return True;
    }
    if (statbuf.st_mode & S_IEXEC) {		/* is executable */
	strcat(entry->d_name, "*");
	++(entry->d_namlen);
	return True;
    }
    return True;
}
#endif /* not SYSV */


/*  Scans the working directory for files selected by InList(), sorted
 *  alphabetically, and stored in an array of pointers to directory
 *  entries called namelist.
 *  The names of the selected files are stored in filelist.
 */
static void ScanDir(dir)
char *dir;
{
#ifndef SYSV 
    extern 	alphasort();
    Directory   **namelist;
#else
    struct dirent *WorkingDirEntry;
    DIR *WorkingDir;
    char store[LINESIZ];
#endif
    register int		i,j;

#ifdef SYSV 
    if(!(WorkingDir = opendir(dir)))
      {
	UpdateMessageWindow("scandir: cannot open %s", dir);
	return;
      }
    nfiles=0;
    while(readdir(WorkingDir))nfiles++;
    rewinddir(WorkingDir);
#else    
    nfiles = scandir(dir, &namelist, InList, alphasort);
    if (nfiles == -1) {
	UpdateMessageWindow("scandir: cannot open %s", dir);
	return;
    }
#endif
    if (filelist) {
	for (i=0; filelist[i]; i++)
	    XtFree(filelist[i]);
	XtFree(filelist);
    }
    filelist = (char **) XtMalloc((nfiles+1) * sizeof(char *));
    i = 0;
    for (j=0; j<nfiles; j++) {
#ifdef SYSV 
      WorkingDirEntry = readdir(WorkingDir);
      if(!strcmp(WorkingDirEntry->d_name, "."))
	  strcpy(store, "./");
      else
	{
	  if(!strcmp(WorkingDirEntry->d_name, ".."))
	    strcpy(store, "../");
	  else
	    {
	      struct stat statbuf;
	      
	      sprintf(store,"%s/%s",cwd,WorkingDirEntry->d_name);
	      if(stat(store, &statbuf) == -1)
		store[0]='\0';
	      else
		{
		  if (statbuf.st_mode & S_IFDIR)
		    sprintf(store, "%s/", WorkingDirEntry->d_name);
		  else
		    if (statbuf.st_mode & S_IEXEC)
		      sprintf(store, "%s*", WorkingDirEntry->d_name);
		    else
		      if(LASTCH(WorkingDirEntry->d_name) == '~' ||
			 LASTCH(WorkingDirEntry->d_name) == '#' ||
			 WorkingDirEntry->d_name[0] == '.' ||
			 (LASTCH(WorkingDirEntry->d_name) == 'o' && 
			  SECLASTCH(WorkingDirEntry->d_name) == '.'))
			store[0]='\0';
		      else
			strcpy(store, WorkingDirEntry->d_name);
		}
	    }
	}
	 if(store[0])    
	   filelist[i++] = XtNewString(store);
#else /* not SYSV */
      filelist[i++] = XtNewString(namelist[j]->d_name);
      XtFree(namelist[j]);
#endif
    }
    filelist[i++] = NULL;

#ifdef SYSV 
    closedir(WorkingDir);
#else
    XtFree(namelist);
#endif
    return;
}
    

/*  Callback for the fileMenu list widget.
 *  >  if the selected item is a directory, display contents of that directory.
 *  >  (Sun dbx only) if the selected item is an executable file, issue the 
 *     debug command.
 *  >  if the selected item is a readable file, display the file.
 */
/* ARGSUSED */
static void DisplayMenuFile(w, popupshell, call_data)
    Widget w;
    Widget popupshell;
    XawListReturnStruct *call_data;
{
    char string[LINESIZ], *filename, command[LINESIZ];

    XtPopdown(popupshell);
    filename = call_data->string;
    if (filename == NULL) return;
    if (LASTCH(filename) == '/') {
	changeDir(filename);
	XtDestroyWidget(popupshell);
	UpdateFileMenu();	/* create new menu */
	File();			/* pop it up */
    }
    else if (LASTCH(filename) == '*') {
    	UpdateMessageWindow("",NULL);
#ifdef GDB
	strcpy(string, filename);
	LASTCH(string) = '\0';

	/* for GDB 4.xx, we send the command : file */
	if (new_gdb4())
		sprintf(command, "file %s\n", string);
	else
		{
		/* for GDB 3.xx, we send the commands : exec-file & symbol-file */
		
		/* (PW)21DEC90 : this button is special because it has to send
		TWO commands to GDB. */
		
		sprintf(command, "exec-file %s\n", string);
		send_command(command);
	    	AppendDialogText(command);
	    	
		sprintf(command, "symbol-file %s\n", string);
		}
	send_command(command);
	AppendDialogText(command);
#else
#ifndef BSD
	strcpy(string, filename);
	LASTCH(string) = '\0';
	sprintf(command, "debug %s\n", string);
	send_command(command);
    	AppendDialogText(command);
#endif
#endif /* GDB */
    }
    else {
    	UpdateMessageWindow("",NULL);
#ifdef GDB
    if (strcmp(filename, "core") == NULL)
		sprintf(command, "core-file %s\n", filename);
    else
		sprintf(command, "list %s:1\n", filename);
	send_command(command);
    	AppendDialogText(command);
#else /* not GDB */
	sprintf(command, "file %s\n", filename);
	send_command(command);
    	AppendDialogText(command);
#endif /* GDB */
    }
}


/*  Callback to popdown the file menu
 */
/* ARGSUSED */
static void CancelFileMenu(w, popupshell, call_data)
    Widget w;
    Widget popupshell;
    caddr_t call_data;
{
    XtPopdown(popupshell);
    UpdateMessageWindow("",NULL);
}


/*  Creates a popup shell with its child being a vpane widget containing
 *  a file menu label, a file menu containing file names returned from 
 *  ScanDir(), and a cancel command button.
 *  When an item in the list is selected, DisplayMenuFile is called.
 */
static void SetUpFileMenu(dir) 
char *dir;
{
    Widget	cancelButton;
    Arg 	args[MAXARGS];
    Cardinal	n;
    char	menulabel[LINESIZ];
    int		ncolumns;

    n = 0;
    popupshell = XtCreatePopupShell("File Directory", transientShellWidgetClass,
				    toplevel, args, n);

    n = 0;
    popup = XtCreateManagedWidget("popup", panedWidgetClass, popupshell,
				  args, n);
    ScanDir(dir);
    strcpy(fileMenuDir, dir);

    n = 0;
    sprintf(menulabel, "   %s   ", dir);
    XtSetArg(args[n], XtNlabel, (XtArgVal) menulabel); 			n++;
    XtSetArg(args[n], XtNjustify, (XtArgVal) XtJustifyCenter);          n++;
    fileMenuLabel = XtCreateManagedWidget("fileMenuLabel", labelWidgetClass,
                                          popup, args, n);

    n = 0;
    ncolumns = nfiles/FILES_PER_COL + 1;
    ncolumns = MIN(ncolumns, MAXCOLUMNS);
    XtSetArg(args[n], XtNlist, filelist);				n++;
    XtSetArg(args[n], XtNverticalList, True);				n++;
    XtSetArg(args[n], XtNdefaultColumns, (XtArgVal) ncolumns);		n++;
    fileMenu = XtCreateManagedWidget("fileMenu", listWidgetClass, 
				     popup, args, n);
    XtAddCallback(fileMenu, XtNcallback, DisplayMenuFile, popupshell);

    n = 0;
    XtSetArg(args[n], XtNresize, False);				n++;
    XtSetArg(args[n], XtNlabel, "CANCEL");				n++;
    cancelButton = XtCreateManagedWidget("cancelButton", commandWidgetClass,
					 popup, args, n);
    XtAddCallback(cancelButton, XtNcallback, CancelFileMenu, popupshell);

    DisableWindowResize(fileMenuLabel);
    DisableWindowResize(cancelButton);
}


/*  This routine is called when there is a a change in current directory.
 *  It destroys the existing popup shell and creates a new file menu based
 *  on the new current directory.  A new directory list is created.
 */
static void UpdateFileMenu()
{
    SetUpFileMenu(cwd);
#ifdef GDB
    query_gdb_directories();  /* defined in gdb_handler.c */
#else
    query_dbx("use\n");
#endif /* GDB */
}


/*  File command button callback.
 */
/* ARGSUSED */
void File(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    Arg 	args[MAXARGS];
    Cardinal	n;
    Position	x, y, x_offset;
    Dimension	fileMenu_width, fileMenuLabel_width, border_width,
		width, dialog_width;

    XDefineCursor(display, XtWindow(toplevel), watch);
    XDefineCursor(display, XtWindow(sourceWindow), watch);
    XDefineCursor(display, XtWindow(dialogWindow), watch);
    XFlush(display);
    if (strcmp(fileMenuDir, cwd))
	UpdateFileMenu();

    n = 0;
    XtSetArg(args[n], XtNwidth, &fileMenu_width);			n++;
    XtSetArg(args[n], XtNborderWidth, &border_width);		n++;
    XtGetValues(fileMenu, args, n);

    n = 0;
    XtSetArg(args[n], XtNwidth, &fileMenuLabel_width);		n++;
    XtGetValues(fileMenuLabel, args, n);

    n = 0;
    XtSetArg(args[n], XtNwidth, &dialog_width);			n++;
    XtGetValues(dialogWindow, args, n);

    width = MAX(fileMenu_width, fileMenuLabel_width);
    x_offset = (Position) (dialog_width - width - border_width);
    XtTranslateCoords(dialogWindow, x_offset, 0, &x, &y);

    x = MAX(0, x);
    y = MAX(0, y);

    n = 0;
    XtSetArg(args[n], XtNx, x);					n++;
    XtSetArg(args[n], XtNy, y);					n++;
    XtSetValues(popupshell, args, n);
    XtPopup(popupshell, XtGrabNonexclusive);

    UpdateMessageWindow("Select a file or directory",NULL);
    
    XUndefineCursor(display, XtWindow(toplevel));
    XUndefineCursor(display, XtWindow(sourceWindow));
    XUndefineCursor(display, XtWindow(dialogWindow));
}
