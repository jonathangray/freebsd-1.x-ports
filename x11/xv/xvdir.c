/* 
 * xvdir.c - Directory changin', file i/o dialog box
 *
 * callable functions:
 *
 *   CreateDirW(geom,bwidth)-  creates the dirW window.  Doesn't map it.
 *   DirBox(vis)            -  random processing based on value of 'vis'
 *                             maps/unmaps window, etc.
 *   ClickDirW()            -  handles mouse clicks in DirW
 *   LoadCurrentDirectory() -  loads up current dir information for dirW
 *   GetDirPath()           -  returns path that 'dirW' is looking at
 *   DoSave()               -  calls appropriate save routines
 *   SetDirFName()          -  sets the 'load/save-as' filename and default
 *   GetDirFName()          -  gets the 'load/save-as' filename (no path)
 *   SetDirRButt()          -  sets format/color/size rbutts 
 *
 *   InitPoll()             -  called whenever a file is first loaded
 *   CheckPoll(int)         -  checks to see whether we should reload
 */

/* Copyright Notice
 * ================
 * Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
 * 
 * Permission to use, copy, and distribute XV in its entirety, for 
 * non-commercial purposes, is hereby granted without fee, provided that
 * this license information and copyright notice appear in all copies.
 * 
 * Note that distributing XV 'bundled' in with ANY product is considered
 * to be a 'commercial purpose'.
 *
 * Also note that any copies of XV that are distributed MUST be built
 * and/or configured to be in their 'unregistered copy' mode, so that it
 * is made obvious to the user that XV is shareware, and that they should
 * consider donating, or at least reading this License Info.
 * 
 * The software may be modified for your own purposes, but modified
 * versions may NOT be distributed without prior consent of the author.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author be held liable for any damages
 * arising from the use of this software.
 * 
 * If you would like to do something with XV that this copyright
 * prohibits (such as distributing it with a commercial product, 
 * using portions of the source in some other program, etc.), please
 * contact the author (preferably via email).  Arrangements can
 * probably be worked out.
 *
 * XV is shareware for PERSONAL USE only.  You may use XV for your own
 * amusement, and if you find it nifty, useful, generally cool, or of
 * some value to you, your non-deductable donation would be greatly
 * appreciated.  $25 is the suggested donation, though, of course,
 * larger donations are quite welcome.  Folks who donate $25 or more
 * can receive a Real Nice bound copy of the XV manual for no extra
 * charge.
 * 
 * Commercial, government, and institutional users MUST register their
 * copies of XV, for the exceedingly REASONABLE price of just $25 per
 * workstation/X terminal.  Site licenses are available for those who
 * wish to run XV on a large number of machines.  Contact the author
 * for more details.
 *
 * The author may be contacted via:
 *    US Mail:  John Bradley
 *              1053 Floyd Terrace
 *              Bryn Mawr, PA  19010
 *
 *    Phone:    (215) 898-8813
 *    EMail:    bradley@cis.upenn.edu
 */


#define NEEDSDIR
#include "xv.h"

#ifndef VMS
#include <pwd.h>       /* for getpwnam() prototype and passwd struct */
#endif


#define NLINES 9                   /* # of lines in list control (keep odd) */
#define LISTW  237

#define DIRWIDE  350               /* (fixed) size of directory window */
#define DIRHIGH  460

#define BUTTW   60
#define BUTTH   24
#define DDWIDE  (LISTW-80+15)
#define DNAMWIDE 252

#define MAXDEEP 30    /* maximum number of directories in cwd path */
#define MAXFNLEN 256   /* max length of filename being entered */


/* convex c 4.x is fanatically ANSI, and the OS is increasingly fanatically
   POSIX.  The compiler won't define __STDC__ in extended (default) mode,
   and the various include files won't do some things if __STDC__ is
   defined, and won't do others if it isn't.  The compiler people say that
   the use of __STDC__ to detect an ANSI compiler is wrong, and that everyone
   should use __stdc__.  I typically just add -D__STDC__ to the commandline
   and work around any problems it causes.  Here, these don't get defined
   if __STDC__ is defined, because the include files assume that if it is,
   the compiler is being strict.  Sigh   

   Anthony A. Datri  <datri@concave.convex.com> */



#ifdef __STDC__
static void RedrawDList(int, SCRL *);
static void changedDirMB(int);
static int  dnamcmp(char **, char **);
static int  FNameCdable(void);
static void loadCWD(void);
static int  cd_able(char *);
static void scrollToFileName(void);
static void setFName(char *);
static void showFName(void);
static void changeSuffix(void);
#else
static void RedrawDList();
static void changedDirMB();
static int  dnamcmp();
static int  FNameCdable();
static void loadCWD();
static int  cd_able();
static void scrollToFileName();
static void setFName();
static void showFName();
static void changeSuffix();
#endif


static char  *fnames[MAXNAMES];
static int    numfnames = 0, ndirs = 0;
static char   path[MAXPATHLEN+1];       /* '/' terminated */
static char  *dirs[MAXDEEP];            /* list of directory names */
static char  *dirMBlist[MAXDEEP];       /* list of dir names in right order */
static char  *lastdir;                  /* name of the directory we're in */
static char   filename[MAXFNLEN+100];   /* filename being entered */
static char   deffname[MAXFNLEN+100];   /* default filename */
static char   globname[MAXFNLEN+100];   /* the +100 is for ~ expansion */

static RBUTT *formatRB, *colorRB;
static int    savemode;                 /* if 0 'load box', if 1 'save box' */
static int    curPos, stPos, enPos;     /* filename textedit stuff */
static MBUTT  dirMB;                    /* popup path menu */

static int haveoldinfo = 0;
static int oldformat, oldcolors;
static char oldfname[MAXFNLEN+100];

/* the name of the file actually opened.  (the temp file if we are piping) */
static char outFName[256];  
static int  dopipe;


/***************************************************/
void CreateDirW(geom)
char *geom;
{
  int y;

  path[0] = '\0';

  dirW = CreateWindow("","XVdir", geom, DIRWIDE, DIRHIGH, infofg, infobg, 0);
  if (!dirW) FatalError("couldn't create 'directory' window!");

  LSCreate(&dList, dirW, 10, 12+LINEHIGH, LISTW, LINEHIGH*NLINES, NLINES,
	   fnames, numfnames, infofg, infobg, hicol, locol,
	   RedrawDList, 1, 0);

  dnamW = XCreateSimpleWindow(theDisp, dirW, 80, 
			      dList.h + 70 - ASCENT - 4, 
			      DNAMWIDE+6, LINEHIGH+5, 1, infofg, infobg);
  if (!dnamW) FatalError("can't create name window");
  XSelectInput(theDisp, dnamW, ExposureMask);


  CBCreate(&browseCB, dirW, DIRWIDE/2, dList.y + dList.h + 6, 
	   "Browse", infofg, infobg, hicol,locol);

  CBCreate(&savenormCB, dirW, DIRWIDE/2, dList.y + dList.h + 6, 
	   "Save at normal size", infofg, infobg,hicol,locol);

  CBCreate(&flistCB, dirW, 90, dList.y + dList.h + 6, 
	   "File list", infofg, infobg,hicol,locol);


  BTCreate(&dbut[S_BOK],     dirW, 259, dList.y+((dList.h-BUTTH)*0)/4, 
	   80, BUTTH, "Ok", infofg, infobg,hicol,locol);
  BTCreate(&dbut[S_BCANC],   dirW, 259, dList.y+((dList.h-BUTTH)*1)/4, 
	   80, BUTTH, "Cancel", infofg, infobg,hicol,locol);
  BTCreate(&dbut[S_BRESCAN], dirW, 259, dList.y+((dList.h-BUTTH)*2)/4, 
	   80, BUTTH, "Rescan", infofg, infobg,hicol,locol);
  BTCreate(&dbut[S_BOLDSET], dirW, 259, dList.y+((dList.h-BUTTH)*3)/4, 
	   80, BUTTH, "Prev Set", infofg, infobg,hicol,locol);
  BTCreate(&dbut[S_BOLDNAM], dirW, 259, dList.y+((dList.h-BUTTH)*4)/4, 
	   80, BUTTH, "Prev Name", infofg, infobg,hicol,locol);


  y = dList.h + 100;
  formatRB = RBCreate(NULL, dirW, 26, y, "GIF", 
		      infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+18,  "PM", 
	   infofg, infobg, hicol, locol);
  RBCreate(formatRB, dirW, 26, y+36,  "PBM (raw)", 
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+54,  "PBM (ascii)", 
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+72,  "X11 Bitmap",
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+90,  "Sun Rasterfile", 
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+108, "BMP", 
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+126, "PostScript", 
	   infofg, infobg,hicol,locol);
  RBCreate(formatRB, dirW, 26, y+144, "IRIS",
	   infofg, infobg,hicol,locol);
  y = y + 162;

#ifdef HAVE_JPEG
  RBCreate(formatRB, dirW, 26, y, "JPEG", infofg, infobg,hicol,locol);
  y += 18;
#endif

#ifdef HAVE_TIFF
  RBCreate(formatRB, dirW, 26, y, "TIFF", infofg, infobg,hicol,locol);
  y += 18;
#endif

  y = dList.h + 100;
  colorRB = RBCreate(NULL, dirW, DIRWIDE/2, y, "Full Color", 
		     infofg, infobg,hicol,locol);
  RBCreate(colorRB, dirW, DIRWIDE/2, y+18, "Greyscale", 
	   infofg, infobg,hicol,locol);
  RBCreate(colorRB, dirW, DIRWIDE/2, y+36, "B/W Dithered", 
	   infofg, infobg,hicol,locol);
  RBCreate(colorRB, dirW, DIRWIDE/2, y+54, "Reduced Color", 
	   infofg, infobg,hicol,locol);

  SetDirFName("");

  XMapSubwindows(theDisp, dirW);

  /* have to create MBUTTs after XMapSubWindows to keep popup unmapped */
  MBCreate(&dirMB, dirW, 50, 5, DDWIDE,LINEHIGH,NULL,NULL,0,
	   infofg,infobg,hicol,locol);

  numfnames = 0;
}
  

/***************************************************/
void DirBox(mode)
int mode;
{
  int i;

  if (mode) {
    WaitCursor();  LoadCurrentDirectory();  SetCursors(-1);
  }

  if (!mode) XUnmapWindow(theDisp, dirW);  /* close */

  else if (mode == BLOAD) {
    XStoreName(theDisp, dirW, "xv load");
    XSetIconName(theDisp, dirW, "xv load");
    XResizeWindow(theDisp, dirW, DIRWIDE, dList.h+100);

    dbut[S_BLOADALL].str = "Load All";
    BTSetActive(&dbut[S_BLOADALL], 1);

    /* position buttons appropriately */
    for (i=0; i<S_LOAD_NBUTTS; i++)
      dbut[i].y = dList.y + 10 + ((dList.h-BUTTH-20)*i)/(S_LOAD_NBUTTS-1);

    CenterMapWindow(dirW, dbut[S_BOK].x+30, dbut[S_BOK].y + BUTTH/2,
		    DIRWIDE, dList.h+100);

    savemode = 0;
  }

  else if (mode == BSAVE) {
    XStoreName(theDisp, dirW, "xv save");
    XSetIconName(theDisp, dirW, "xv save");
    XResizeWindow(theDisp, dirW, DIRWIDE, DIRHIGH);

    dbut[S_BOLDSET].str = "Prev Set";

    /* position buttons appropriately */
    for (i=0; i<S_NBUTTS; i++)
      dbut[i].y = dList.y + ((dList.h-BUTTH)*i)/(S_NBUTTS-1);

    BTSetActive(&dbut[S_BOLDSET], haveoldinfo);
    BTSetActive(&dbut[S_BOLDNAM], haveoldinfo);

    CenterMapWindow(dirW, dbut[S_BOK].x+30, dbut[S_BOK].y + BUTTH/2,
		    DIRWIDE, DIRHIGH);

    savemode = 1;
  }

  scrollToFileName();

  dirUp = mode;
  BTSetActive(&but[BLOAD], !dirUp);
  BTSetActive(&but[BSAVE], !dirUp);
}


/***************************************************/
void RedrawDirW(x,y,w,h)
int x,y,w,h;
{
  int  i,ypos;
  char foo[30], *str;
  XRectangle xr;

  if (dList.nstr==1) strcpy(foo,"1 file");
                else sprintf(foo,"%d files",dList.nstr);

  ypos = dList.y + dList.h + 5 + ASCENT;
  XSetForeground(theDisp, theGC, infobg);
  XFillRectangle(theDisp, dirW, theGC, 10, ypos-ASCENT, DIRWIDE, CHIGH);
  XSetForeground(theDisp, theGC, infofg);
  XDrawString(theDisp, dirW, theGC, 10, ypos, foo, strlen(foo));

  if (dirUp == BLOAD) str = "Load file:";  
                 else str = "Save file:";
  XDrawString(theDisp, dirW, theGC, 10, dList.h+70, str,strlen(str));

  if (!savemode) 
    for (i=0; i<S_LOAD_NBUTTS; i++) BTRedraw(&dbut[i]);
  else 
    for (i=0; i<S_NBUTTS; i++) BTRedraw(&dbut[i]);

  MBRedraw(&dirMB);

  if (savemode) {
    RBRedraw(formatRB, -1);
    RBRedraw(colorRB, -1);

    ULineString(dirW, "Format", formatRB->x-16, formatRB->y-3-DESCENT);
    ULineString(dirW, "Colors", colorRB->x-16,  colorRB->y-3-DESCENT);

    CBRedraw(&flistCB);
    CBRedraw(&savenormCB);
  }
  else {
    CBRedraw(&browseCB);
  }
}


/***************************************************/
int ClickDirW(x,y)
int x,y;
{
  BUTT  *bp;
  int    bnum,i;
  char   buf[1024];


  /* check the RBUTTS first, since they don't DO anything */
  if ( (bnum=RBClick(formatRB, x,y)) >= 0) { 
    if (RBTrack(formatRB, bnum)) {
      if (RBWhich(formatRB)==4) {  /* turn off all but B/W dithered */
	RBSetActive(colorRB,0,0);
	RBSetActive(colorRB,1,0);
	RBSetActive(colorRB,3,0);
	RBSelect(colorRB,2);
      }
      else {                       /* turn on all but B/W dithered */
	RBSetActive(colorRB,0,1);
	RBSetActive(colorRB,1,1);
	RBSetActive(colorRB,3,(picType==PIC8) ? 1 : 0);
	if (picType!=PIC8 && RBWhich(colorRB)==3) RBSelect(colorRB,0);
      }

      changeSuffix();
    }
    return -1;
  }

  if ( (bnum=RBClick(colorRB, x,y)) >= 0) {
    i=RBTrack(colorRB, bnum);
    changeSuffix();
    return -1;
  }

  /* controls that are only in Load *or* Save mode */
  if (!savemode) {
    if (CBClick(&browseCB, x, y)) CBTrack(&browseCB);
  } 
  else {
    if (CBClick(&savenormCB, x, y)) CBTrack(&savenormCB);

    if (CBClick(&flistCB, x, y) && CBTrack(&flistCB)) {
      /* toggled flistCB */
      for (i=0; i<RBCount(formatRB); i++) {
	RBSetActive(formatRB, i, !flistCB.val);
      }

      for (i=0; i<RBCount(colorRB); i++) {
	RBSetActive(colorRB, i, !flistCB.val);
      }

      if (RBWhich(formatRB) == F_XBM && !flistCB.val) {
	RBSetActive(colorRB, 0, 0);
	RBSetActive(colorRB, 1, 0);
	RBSetActive(colorRB, 3, 0);
      }

    }
  }


  for (bnum=0; bnum<S_NBUTTS; bnum++) {
    bp = &dbut[bnum];
    if (PTINRECT(x, y, bp->x, bp->y, bp->w, bp->h)) break;
  }

  if (bnum<S_NBUTTS && BTTrack(bp)) {   /* found one */
    if (bnum<S_BOLDSET) return bnum;  /* do Ok,Cancel,Rescan in xvevent.c */

    if (bnum == S_BOLDSET && savemode && haveoldinfo) {
      RBSelect(formatRB, oldformat);
      RBSelect(colorRB, oldcolors);
      changeSuffix();
    }

    else if (bnum == S_BOLDNAM && savemode && haveoldinfo) {
      setFName(oldfname);
    }

    else if (bnum == S_BLOADALL && !savemode) {
      int j, oldnumnames;
      char *dname;

      oldnumnames = numnames;

      for (i=0; i<numfnames && numnames<MAXNAMES; i++) {
	if (fnames[i][0] == C_REG || fnames[i][0] == C_EXE) {
	  sprintf(buf,"%s%s", path, fnames[i]+1);

	  /* check for dups.  Don't add it if it is. */
	  for (j=0; j<numnames && strcmp(buf,namelist[j]); j++);

	  if (j==numnames) {  /* add to list */
	    namelist[numnames] = (char *) malloc(strlen(buf)+1);
	    if (!namelist[numnames]) FatalError("out of memory!\n");
	    strcpy(namelist[numnames],buf);

	    dname = namelist[numnames];

	    /* figure out how much of name can be shown */
	    if (StringWidth(dname) > (nList.w-10-16)) {   /* truncate */
	      char *tmp;
	      int   prelen = 0;

	      tmp = dname;
	      while (1) {
		tmp = (char *) strchr(tmp,'/'); /* find next '/' in buf */
		if (!tmp) break;

		tmp++;                   /* move to char following the '/' */
		prelen = tmp - dname;
		if (StringWidth(tmp) <= (nList.w-10-16)) break; /* cool now */
	      }

	      dispnames[numnames] = dname + prelen;
	    }
	    else dispnames[numnames] = dname;

	    numnames++;
	  }
	}
      }

      if (oldnumnames != numnames) {  /* added some */
	if (numnames>0) BTSetActive(&but[BDELETE],1); 
	if (numnames>0) BTSetActive(&but[BTXVIEW],1); 
	LSNewData(&nList, dispnames, numnames);
	nList.selected = oldnumnames;
	curname = oldnumnames - 1;

	ActivePrevNext();

	ScrollToCurrent(&nList);
	XClearArea(theDisp, ctrlW, 0, 0, 200, 40, True);

	if (!browseCB.val) DirBox(0);
      }

    }
  }



  if (MBClick(&dirMB, x, y)) {
    i = MBTrack(&dirMB);
    if (i >= 0) changedDirMB(i);
  }

  return -1;
}


/***************************************************/
void SelectDir(n)
int n;
{
  /* called when entry #n in the dir list was selected/double-clicked */

  /* if n<0, nothing was double-clicked, but perhaps the selection
     has changed.  Copy the selection to the filename if a) we're in
     the 'load' box, and b) it's not a directory name */

  if (n<0) {
    if (dList.selected>=0)
      setFName(dList.str[dList.selected]+1);
    return;
  }

  /* can just pretend 'enter' was hit on a double click, as the original
     click would've copied the string to filename */

  if (!DirCheckCD()) FakeButtonPress(&dbut[S_BOK]);
}



/***************************************************/
static void changedDirMB(sel)
     int sel;
{
  if (sel != 0) {   /* changed directories */
    char tmppath[MAXPATHLEN+1], *trunc_point;

    /* end 'path' by changing trailing '/' (of dir name) to a '\0' */
    trunc_point = (dirs[(ndirs-1)-sel + 1] - 1);
    *trunc_point = '\0';

    if (path[0] == '\0') {
      /* special case:  if cd to '/', fix path (it's currently "") */
#ifdef apollo    /*** Apollo DomainOS uses // as the network root ***/
      strcpy(tmppath,"//");
#else
      strcpy(tmppath,"/");
#endif
    }
    else strcpy(tmppath, path);

#ifdef VMS
    /*
     *  The VMS chdir always needs 2 components (device and directory),
     *  so convert "/device" to "/device/000000" and convert
     *  "/" to "/DEVICE_LIST_ROOT/000000" (device_list_root is special
     *  concealed device setup to provide list of available disks).
     */
    if ( ((ndirs-sel) == 2) && (strlen(tmppath) > 1) ) 
      strcat ( tmppath, "/000000" ); /* add root dir for device */
    else if  ((ndirs-sel) == 1 ) {
      strcpy ( tmppath, "/device_list_root/000000" );  /* fake top level */
    }
#endif

    if (chdir(tmppath)) {
      char str[512];
      sprintf(str,"Unable to cd to '%s'\n", tmppath);
      *trunc_point = '/';  /* restore the path */
      MBRedraw(&dirMB);
      ErrPopUp(str, "\nWhatever");
    }
    else {
      loadCWD();
    }
  }
}


/***************************************************/
static void RedrawDList(delta, sptr)
     int   delta;
     SCRL *sptr;
{
  LSRedraw(&dList,delta);
}


/***************************************************/
static void loadCWD()
{
  /* loads up current-working-directory into load/save list */

  GETWD(path);
  LoadCurrentDirectory();
}
  


/***************************************************/
void LoadCurrentDirectory()
{
  /* rescans current load/save directory */

  DIR           *dirp;
  int            i, j, ftype, mode, changedDir;
  struct stat    st;
  char          *dbeg, *dend;
  static char    oldpath[MAXPATHLEN + 2] = { '\0' };

#ifdef NODIRENT
  struct direct *dp;
#else
  struct dirent *dp;
#endif
  

  /* get rid of previous file names */
  for (i=0; i<numfnames; i++) free(fnames[i]);
  numfnames = 0;

  /* get rid of old dirMBlist */
  for (i=0; i<ndirs; i++) free(dirMBlist[i]);

#ifndef VMS
  if (strlen(path) == 0) GETWD(path);  /* don't have load/save dir, use cwd */
#else
  GETWD(path);
#endif

  if (chdir(path)) {
    ErrPopUp("Current load/save directory seems to have gone away!",
	     "\nYikes!");
#ifdef apollo
    strcpy(path,"//");
#else
    strcpy(path,"/");
#endif
    chdir(path);
  }

  changedDir = strcmp(path, oldpath);
  strcpy(oldpath, path);

  if (strlen(path)>1 && path[strlen(path)-1] != '/')
    strcat(path,"/");   /* tack on a trailing '/' to make path consistent */

  /* path will be something like: "/u3/bradley/src/weiner/whatever/" */
  /* parse path into individual directory names */
  dbeg = dend = path;
  for (i=0; i<MAXDEEP && dend; i++) {
    dend = (char *) strchr(dbeg,'/');  /* find next '/' char */

#ifdef apollo
    /** On apollos the path will be something like //machine/users/foo/ **/
    /** handle the initial // **/
    if ((dend == dbeg ) && (dbeg[0] == '/') && (dbeg[1] == '/')) dend += 1;
#endif

    dirs[i] = dbeg;
    dbeg = dend+1;
  }
  ndirs = i-1;


  /* build dirMBlist */
  for (i=ndirs-1,j=0; i>=0; i--,j++) {
    int stlen = (i<(ndirs-1)) ? dirs[i+1] - dirs[i] : strlen(dirs[i]);
    dirMBlist[j] = (char *) malloc(stlen+1);
    if (!dirMBlist[j]) FatalError("unable to malloc dirMBlist[]");

    strncpy(dirMBlist[j], dirs[i], stlen);
    dirMBlist[j][stlen] = '\0';
  }
    

  lastdir = dirs[ndirs-1];
  dirMB.list = dirMBlist;
  dirMB.nlist = ndirs;
  XClearArea(theDisp, dirMB.win, dirMB.x,dirMB.y, dirMB.w+3,dirMB.h+3, False);
  i = StringWidth(dirMBlist[0]) + 10;
  dirMB.x = dirMB.x + dirMB.w/2 - i/2;
  dirMB.w = i;
  MBRedraw(&dirMB);


  dirp = opendir(".");
  if (!dirp) {
    LSNewData(&dList, fnames, 0);
    RedrawDirW(0,0,DIRWIDE,DIRHIGH);
    return;
  }

  WaitCursor();

  i=0;
  while ( (dp = readdir(dirp)) != NULL) {
    if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0 ||
	strcmp(dp->d_name, THUMBDIR)==0) {
      /* skip over '.' and '..' and THUMBDIR */
    }
    else {

      if (i == MAXNAMES) {
	fprintf(stderr,
		"%s: too many directory entries.  Only using first %d.\n",
		cmd, MAXNAMES);
	break;
      }

      if ((i&31)==0) WaitCursor();

      fnames[i] = (char *) malloc(strlen(dp->d_name)+2); /* +2=ftype + '\0' */

      if (!fnames[i]) FatalError("malloc error while reading directory");
      strcpy(fnames[i]+1, dp->d_name);

      /* figure out what type of file the beastie is */
      fnames[i][0] = C_REG;   /* default to normal file, if stat fails */

#ifdef VMS
      /* For VMS we will default all files EXCEPT directories to avoid
	 the high cost of the VAX C implementation of the stat function.
	 Suggested by Kevin Oberman (OBERMAN@icdc.llnl.gov) */
 
      if (strstr (fnames[i]+1, ".DIR") != NULL) fnames[i][0] = C_DIR;
      if (strstr (fnames[i]+1, ".EXE") != NULL) fnames[i][0] = C_EXE;
      if (strstr (fnames[i]+1, ".OBJ") != NULL) fnames[i][0] = C_BLK;
#else
      if (!nostat && (stat(fnames[i]+1, &st)==0)) {
	mode  = st.st_mode & 0777;     /* rwx modes */

#  if defined (_POSIX_SOURCE)
	ftype = st.st_mode;
	if      (S_ISDIR(ftype))  fnames[i][0] = C_DIR;
	else if (S_ISCHR(ftype))  fnames[i][0] = C_CHR;
	else if (S_ISBLK(ftype))  fnames[i][0] = C_BLK;
#    ifdef S_ISFIFO
	else if (S_ISFIFO(ftype)) fnames[i][0] = C_FIFO;
#    endif
	/* Hack: S_IFSOCK is not POSIX and there's no macro for this test */
	else if ((ftype & 0xC000) == 0xC000) fnames[i][0] = C_SOCK;

#  else /* not _POSIX_SOURCE */

	ftype = st.st_mode & S_IFMT;   /* mask off uninteresting bits */
	if      (ftype == S_IFDIR)  fnames[i][0] = C_DIR;
	else if (ftype == S_IFCHR)  fnames[i][0] = C_CHR;
	else if (ftype == S_IFBLK)  fnames[i][0] = C_BLK;
#    ifdef S_IFIFO
	else if (ftype == S_IFIFO)  fnames[i][0] = C_FIFO;
#    endif
#    ifdef S_IFLNK
	else if (ftype == S_IFLNK)  fnames[i][0] = C_LNK;
#    endif
#    ifdef S_IFSOCK
        else if (ftype == S_IFSOCK) fnames[i][0] = C_SOCK;
#    endif

#  endif /* POSIX_SOURCE */

	else if (fnames[i][0] == C_REG && (mode&0111)) fnames[i][0] = C_EXE;

      }
      else {
	/* fprintf(stderr,"problems 'stat-ing' files\n");*/
	fnames[i][0] = C_REG;
      }
#endif /* VMS */

      i++;
    }
  }

  closedir(dirp);

  numfnames = i;

  qsort((char *) fnames, numfnames, sizeof(char *), dnamcmp);

  if (changedDir) LSNewData(&dList, fnames, numfnames);
             else LSChangeData(&dList, fnames, numfnames);
  RedrawDirW(0,0,DIRWIDE,DIRHIGH);
  SetCursors(-1);
}


/***************************************************/
void GetDirPath(buf)
     char *buf;
{
  /* returns current 'dirW' path.  buf should be MAXPATHLEN long */

  strcpy(buf, path);
}


/***************************************************/
static int cd_able(str)
char *str;
{
  return ((str[0] == C_DIR || str[0] == C_LNK));
}


/***************************************************/
static int dnamcmp(s1,s2)
char **s1, **s2;
{
#ifdef FOO
  /* sort so that directories are at beginning of list */

  /* if both dir/lnk or both NOT dir/lnk, sort on name */

  if ( ( cd_able(*s1) &&  cd_able(*s2)) ||
       (!cd_able(*s1) && !cd_able(*s2)))
    return (strcmp((*s1)+1, (*s2)+1));

  else if (cd_able(*s1)) return -1;  /* s1 is first */
  else return 1;                     /* s2 is first */
#else
  /* sort in pure alpha order */
  return(strcmp((*s1)+1, (*s2)+1));
#endif
}





/***************************************************/
int DirKey(c)
int c;
{
  /* got keypress in dirW.  stick on end of filename */
  int len;

  len = strlen(filename);
  
  if (c>=' ' && c<'\177') {             /* printable characters */
    /* note: only allow 'piped commands' in savemode... */

    /* only allow spaces in 'piped commands', not filenames */
    if (c==' ' && (!ISPIPE(filename[0]) || curPos==0)) return (-1);

    /* only allow vertbars in 'piped commands', not filenames */
    if (c=='|' && curPos!=0 && !ISPIPE(filename[0])) return(-1);

    if (len >= MAXFNLEN-1) return(-1);  /* max length of string */
    xvbcopy(&filename[curPos], &filename[curPos+1], len-curPos+1);
    filename[curPos]=c;  curPos++;

    scrollToFileName();
  }

  else if (c=='\010' || c=='\177') {    /* BS or DEL */
    if (curPos==0) return(-1);          /* at beginning of str */
    xvbcopy(&filename[curPos], &filename[curPos-1], len-curPos+1);
    curPos--;

    if (strlen(filename)>0) scrollToFileName();
  }

  else if (c=='\025') {                 /* ^U: clear entire line */
    filename[0] = '\0';
    curPos = 0;
  }

  else if (c=='\013') {                 /* ^K: clear to end of line */
    filename[curPos] = '\0';
  }

  else if (c=='\001') {                 /* ^A: move to beginning */
    curPos = 0;
  }

  else if (c=='\005') {                 /* ^E: move to end */
    curPos = len;
  }

  else if (c=='\004') {                 /* ^D: delete character at curPos */
    if (curPos==len) return(-1);
    xvbcopy(&filename[curPos+1], &filename[curPos], len-curPos);
  }

  else if (c=='\002') {                 /* ^B: move backwards char */
    if (curPos==0) return(-1);
    curPos--;
  }

  else if (c=='\006') {                 /* ^F: move forwards char */
    if (curPos==len) return(-1);
    curPos++;
  }

  else if (c=='\012' || c=='\015') {    /* CR or LF */
    if (!DirCheckCD()) FakeButtonPress(&dbut[S_BOK]);
  }

  else if (c=='\033') {                  /* ESC = Cancel */
    FakeButtonPress(&dbut[S_BCANC]);
  }

  else return(-1);                      /* unhandled character */

  showFName();

  /* if we cleared out filename, clear out deffname as well */
  if (!filename[0]) deffname[0] = '\0';

  return(0);
}


/***************************************************/
static void scrollToFileName()
{
  int i, hi, lo, pos, cmp;

  /* called when 'fname' changes.  Tries to scroll the directory list
     so that fname would be centered in it */

  /* nothing to do if scrlbar not enabled ( <= NLINES names in list) */
  if (dList.scrl.max <= 0) return;

  /* find the position in the namelist that the current name should be at
     (binary search) */

  pos = 0;  lo = 0;  hi = dList.nstr-1;
  i = strlen(filename);
  if (!i) { SCSetVal(&dList.scrl, 0); return; }

  while ((hi-lo)>=0) {
    pos = lo + (hi-lo)/2;
    cmp = strcmp(filename, dList.str[pos]+1);
    if      (cmp<0) hi = pos-1;
    else if (cmp>0) lo = pos+1;
    else break;      /* found it! */
  }

  /* set scroll position so that 'pos' will be centered in the list */
  i = pos - (NLINES/2);
  SCSetVal(&dList.scrl, i);
}
  

/***************************************************/
void RedrawDNamW()
{
  int cpos;

  /* draw substring filename[stPos:enPos] and cursor */

  Draw3dRect(dnamW, 0,0,DNAMWIDE+5, LINEHIGH+4, R3D_IN, 2, 
	     hicol, locol, infobg);

  XSetForeground(theDisp, theGC, infofg);

  if (stPos>0) {  /* draw a "there's more over here" doowah */
    XDrawLine(theDisp, dnamW, theGC, 0,0,0,LINEHIGH+5);
    XDrawLine(theDisp, dnamW, theGC, 1,0,1,LINEHIGH+5);
    XDrawLine(theDisp, dnamW, theGC, 2,0,2,LINEHIGH+5);
  }

  if (enPos<strlen(filename)) {  /* draw a "there's more over here" doowah */
    XDrawLine(theDisp, dnamW, theGC, DNAMWIDE+5,0,DNAMWIDE+5,LINEHIGH+5);
    XDrawLine(theDisp, dnamW, theGC, DNAMWIDE+4,0,DNAMWIDE+4,LINEHIGH+5);
    XDrawLine(theDisp, dnamW, theGC, DNAMWIDE+3,0,DNAMWIDE+3,LINEHIGH+5);
  }

  XDrawString(theDisp, dnamW, theGC,3,ASCENT+3,filename+stPos, enPos-stPos);

  cpos = XTextWidth(mfinfo, &filename[stPos], curPos-stPos);
  XDrawLine(theDisp, dnamW, theGC, 3+cpos, 2, 3+cpos, 2+CHIGH+1);
  XDrawLine(theDisp, dnamW, theGC, 3+cpos, 2+CHIGH+1, 5+cpos, 2+CHIGH+3);
  XDrawLine(theDisp, dnamW, theGC, 3+cpos, 2+CHIGH+1, 1+cpos, 2+CHIGH+3);
}


/***************************************************/
int DoSave()
{
  FILE *fp;
  byte *thepic, *bwpic, *rp, *gp, *bp, *gampic24;
  int   i, w, h, rv, fmt, col, nc, ptype;
  char  fullname[MAXPATHLEN + 2];

  /* opens file, does appropriate color pre-processing, calls save routine
     based on chosen format.  Returns '0' if successful */

  ptype = picType;
  dbut[S_BOK].lit = 1;  BTRedraw(&dbut[S_BOK]);

  strcpy(globname, filename);
  if (globname[0] == '~') Globify(globname);

  if (globname[0] != '/') sprintf(fullname, "%s%s", path, globname);
  else strcpy(fullname, globname);


  if (flistCB.val) {       /* write filename list */
    fp = OpenOutFile(fullname);
    if (!fp) {
      SetCursors(-1);
      dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
      return -1;
    }

    for (i=0; i<numnames; i++) {
      if ((i&0x3f)==0) WaitCursor();
      if (namelist[i][0] != '/') fprintf(fp, "%s/%s\n", initdir, namelist[i]);
                            else fprintf(fp, "%s\n", namelist[i]);
    }

    i = (ferror(fp)) ? 1 : 0;
    if (CloseOutFile(fp, fullname, i) == 0) {
      DirBox(0);
      XVCreatedFile(fullname);
    }

    SetCursors(-1);
    dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
    return i;
  }


  if (savenormCB.val) { thepic = cpic;  w = cWIDE;  h = cHIGH; }
                 else { thepic = epic;  w = eWIDE;  h = eHIGH; }

  col = RBWhich(colorRB);  fmt = RBWhich(formatRB);


  /* handle formats that pop up 'how do you want to save this' boxes */
  if (fmt == F_PS) {   /* PostScript */
    PSSaveParams(globname, col);
    PSDialog(1);                   /* open PSDialog box */
    dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
    return 0;                      /* always 'succeeds' */
  }

#ifdef HAVE_JPEG
  else if (fmt == F_JPEG) {   /* JPEG */
    JPEGSaveParams(globname, col);
    JPEGDialog(1);                   /* open JPEGDialog box */
    dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
    return 0;                      /* always 'succeeds' */
  }
#endif

#ifdef HAVE_TIFF
  else if (fmt == F_TIFF) {   /* TIFF */
    TIFFSaveParams(globname, col);
    TIFFDialog(1);                   /* open TIFF Dialog box */
    dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
    return 0;                      /* always 'succeeds' */
  }
#endif




  WaitCursor();

  bwpic = HandleBWandReduced(col, &nc, &rp, &gp, &bp);
  if (bwpic) { thepic = bwpic;  ptype = PIC8; }


  /* deal with possiblity of Gamma-fication in 24-bit mode... */
  gampic24 = NULL;
  if (ptype == PIC24) {
    gampic24 = GammifyPic24(thepic, w, h);
    if (gampic24) thepic = gampic24;
  }



  fp = OpenOutFile(fullname);
  if (!fp) {
    if (bwpic) free(bwpic);
    SetCursors(-1);
    dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);
    return -1;
  }


  if (col == F_REDUCED) col = F_FULLCOLOR;
  rv = 0;

  switch (fmt) {
  case F_GIF:
    rv = WriteGIF(fp, thepic, ptype, w, h, rp, gp, bp, nc, col, picComments);
    break;

  case F_PM:
    rv = WritePM (fp, thepic, ptype, w, h, rp, gp, bp, nc, col, picComments);
    break;

  case F_PBMRAW:
    rv = WritePBM(fp, thepic, ptype, w, h, rp,gp,bp, nc, col, 1, picComments);
    break;

  case F_PBMASCII: 
    rv = WritePBM(fp, thepic, ptype, w, h, rp,gp,bp, nc, col, 0, picComments);
    break;

  case F_XBM:
    rv = WriteXBM(fp, thepic, w, h, rp, gp, bp, globname);  break;

  case F_SUNRAS:
    rv = WriteSunRas(fp, thepic, ptype, w, h, rp, gp, bp, nc, col,0);  break;

  case F_BMP:
    rv = WriteBMP(fp, thepic, ptype, w, h, rp, gp, bp, nc, col);  break;

  case F_IRIS:
    rv = WriteIRIS(fp, thepic, ptype, w, h, rp, gp, bp, nc, col);  break;
  }

  if (CloseOutFile(fp, fullname, rv) == 0) {
    DirBox(0);
    if (!dopipe) {
      XVCreatedFile(fullname);
      StickInCtrlList();
    }
  }

  if (bwpic) free(bwpic);
  if (gampic24) free(gampic24);

  SetCursors(-1);
  dbut[S_BOK].lit = 0;  BTRedraw(&dbut[S_BOK]);

  return rv;
}



/***************************************************/
void SetDirFName(st)
char *st;
{
  strncpy(deffname, st, MAXFNLEN-1);
  setFName(st);
}


/***************************************************/
static void setFName(st)
char *st;
{
  strncpy(filename, st, MAXFNLEN-1);
  filename[MAXFNLEN-1] = '\0';  /* make sure it's terminated */
  curPos = strlen(st);
  stPos = 0;  enPos = curPos;

  showFName();
}


/***************************************************/
static void showFName()
{
  int len;

  len = strlen(filename);

  if (curPos<stPos) stPos = curPos;
  if (curPos>enPos) enPos = curPos;

  if (stPos>len) stPos = (len>0) ? len-1 : 0;
  if (enPos>len) enPos = (len>0) ? len-1 : 0;

  /* while substring is shorter than window, inc enPos */

  while (XTextWidth(mfinfo, &filename[stPos], enPos-stPos) < DNAMWIDE
	 && enPos<len) { enPos++; }

  /* while substring is longer than window, dec enpos, unless enpos==curpos,
     in which case, inc stpos */

  while (XTextWidth(mfinfo, &filename[stPos], enPos-stPos) > DNAMWIDE) {
    if (enPos != curPos) enPos--;
    else stPos++;
  }


  if (ctrlColor) 
    XClearArea(theDisp, dnamW, 2,2,DNAMWIDE+5-3, LINEHIGH+4-3, False);
  else XClearWindow(theDisp, dnamW);

  RedrawDNamW();
  BTSetActive(&dbut[S_BOK], strlen(filename)!=0);
}


/***************************************************/
char *GetDirFName()
{
  return (filename);
}


/***************************************************/
void SetDirRButt(group, bnum)
int group, bnum;
{
  if (group == F_COLORS) {
    if (picType == PIC24) {   /* disable REDUCED COLOR */
      RBSetActive(colorRB, 3, 0);
      if (RBWhich(colorRB)==3) RBSelect(colorRB, 0);
    }
    else {  /* PIC8 - turn on REDUCED COLOR, if not XBM */
      if (RBWhich(formatRB) != F_XBM) RBSetActive(colorRB, 3, !flistCB.val);
    }

    if (bnum>=0) RBSelect(colorRB, bnum);
  }

  else if (group == F_FORMAT) {
    RBSelect(formatRB, bnum);

    if (RBWhich(formatRB) == F_XBM) { /* turn off all but B/W */
      RBSetActive(colorRB,0,0);
      RBSetActive(colorRB,1,0);
      RBSetActive(colorRB,3,0);
      RBSelect(colorRB,2);
    }

    else {                       /* turn on all but B/W dithered */
      RBSetActive(colorRB,0,1);
      RBSetActive(colorRB,1,1);
      RBSetActive(colorRB,3, (picType==PIC8) ? 1 : 0);
      if (picType!=PIC8 && RBWhich(colorRB)==3) RBSelect(colorRB,0);
    }
  }
}

  

/***************************************/
static void changeSuffix()
{
  /* see if there's a common suffix at the end of the filename.  
     if there is, remember what case it was (all caps or all lower), lop
     it off, and replace it with a new appropriate suffix, in the
     same case */

  int allcaps;
  char *suffix, *sp, *dp, lowsuf[512];

  /* find the last '.' in the filename */
  suffix = (char *) strrchr(filename, '.');
  if (!suffix) return;
  suffix++;  /* point to first letter of the suffix */

  /* check for all-caposity */
  for (sp = suffix, allcaps=1; *sp; sp++) 
    if (islower(*sp)) allcaps = 0;

  /* copy the suffix into an all-lower-case buffer */
  for (sp=suffix, dp=lowsuf; *sp; sp++, dp++) {
    *dp = (isupper(*sp)) ? tolower(*sp) : *sp;
  }
  *dp = '\0';

  /* compare for common suffixes */
  if ((strcmp(lowsuf,"gif" )==0) ||
      (strcmp(lowsuf,"pm"  )==0) ||
      (strcmp(lowsuf,"pbm" )==0) ||
      (strcmp(lowsuf,"pgm" )==0) ||
      (strcmp(lowsuf,"ppm" )==0) ||
      (strcmp(lowsuf,"pnm" )==0) ||
      (strcmp(lowsuf,"bm"  )==0) ||
      (strcmp(lowsuf,"xbm" )==0) ||
      (strcmp(lowsuf,"ras" )==0) ||
      (strcmp(lowsuf,"bmp" )==0) ||
      (strcmp(lowsuf,"ps"  )==0) ||
      (strcmp(lowsuf,"eps" )==0) ||
      (strcmp(lowsuf,"rgb" )==0) ||
      (strcmp(lowsuf,"jpg" )==0) ||
      (strcmp(lowsuf,"jpeg")==0) ||
      (strcmp(lowsuf,"jfif")==0) ||
      (strcmp(lowsuf,"tif" )==0) ||
      (strcmp(lowsuf,"tiff")==0)) {

    /* found one.  set lowsuf = to the new suffix, and tack on to filename */

    int fmt, col;
    fmt = RBWhich(formatRB);
    col = RBWhich(colorRB);

    switch (fmt) {
    case F_GIF:      strcpy(lowsuf,"gif");  break;
    case F_PM:       strcpy(lowsuf,"pm");   break;
    case F_PBMRAW:
    case F_PBMASCII: if (col == F_FULLCOLOR || col == F_REDUCED) 
                                                  strcpy(lowsuf,"ppm");
                     else if (col == F_GREYSCALE) strcpy(lowsuf,"pgm");
                     else if (col == F_BWDITHER)  strcpy(lowsuf,"pbm");
                     break;

    case F_XBM:      strcpy(lowsuf,"xbm");  break;
    case F_SUNRAS:   strcpy(lowsuf,"ras");  break;
    case F_BMP:      strcpy(lowsuf,"bmp");  break;
    case F_PS:       strcpy(lowsuf,"ps");   break;
    case F_IRIS:     strcpy(lowsuf,"rgb");  break;

#ifdef HAVE_JPEG
    case F_JPEG:     strcpy(lowsuf,"jpg");  break;
#endif

#ifdef HAVE_TIFF
    case F_TIFF:     strcpy(lowsuf,"tif");  break;
#endif
    }

    if (allcaps) {  /* upper-caseify lowsuf */
      for (sp=lowsuf; *sp; sp++) 
	*sp = (islower(*sp)) ? toupper(*sp) : *sp;
    }

    /* one other case:  if the original suffix started with a single
       capital letter, make the new suffix start with a single cap */
    if (isupper(suffix[0])) lowsuf[0] = toupper(lowsuf[0]);

    strcpy(suffix, lowsuf);   /* tack onto filename */
    SetDirFName(filename);
  }

}
  

/***************************************************/
int DirCheckCD()
{
  /* checks if the current filename is a directory.  If so,
     cd's there, resets the filename to 'deffname', and returns '1'

     otherwise, does nothing and returns '0' */

  if (FNameCdable()) {
    setFName(deffname);
    return 1;
  }

  return 0;
}


/***************************************************/
static int FNameCdable()
{
  /* returns '1' if filename is a directory, and goes there */
  
  char newpath[1024];
  struct stat st;
  int retval = 0;

  newpath[0] = '\0';   /* start out empty */

  if (ISPIPE(filename[0]) || strlen(filename)==0) return 0;

  if (filename[0] == '/' || filename[0] == '~') {  /* absolute path */
    strcpy(newpath, filename);
  }
  else {  /* not an absolute pathname */
    strcpy(newpath,path);
    strcat(newpath,filename);
  }

  if (newpath[0]=='~') {    /* handle globbing */
    Globify(newpath);
  }

#ifdef VMS
  /* Convert names of form "/device.dir" to "/device/000000.DIR"  */
  if ( strrchr ( newpath, '/' ) == newpath ) {
    strcpy ( strrchr ( newpath, '.' ), "/000000.DIR" );
  }
#endif

  if (stat(newpath, &st)==0) {
    int isdir;

#if defined(_POSIX_SOURCE)
    isdir = S_ISDIR(st.st_mode);
#else
    isdir = (st.st_mode & S_IFMT) == S_IFDIR;
#endif

    if (isdir) {
#ifdef VMS
      /*
       * remove the .DIR from the path so that false 000000 directories work
       */
      char *dirext;
      dirext = strrchr ( newpath, '/' );
      if ( dirext == NULL ) dirext = newpath; else dirext++;
      dirext = strstr ( dirext, "." );
      *dirext = '\0';
#endif

      if (chdir(newpath)==0) {
	loadCWD();  /* success! */
      }

      else {
	char str[512];

	sprintf(str,"Can't chdir to '%s'.\n\n  %s.",filename, ERRSTR(errno));
	ErrPopUp(str, "\nPity");
      }
      retval = 1;
    }
  }
  
  return retval;
}


/**************************************************************************/
int Globify(fname)
  char *fname;
{
  /* expands ~s in file names.  Returns the name inplace 'name'.
     returns 0 if okay, 1 if error occurred (user name not found) */

  struct passwd *entry;
  char *cp, *sp, *up, uname[64], tmp[MAXFNLEN+100];

#ifdef VMS
  return 1;
#else
  if (*fname != '~') return 0; /* doesn't start with a tilde, don't expand */

  /* look for the first '/' after the tilde */
  sp = index(fname,'/');
  if (sp == 0) {               /* no '/' after the tilde */
    sp = fname+strlen(fname);  /* sp = end of string */
  }

  /* uname equals the string between the ~ and the / */
  for (cp=fname+1,up=uname; cp<sp; *up++ = *cp++);
  *up='\0';

  if (*uname=='\0') { /* no name.  substitute ~ with $HOME */
    char *homedir;
    homedir = (char *) getenv("HOME");  
    if (homedir == NULL) homedir = ".";
    strcpy(tmp,homedir);
    strcat(tmp,sp);
  }

  else {              /* get password entry for uname */
    entry = getpwnam(uname);
    if (entry==0) return 1;       /* name not found */
    strcpy(tmp,entry->pw_dir);
    strcat(tmp,sp);
    endpwent();
  }

  strcpy(fname,tmp);  /* return expanded file name */
  return 0;
#endif  /* !VMS */
}




/***************************************/
FILE *OpenOutFile(filename)
     char *filename;
{
  /* opens file for output.  does various error handling bits.  Returns
     an open file pointer if success, NULL if failure */

  FILE *fp;
  struct stat st;

  if (!filename || filename[0] == '\0') return NULL;
  strcpy(outFName, filename);
  dopipe = 0;

  /* make sure we're in the correct directory */
  if (strlen(path)) chdir(path);

  if (ISPIPE(filename[0])) {   /* do piping */
    /* make up some bogus temp file to put this in */
    sprintf(outFName, "%s/xvXXXXXX", tmpdir);
    mktemp(outFName);
    dopipe = 1;
  }


  /* see if file exists (ie, we're overwriting) */
  if (stat(outFName, &st)==0) {   /* stat succeeded, file must exist */
    static char *foo[] = { "\nOk", "\033Cancel" };
    char str[512];

    sprintf(str,"Overwrite existing file '%s'?", outFName);
    if (PopUp(str, foo, 2)) return NULL;
  }
    

  /* Open file */
  fp = fopen(outFName, "w");
  if (!fp) {
    char  str[512];
    sprintf(str,"Can't write file '%s'\n\n  %s.",outFName, ERRSTR(errno));
    ErrPopUp(str, "\nBummer");
    return NULL;
  }

  return fp;
}
  

/***************************************/
int CloseOutFile(fp, filename, failed)
     FILE *fp;
     char *filename;
     int   failed;
{
  char buf[64];

  /* close output file, and if piping, deal... Returns '0' if everything OK */

  if (failed) {    /* failure during format-specific output routine */
    char  str[512];
    sprintf(str,"Couldn't write file '%s'.", outFName);
    ErrPopUp(str, "\nBummer!");
    unlink(outFName);   /* couldn't properly write file:  delete it */
    return 1;
  }

    
  if (fclose(fp) == EOF) {
    static char *foo[] = { "\nWeird!" };
    char  str[512];
    sprintf(str,"Can't close file '%s'\n\n  %s.",outFName, ERRSTR(errno));
    ErrPopUp(str, "\nWeird!");
    return 1;
  }

  buf[0]= '\0';  /* empty buffer */
  { /* compute size of written file */
    FILE *fp;
    long  filesize;
    fp = fopen(outFName,"r");
    if (fp) {
      fseek(fp, 0L, 2);
      filesize = ftell(fp);
      fclose(fp);

      sprintf(buf,"  (%ld bytes)", filesize);
    }
  }

  SetISTR(ISTR_INFO,"Successfully wrote '%s'%s", outFName, buf);
  
  if (dopipe) {
    char cmd[512], str[1024];
    sprintf(cmd, "cat %s |%s", outFName, filename+1);  /* lose pipe char */
    sprintf(str,"Doing command: '%s'", cmd);
    OpenAlert(str);
    if (system(cmd)) {
      sprintf(str, "Unable to complete command:\n  %s", cmd);
      CloseAlert();
      ErrPopUp(str, "\nThat Sucks!");
      unlink(outFName);
      return 1;
    }
    else {
      CloseAlert();
      SetISTR(ISTR_INFO,"Successfully completed command.");
      unlink(outFName);
    }
  }

  /* save old info */
  haveoldinfo = 1;
  oldformat = RBWhich(formatRB);
  oldcolors = RBWhich(colorRB);
  strcpy(oldfname, filename);

  return 0;
}

      


static byte rBW[2], gBW[2], bBW[2];
static byte gray[256];

/***************************************/
byte *HandleBWandReduced(color, nc, rpp, gpp, bpp)
     int color, *nc;
     byte **rpp, **gpp, **bpp;
{
  /* given 'color' (the mode selected by colorRB), we may have to dither
     and/or use different colormaps.  Returns 'nc', rpp, gpp, bpp (the
     colormap to use).  Also, if the function returns non-NULL, it generated
     a new (dithered) image to use. */

  int   i;
  byte *bwpic = NULL;



  /* quick check:  if we're saving a 24-bit image, then none of this 
     complicated 'reduced'/dithered/smoothed business comes into play.
     'reduced' is disabled, for semi-obvious reasons, in 24-bit mode,
     as is 'dithered'.  If 'smoothed', and we're saving at current
     size, no problem.  Otherwise, if we're saving at original size,
     smoothing should have no effect, so there's no reason to smooth
     the original pic...

     In any event:  in 24-bit mode, all we have to do here is determine
     if we're saving B/W DITHERED, and deal accordingly */

  if (picType == PIC24) {  
    if (color == F_BWDITHER) {  /* generate a bwdithered version of epic */
      byte *thepic, *p24;  int w,h;

      /* generate a FSDithered 1-byte per pixel image */

      if (savenormCB.val) { thepic = cpic;  w = cWIDE;  h = cHIGH; }
                     else { thepic = epic;  w = eWIDE;  h = eHIGH; }

      /* gammify... */
      p24 = GammifyPic24(thepic, w, h);
      if (p24) thepic = p24;

      bwpic = FSDither(thepic, PIC24, w, h, NULL,NULL,NULL, 0, 1);
      if (!bwpic) FatalError("unable to malloc dithered picture (DoSave)");

      if (p24) free(p24);  /* won't need it any more */

      /* build a BW colormap */
      rBW[0] = gBW[0] = bBW[0] = 0;
      rBW[1] = gBW[1] = bBW[1] = 255;

      *rpp = rBW;  *gpp = gBW;  *bpp = bBW;
      *nc = 2;

      return bwpic;
    }

    else return NULL;
  }
    



  *nc = numcols;  *rpp = rMap;  *gpp = gMap;  *bpp = bMap;
  if (color==F_REDUCED) { *rpp = rdisp;  *gpp = gdisp;  *bpp = bdisp; }

  /* if DITHER or SMOOTH, and color==FULLCOLOR or GREY, 
     make color=REDUCED, so it will be written with the correct colortable  */

  if ((epicMode == EM_DITH || epicMode == EM_SMOOTH) && color != F_REDUCED) {
    if      (color == F_FULLCOLOR) {
      *rpp = rdisp;  *gpp = gdisp;  *bpp = bdisp;
    }
    else if (color == F_GREYSCALE) {
      for (i=0; i<256; i++) gray[i] = MONO(rdisp[i], gdisp[i], bdisp[i]);
      *rpp = gray;  *gpp = gray;  *bpp = gray;
    }
  }




  if (color==F_BWDITHER || (ncols==0 && color==F_REDUCED) ) {
    byte *thepic;  int w,h;

    /* generate a dithered image */

    if (numcols==2) {
      /* we're already viewing a 2-color image, no reason to call
	 FSDither(). */

      return NULL;
    }


    /* generate a FSDithered 1-byte per pixel image */
    /* if we're saving as an FSDithered, or we're viewing as FSDithered
       and we're saving 'reduced' */

    if (savenormCB.val) { thepic = cpic;  w = cWIDE;  h = cHIGH; }
                   else { thepic = epic;  w = eWIDE;  h = eHIGH; }

    bwpic = FSDither(thepic, PIC8, w, h, rMap,gMap,bMap, 0, 1);
    if (!bwpic) FatalError("unable to malloc dithered picture (DoSave)");

    /* put a BW colormap */
    rBW[0] = (blkRGB>>16)&0xff;     rBW[1] = (whtRGB>>16)&0xff;
    gBW[0] = (blkRGB>>8)&0xff;      gBW[1] = (whtRGB>>8)&0xff;
    bBW[0] = blkRGB&0xff;           bBW[1] =  whtRGB&0xff;
    *rpp = rBW;  *gpp = gBW;  *bpp = bBW;
    *nc = 2;
  }

  return bwpic;
}





/*************************************************************/
/*       POLLING ROUTINES                                    */
/*************************************************************/


static struct stat origStat, lastStat;
static int haveStat = 0, haveLastStat = 0, statcount = 0;

/****************************/
void InitPoll()
{
  /* called whenever a file is initially loaded.  stat's the file and puts
     the results in origStat */

  haveStat = haveLastStat = statcount = 0;

  /* only do stat() if curname is a valid index, and it's not '<stdin>' */
  if (curname>=0 && curname<numnames &&
      (strcmp(namelist[curname], STDINSTR)!=0)) {

    if (stat(namelist[curname], &origStat)==0) {
      haveStat = 1;
      if (DEBUG) fprintf(stderr," origStat.size=%ld,  origStat.mtime=%ld\n", 
			 origStat.st_size, origStat.st_mtime);
    }
  }
}


/****************************/
int CheckPoll(del)
     int del;
{
  /* returns '1' if the file has been modified, and either 
      A) the file has stabilized (st = lastStat), or
      B) 'del' iterations have gone by (roughly equal to 'del' seconds)
         since the file changed from origStat
   */

  struct stat st;

  if (haveStat && curname>=0 && curname<numnames &&
      (strcmp(namelist[curname], STDINSTR)!=0)) {

    if (stat(namelist[curname], &st)==0) {
      /* got a stat, everything's cool. */

      if (DEBUG) fprintf(stderr," st.size=%ld,  st.mtime=%ld  count=%d\n", 
			 st.st_size, st.st_mtime, statcount);

      if ((st.st_size  == origStat.st_size) &&
	  (st.st_mtime == origStat.st_mtime)) return 0;  /* no chg */

      if (haveLastStat && 
	  st.st_size  == lastStat.st_size &&
	  st.st_mtime == lastStat.st_mtime) {
	/* file has stablized, apparently */
	xvbcopy((char *) &st, (char *) &origStat, sizeof(struct stat));
	haveLastStat = statcount = 0;
	return 1;
      }
      
      if (statcount >= del) {
	/* it's been long enough since initial change */
	xvbcopy((char *) &st, (char *) &origStat, sizeof(struct stat));
	haveLastStat = statcount = 0;
	return 1;
      }

      xvbcopy((char *) &st, (char *) &lastStat, sizeof(struct stat));
      haveLastStat = 1;
      statcount++;
    }
  }

  return 0;
}


/***************************************************************/
void DIRDeletedFile(name)
     char *name;
{
  /* called when file 'name' has been deleted.  If any of the browsers
     were showing the directory that the file was in, does a rescan() */
  
  int  i;
  char buf[MAXPATHLEN + 2], *tmp;

  strcpy(buf, name);
  tmp = BaseName(buf);
  *tmp = '\0';     /* truncate after last '/' */
  
  if (strcmp(path, buf)==0) LoadCurrentDirectory();
}


/***************************************************************/
void DIRCreatedFile(name)
     char *name;
{
  DIRDeletedFile(name);
}


