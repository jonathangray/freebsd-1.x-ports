/*
 * xv.c - main section of xv.  X setup, window creation, etc.
 *
 *  Author:    John Bradley, University of Pennsylvania
 *                (bradley@cis.upenn.edu)
 *
 *  Contains:
 *            int  main(argc,argv)
 *     static void Syntax()
 *     static int  openPic(filenum)
 *     static void closePic()
 *     static void openFirstPic()
 *     static void openNextPic()
 *     static void openNextQuit()
 *     static void openNextLoop()
 *     static void openPrevPic()
 *     static void openNamedPic()
 *     static void mainLoop()
 *     static void createMainWindow(geom, name)
 *            void FixAspect(grow, w, h)
 *     static void makeDispNames()
 *            void DeleteCmd();
 *            int  rd_int(name)
 *            int  rd_str(name)
 *            int  rd_flag(name)
 *            int  rd_str_cl(name_str, class_str)
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


#define MAIN
#define NEEDSTIME
#define NEEDSDIR     /* for value of MAXPATHLEN */

#include "xv.h"
#include "bitmaps.h"

#include <X11/Xatom.h>

#ifdef VMS
extern Window pseudo_root();
#endif


/* program needs one of the following fonts.  Trys them in ascending order */

#define FONT1 "-*-lucida-medium-r-*-*-12-*"
#define FONT2 "-*-helvetica-medium-r-*-*-12-*"
#define FONT3 "-*-helvetica-medium-r-*-*-11-*"
#define FONT4 "6x13"
#define FONT5 "fixed"

/* a mono-spaced font needed for the 'pixel value tracking' feature */
#define MFONT1 "-misc-fixed-medium-r-normal-*-13-*"
#define MFONT2 "6x13"   
#define MFONT3 "-*-courier-medium-r-*-*-12-*"
#define MFONT4 "fixed"   


/* default positions for various windows */
#define DEFINFOGEOM "-10+10"       /* default position of info window */
#define DEFCTRLGEOM "+170+380"     /* default position of ctrl window */
#define DEFGAMGEOM  "+10-10"       /* default position of gamma window */
#define DEFBROWGEOM "-10-10"       /* default position of browse window */
#define DEFTEXTGEOM "-10+320"      /* default position of text window */
#define DEFCMTGEOM  "-10+300"      /* default position of comment window */



static int    revvideo   = 0;   /* true if we should reverse video */
static int    dfltkludge = 0;   /* true if we want dfltpic dithered */
static int    clearonload;      /* clear window/root (on colormap visuals) */
static int    randomShow = 0;   /* do a 'random' slideshow */
static int    startIconic = 0;  /* '-iconic' option */
static float  expand = 1.0;     /* '-expand' argument */
static char  *maingeom = NULL;
static char  *icongeom = NULL;
static Atom   __SWM_VROOT = None;

static char   basefname[128];   /* just the current fname, no path */

/* things to do upon successfully loading an image */
static int    autoraw    = 0;   /* force raw if using stdcmap */
static int    autodither = 0;   /* dither */
static int    autosmooth = 0;   /* smooth */
static int    auto4x3    = 0;   /* do a 4x3 */
static int    autorotate = 0;   /* rotate 0, +-90, +-180, +-270 */
static int    autohflip  = 0;   /* Flip Horizontally */
static int    autovflip  = 0;   /* Flip Vertically */
static int    autocrop   = 0;   /* do an 'AutoCrop' command */
static int    acrop      = 0;   /* automatically do a 'Crop' */
static int    acropX, acropY, acropW, acropH;
static int    autonorm   = 0;   /* normalize */
static int    autohisteq = 0;   /* Histogram equalization */

static int    force8     = 0;   /* force 8-bit mode */
static int    force24    = 0;   /* force 24-bit mode */

/* used in DeleteCmd() */
static char  **mainargv;
static int     mainargc;


/* local function pre-definitions */
#ifdef __STDC__
static void printoption(char *);
static void Syntax(void);
static void RmodeSyntax(void);
static int  openPic(int);
static int  readpipe(char *, char *);
static void openFirstPic(void);
static void openNextPic(void);
static void openNextQuit(void);
static void openNextLoop(void);
static void openPrevPic(void);
static void openNamedPic(void);
static int  findRandomPic(void);
static void mainLoop(void);
static void createMainWindow(char *, char *);
static void setWinIconNames(char *);
static void makeDispNames(void);
static void deleteFromList(int);
static int  argcmp(char *, char *, int);
static void add_filelist_to_namelist(char *, char **, int *, int);
#else
static void printoption();
static void Syntax(), RmodeSyntax(), openFirstPic(), openNextPic();
static void openNextQuit(), openNextLoop(), openPrevPic(), openNamedPic();
static int  findRandomPic();
static void mainLoop(), createMainWindow(), makeDispNames();
static void setWinIconNames(), deleteFromList();
static int  openPic(), argcmp(), readpipe();
static void add_filelist_to_namelist();
#endif


/*******************************************/
int main(argc, argv)
     int   argc;
     char *argv[];
/*******************************************/
{
  int   i, imap, ctrlmap, gmap, browmap, cmtmap, clrroot, nopos, limit2x;
  char *display, *whitestr, *blackstr, *histr, *lostr,
       *infogeom, *fgstr, *bgstr, *ctrlgeom, *gamgeom, *browgeom, *tmpstr;
  char *rootfgstr, *rootbgstr, *visualstr, *textgeom, *cmtgeom;
  char *monofontname, *flistName;
  int  curstype, stdinflag, browseMode, savenorm, preview, pscomp, preset, 
       rmodeset, gamset, cgamset, perfect, owncmap, rwcolor, stdcmap;
  double gamval, rgamval, ggamval, bgamval;

  XColor ecdef;
  Window rootReturn, parentReturn, *children;
  unsigned int numChildren;
  int not_in_first_half;



#ifdef VMS
  /* convert VMS-style arguments to unix names and glob */
  do_vms_wildcard(&argc, &argv);
  getredirection(&argc, &argv);
#endif


  /*****************************************************/
  /*** variable Initialization                       ***/
  /*****************************************************/

  GETWD(initdir);
  searchdir[0] = '\0';
  fullfname[0] = '\0';

  mainargv = argv;
  mainargc = argc;

  /* init internal variables */
  display = NULL;
  fgstr = bgstr = rootfgstr = rootbgstr = NULL;
  histr = lostr = whitestr = blackstr = NULL;
  visualstr = monofontname = flistName = NULL;
  winTitle = NULL;

  pic = egampic = epic = cpic = NULL;
  theImage = NULL;

  picComments = (char *) NULL;

  numPages = 1;  curPage = 0;
  pageBaseName[0] = '\0';

  LocalCmap = browCmap = 0;
  stdinflag = 0;
  autoclose = 0; 
  cmapInGam = 0;
  grabDelay = 0;
  showzoomcursor = 0;
  perfect = owncmap = stdcmap = rwcolor = 0;

  browPerfect = 1;  
  gamval = rgamval = ggamval = bgamval = 1.0;
  
  picType = -1;              /* gets set once file is loaded */
  colorMapMode = CM_NORMAL;
  haveStdCmap  = STD_NONE;
  allocMode    = AM_READONLY;
  novbrowse    = 0;


  /* default Ghostscript parameters */
  gsDev = "pbmraw";

#ifdef GS_DEV
  gsDev = GS_DEV;
#endif

  gsRes = 72;
  gsGeomStr = NULL;


  /* init default colors */
  fgstr = "#000000";  bgstr = "#B2C0DC";
  histr = "#C6D5E2";  lostr = "#8B99B5";

  cmd = rindex(argv[0],'/');
  if (!cmd) cmd = argv[0]; else cmd++;

  tmpstr = (char *) getenv("TMPDIR");
  if (!tmpstr) tmpdir = "/tmp";
  else {
    tmpdir = (char *) malloc(strlen(tmpstr) + 1);
    if (!tmpdir) FatalError("can't malloc 'tmpdir'\n");
    strcpy(tmpdir, tmpstr);
  }

  /* init command-line options flags */
  infogeom = DEFINFOGEOM;  ctrlgeom = DEFCTRLGEOM;  
  gamgeom  = DEFGAMGEOM;   browgeom = DEFBROWGEOM;
  textgeom = DEFTEXTGEOM;  cmtgeom  = DEFCMTGEOM;

  ncols = -1;  mono = 0;  
  ninstall = 0;  fixedaspect = 0;  noFreeCols = 0;
  DEBUG = 0;  bwidth = 2;
  nolimits = useroot = clrroot = noqcheck = 0;
  waitsec = -1;  waitloop = 0;  automax = 0;
  rootMode = 0;  hsvmode = 0;
  rmodeset = gamset = cgamset = 0;
  nopos = limit2x = 0;
  resetroot = 1;
  clearonload = 0;
  curstype = XC_crosshair;
  browseMode = savenorm = nostat = 0;
  preview = 0;
  pscomp = 0;
  preset = 0;
  viewonly = 0;

  kludge_offx = kludge_offy = 0;

  conv24 = CONV24_SLOW;  /* use 'slow' algorithm by default */

  defaspect = normaspect = 1.0;
  mainW = dirW = infoW = ctrlW = gamW = psW = (Window) NULL;
  anyBrowUp = 0;

#ifdef HAVE_JPEG
  jpegW = (Window) NULL;  jpegUp = 0;
#endif

#ifdef HAVE_TIFF
  tiffW = (Window) NULL;  tiffUp = 0;
#endif

  imap = ctrlmap = gmap = browmap = cmtmap = 0;

  ch_offx = ch_offy = p_offx = p_offy = 0;

  /* init info box variables */
  infoUp = 0;
  infoMode = INF_STR;
  for (i=0; i<NISTR; i++) SetISTR(i,"");

  /* init ctrl box variables */
  ctrlUp = 0;
  curname = -1;
  formatStr[0] ='\0';

  gamUp = 0;
  psUp  = 0;

  Init24to8();


  /*****************************************************/
  /*** X Resource Initialization                     ***/
  /*****************************************************/

  /* once through the argument list to find the display name
     and DEBUG level, if any */

  for (i=1; i<argc; i++) {
    if (!strncmp(argv[i],"-help",5)) { 	/* help */
      Syntax();
      exit(0);
    }

    else if (!argcmp(argv[i],"-display",4)) {
      i++;
      if (i<argc) display = argv[i];
      break;
    }

#ifdef VMS    /* in VMS, cmd-line-opts are in lower case */
    else if (!argcmp(argv[i],"-debug",3)) {
      { if (++i<argc) DEBUG = atoi(argv[i]); }
    }
#else
    else if (!argcmp(argv[i],"-DEBUG",2)) {
      { if (++i<argc) DEBUG = atoi(argv[i]); }
    }
#endif
  }

  /* open the display */
  if ( (theDisp=XOpenDisplay(display)) == NULL) {
    fprintf(stderr, "%s: Can't open display\n",argv[0]);
    exit(1);
  }



  if (rd_str ("aspect")) {
    int n,d;
    if (sscanf(def_str,"%d:%d",&n,&d)!=2 || n<1 || d<1)
      fprintf(stderr,"%s: unable to parse 'aspect' resource\n",cmd);
    else defaspect = (float) n / (float) d;
  }
      
  if (rd_flag("2xlimit"))        limit2x     = def_int;      
  if (rd_flag("auto4x3"))        auto4x3     = def_int;
  if (rd_flag("autoClose"))      autoclose   = def_int;
  if (rd_flag("autoCrop"))       autocrop    = def_int;
  if (rd_flag("autoDither"))     autodither  = def_int;
  if (rd_flag("autoHFlip"))      autohflip   = def_int;
  if (rd_flag("autoHistEq"))     autohisteq  = def_int;
  if (rd_flag("autoNorm"))       autonorm    = def_int;
  if (rd_flag("autoRaw"))        autoraw     = def_int;
  if (rd_int ("autoRotate"))     autorotate  = def_int;
  if (rd_flag("autoSmooth"))     autosmooth  = def_int;
  if (rd_flag("autoVFlip"))      autovflip   = def_int;
  if (rd_str ("background"))     bgstr       = def_str;
  if (rd_flag("best24") && def_int)  conv24  = CONV24_BEST;
  if (rd_str ("black"))          blackstr    = def_str;
  if (rd_int ("borderWidth"))    bwidth      = def_int;
  if (rd_str ("ceditGeometry"))  gamgeom     = def_str;
  if (rd_flag("ceditMap"))       gmap        = def_int;
  if (rd_flag("ceditColorMap"))  cmapInGam   = def_int;
  if (rd_flag("clearOnLoad"))    clearonload = def_int;
  if (rd_str ("commentGeometry")) cmtgeom    = def_str;
  if (rd_flag("commentMap"))     cmtmap      = def_int;
  if (rd_str ("ctrlGeometry"))   ctrlgeom    = def_str;
  if (rd_flag("ctrlMap"))        ctrlmap     = def_int;
  if (rd_int ("cursor"))         curstype    = def_int;
  if (rd_int ("defaultPreset"))  preset      = def_int;

  if (rd_str ("driftKludge")) {
    if (sscanf(def_str,"%d %d", &kludge_offx, &kludge_offy) != 2) {
      kludge_offx = kludge_offy = 0;
    }
  }

  if (rd_str ("expand"))         expand      = atof(def_str);
  if (rd_str ("fileList"))       flistName   = def_str;
  if (rd_flag("fixed"))          fixedaspect = def_int;
  if (rd_flag("force8"))         force8      = def_int;
  if (rd_flag("force24"))        force24     = def_int;
  if (rd_str ("foreground"))     fgstr       = def_str;
  if (rd_str ("geometry"))       maingeom    = def_str;
  if (rd_str ("gsDevice"))       gsDev       = def_str;
  if (rd_str ("gsGeometry"))     gsGeomStr   = def_str;
  if (rd_int ("gsResolution"))   gsRes       = def_int;
  if (rd_flag("hsvMode"))        hsvmode     = def_int;
  if (rd_str ("highlight"))      histr       = def_str;
  if (rd_str ("iconGeometry"))   icongeom    = def_str;
  if (rd_flag("iconic"))         startIconic = def_int;
  if (rd_str ("infoGeometry"))   infogeom    = def_str;
  if (rd_flag("infoMap"))        imap        = def_int;
  if (rd_flag("loadBrowse"))     browseMode  = def_int;
  if (rd_str ("lowlight"))       lostr       = def_str;
  if (rd_flag("mono"))           mono        = def_int;
  if (rd_str ("monofont"))       monofontname = def_str;
  if (rd_int ("ncols"))          ncols       = def_int;
  if (rd_flag("ninstall"))       ninstall    = def_int;
  if (rd_flag("nolimits"))       nolimits    = def_int;
  if (rd_flag("nopos"))          nopos       = def_int;
  if (rd_flag("noqcheck"))       noqcheck    = def_int;
  if (rd_flag("nostat"))         nostat      = def_int;
  if (rd_flag("ownCmap"))        owncmap     = def_int;
  if (rd_flag("perfect"))        perfect     = def_int;
  if (rd_flag("pscompress"))     pscomp      = def_int;
  if (rd_flag("pspreview"))      preview     = def_int;
  if (rd_flag("quick24") && def_int)  conv24 = CONV24_FAST;
  if (rd_flag("resetroot"))      resetroot   = def_int;
  if (rd_flag("reverse"))        revvideo    = def_int;
  if (rd_str ("rootBackground")) rootbgstr   = def_str;
  if (rd_str ("rootForeground")) rootfgstr   = def_str;
  if (rd_int ("rootMode"))       { rootMode    = def_int;  rmodeset++; }
  if (rd_flag("rwColor"))        rwcolor     = def_int;
  if (rd_flag("saveNormal"))     savenorm    = def_int;
  if (rd_str ("searchDirectory"))  strcpy(searchdir, def_str);
  if (rd_str ("textviewGeometry")) textgeom  = def_str;
  if (rd_flag("useStdCmap"))     stdcmap     = def_int;
  if (rd_str ("visual"))         visualstr   = def_str;
  if (rd_flag("vsDisable"))      novbrowse   = def_int;
  if (rd_str ("vsGeometry"))     browgeom    = def_str;
  if (rd_flag("vsMap"))          browmap     = def_int;
  if (rd_flag("vsPerfect"))      browPerfect = def_int;
  if (rd_str ("white"))          whitestr    = def_str;
      

  /*****************************************************/
  /*** Command Line Options                          ***/
  /*****************************************************/
  
  for (i=1, numnames=0; i<argc; i++) {
    not_in_first_half = 0;

    if (argv[i][0] != '-') {   		/* a file name.  put it in list */
      if (numnames<MAXNAMES) {
	namelist[numnames++] = argv[i];
	if (numnames==MAXNAMES) {
	  fprintf(stderr,"%s: too many filenames.  Only using first %d.\n",
		  cmd, MAXNAMES);
	}
      }
    }

    else if (!strcmp(argv[i],  "-"))           /* stdin flag */
      stdinflag++;

    else if (!argcmp(argv[i],"-24",3))         /* force24 */
      force24 = !force24;

    else if (!argcmp(argv[i],"-2xlimit",3))    /* 2xlimit */
      limit2x = !limit2x;

    else if (!argcmp(argv[i],"-4x3",2))        /* 4x3 */
      auto4x3 = !auto4x3;

    else if (!argcmp(argv[i],"-8",2))          /* force8 */
      force8 = !force8;

    else if (!argcmp(argv[i],"-acrop",3))      /* autocrop */
      autocrop = !autocrop;

    else if (!argcmp(argv[i],"-aspect",3)) {   /* default aspect */
      int n,d;
      if (++i<argc) {
	if (sscanf(argv[i],"%d:%d",&n,&d)!=2 || n<1 || d<1)
	  fprintf(stderr,"%s: bad aspect ratio '%s'\n",cmd,argv[i]);
	else defaspect = (float) n / (float) d;
      }
    }

    else if (!argcmp(argv[i],"-best24",3))     /* ppmquant 24->8 conv. */
      conv24 = CONV24_BEST;
    
    else if (!argcmp(argv[i],"-bg",3))        /* background color */
      { if (++i<argc) bgstr = argv[i]; }

    else if (!argcmp(argv[i],"-black",3))     /* black color */
      { if (++i<argc) blackstr = argv[i]; }
    
    else if (!argcmp(argv[i],"-bw",3))        /* border width */
      { if (++i<argc) bwidth=atoi(argv[i]); }

    else if (!argcmp(argv[i],"-cecmap",4))	/* cmapInGam */
      cmapInGam = !cmapInGam;
    
    else if (!argcmp(argv[i],"-cegeometry",4))	/* gammageom */
      { if (++i<argc) gamgeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-cemap",4))	/* gmap */
      gmap = !gmap;
    
    else if (!argcmp(argv[i],"-cgamma",4))	/* color gamma */
      { if (i+3<argc) {
	  rgamval = atof(argv[++i]); 
	  ggamval = atof(argv[++i]); 
	  bgamval = atof(argv[++i]); 
	}
	cgamset++;
      }
    
    else if (!argcmp(argv[i],"-cgeometry",4))	/* ctrlgeom */
      { if (++i<argc) ctrlgeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-clear",4))	/* clear */
      clrroot = !clrroot;

    else if (!argcmp(argv[i],"-close",4))	/* close */
      autoclose = !autoclose;
    
    else if (!argcmp(argv[i],"-cmap",3))	/* ctrlmap */
      ctrlmap = !ctrlmap;
    
    else if (!argcmp(argv[i],"-cmtgeometry",5))	/* comment geom */
      { if (++i<argc) cmtgeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-cmtmap",5))	/* map comment window */
      cmtmap = !cmtmap;
    
    else if (!argcmp(argv[i],"-crop",3))	/* crop */
      { if (i+4<argc) {
	  acropX = atoi(argv[++i]); 
	  acropY = atoi(argv[++i]); 
	  acropW = atoi(argv[++i]); 
	  acropH = atoi(argv[++i]); 
	}
	acrop++;
      }
    
    else if (!argcmp(argv[i],"-cursor",3))	/* cursor */
      { if (++i<argc) curstype = atoi(argv[i]); }

#ifdef VMS    /* in VMS, cmd-line-opts are in lower case */
    else if (!argcmp(argv[i],"-debug",3)) {
      { if (++i<argc) DEBUG = atoi(argv[i]); }
    }
#else
    else if (!argcmp(argv[i],"-DEBUG",2)) {
      { if (++i<argc) DEBUG = atoi(argv[i]); }
    }
#endif

    else if (!argcmp(argv[i],"-dir",4))         /* search directory */
      { if (++i<argc) strcpy(searchdir, argv[i]); }

    else if (!argcmp(argv[i],"-display",4))     /* display */
      { if (++i<argc) display = argv[i]; }

    else if (!argcmp(argv[i],"-dither",4))      /* autodither */
      autodither = !autodither;

    else if (!argcmp(argv[i],"-drift",3)) {     /* drift kludge */
      if (i<argc-2) {
	kludge_offx = atoi(argv[++i]);
	kludge_offy = atoi(argv[++i]);
      }
    }

    else if (!argcmp(argv[i],"-expand",2)) 	/* expand factor */
      { if (++i<argc) expand=atof(argv[i]); }

    else if (!argcmp(argv[i],"-fg",3))          /* foreground color */
      { if (++i<argc) fgstr = argv[i]; }
    
    else if (!argcmp(argv[i],"-fixed",3))       /* fixed aspect ratio */
      fixedaspect = !fixedaspect;
    
    else if (!argcmp(argv[i],"-flist",3))       /* file list */
      { if (++i<argc) flistName = argv[i]; }

    else if (!argcmp(argv[i],"-gamma",3))	/* gamma */
      { if (++i<argc) gamval = atof(argv[i]);  gamset++; }
    
    else if (!argcmp(argv[i],"-geometry",3))	/* geometry */
      { if (++i<argc) maingeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-grabdelay",3))	/* grabDelay */
      { if (++i<argc) grabDelay = atoi(argv[i]); }
    
    else if (!argcmp(argv[i],"-gsdev",4))	/* gsDevice */
      { if (++i<argc) gsDev = argv[i]; }
    
    else if (!argcmp(argv[i],"-gsgeom",4))	/* gsGeometry */
      { if (++i<argc) gsGeomStr = argv[i]; }
    
    else if (!argcmp(argv[i],"-gsres",4))       /* gsResolution */
      { if (++i<argc) gsRes=abs(atoi(argv[i])); }
    
    else if (!argcmp(argv[i],"-hflip",3))       /* hflip */
      autohflip = !autohflip;
    
    else if (!argcmp(argv[i],"-hist",4))        /* hist eq */
      autohisteq = !autohisteq;
    
    else if (!argcmp(argv[i],"-hi",3))	        /* highlight */
      { if (++i<argc) histr = argv[i]; }
    
    else if (!argcmp(argv[i],"-hsv",3))         /* hsvmode */
      hsvmode = 1;
    
    else if (!argcmp(argv[i],"-iconic",4))	/* iconic */
      startIconic = !startIconic;
    
    else if (!argcmp(argv[i],"-icgeometry",4))	/* icon geometry */
      { if (++i<argc) icongeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-igeometry",3))	/* infogeom */
      { if (++i<argc) infogeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-imap",3))	/* imap */
      imap = !imap;
    
    else if (!argcmp(argv[i],"-lbrowse",3))    /* loadbrowse mode */
      browseMode = !browseMode;

    else if (!argcmp(argv[i],"-loadclear",4))   /* toggle clearonload */
      clearonload = !clearonload;

    else if (!argcmp(argv[i],"-lo",3))	        /* lowlight */
      { if (++i<argc) lostr = argv[i]; }

    
    else not_in_first_half = 1;     



    /* split huge else-if group into two halves, as it breaks some compilers */



    if (!argcmp(argv[i],"-max",4))	        /* auto maximize */
      automax = !automax;

    else if (!argcmp(argv[i],"-maxpect",5))     /* auto maximize */
      { automax++; fixedaspect++; }
    
    else if (!argcmp(argv[i],"-mfn",3))         /* mono font name */
      { if (++i<argc) monofontname = argv[i]; }

    else if (!argcmp(argv[i],"-mono",3))	/* mono */
      mono = !mono;
    
    else if (!argcmp(argv[i],"-name",3))        /* name */
      { if (++i<argc) winTitle = argv[i]; }
    
    else if (!argcmp(argv[i],"-ncols",3))       /* ncols */
      { if (++i<argc) ncols=abs(atoi(argv[i])); }
    
    else if (!argcmp(argv[i],"-ninstall",3))	/* don't install colormaps */
      ninstall = !ninstall;

    else if (!argcmp(argv[i],"-nofreecols",4))	/* don't install colormaps */
      noFreeCols = !noFreeCols;

    else if (!argcmp(argv[i],"-nolimits",4))    /* nolimits */
      nolimits = !nolimits;

    else if (!argcmp(argv[i],"-nopos",4))       /* nopos */
      nopos = !nopos;

    else if (!argcmp(argv[i],"-noqcheck",4))    /* noqcheck */
      noqcheck = !noqcheck;

    else if (!argcmp(argv[i],"-noresetroot",5)) /* clrroot in window mode */
      resetroot = !resetroot;  
    
    else if (!argcmp(argv[i],"-norm",5))        /* norm */
      autonorm = !autonorm;

    else if (!argcmp(argv[i],"-nostat",4))      /* nostat */
      nostat = !nostat;

    else if (!argcmp(argv[i],"-owncmap",2))     /* own colormap */
      owncmap = !owncmap;

    else if (!argcmp(argv[i],"-perfect",3))     /* perfect colors */
      perfect = !perfect;  

    else if (!argcmp(argv[i],"-poll",3))        /* check for file mod. */
      polling = !polling;

    else if (!argcmp(argv[i],"-preset",3))      /* preset */
      { if (++i<argc) preset=abs(atoi(argv[i])); }
    
    else if (!argcmp(argv[i],"-quick24",5))     /* quick 24-to-8 conv */
      conv24 = CONV24_FAST;
    
    else if (!argcmp(argv[i],"-quit",2))        /* auto-quit if -root */
      autoquit = !autoquit;

    else if (!argcmp(argv[i],"-random", 4))     /* random slide show */
      randomShow = !randomShow;

    else if (!argcmp(argv[i],"-raw", 4))        /* force raw */
      autoraw = !autoraw;

    else if (!argcmp(argv[i],"-rbg",3))         /* root background color */
      { if (++i<argc) rootbgstr = argv[i]; }
    
    else if (!argcmp(argv[i],"-rfg",3))         /* root foreground color */
      { if (++i<argc) rootfgstr = argv[i]; }
    
    else if (!argcmp(argv[i],"-rgb",4))         /* rgb mode */
      hsvmode = 0;
    
    else if (!argcmp(argv[i],"-rmode",3))	/* root pattern */
      { if (++i<argc) rootMode = atoi(argv[i]); 
	useroot++;  rmodeset++;
      }
    
    else if (!argcmp(argv[i],"-root",4))        /* use root window */
      { if (!rmodeset) useroot = !useroot; }
    
    else if (!argcmp(argv[i],"-rotate",4))      /* rotate */
      { if (++i<argc) autorotate = atoi(argv[i]); }
    
    else if (!argcmp(argv[i],"-rv",3))          /* reverse video */
      revvideo = !revvideo;
    
    else if (!argcmp(argv[i],"-rw",3))          /* use r/w color */
      rwcolor = !rwcolor;
    
    else if (!argcmp(argv[i],"-slow24",3))      /* slow 24-to-8 conversion */
      conv24 = CONV24_SLOW;
    
    else if (!argcmp(argv[i],"-smooth",3))      /* autosmooth */
      autosmooth = !autosmooth;

    else if (!argcmp(argv[i],"-stdcmap",3))     /* use stdcmap */
      stdcmap = !stdcmap;

    else if (!argcmp(argv[i],"-tgeometry",2))	/* text viewer geom */
      { if (++i<argc) textgeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-vflip",3))	/* vflip */
      autovflip = !autovflip;

    else if (!argcmp(argv[i],"-viewonly",4))    /* viewonly */
      viewonly = !viewonly;

    else if (!argcmp(argv[i],"-visual",4))	/* visual */
      { if (++i<argc) visualstr = argv[i]; }
    
    else if (!argcmp(argv[i],"-vsdisable",4))   /* disable visual schnauzer */
      novbrowse = !novbrowse;
    
    else if (!argcmp(argv[i],"-vsgeometry",4))	/* visSchnauzer geom */
      { if (++i<argc) browgeom = argv[i]; }
    
    else if (!argcmp(argv[i],"-vsmap",4))	/* visSchnauzer map */
      browmap = !browmap;
    
    else if (!argcmp(argv[i],"-vsperfect",3))	/* visSchanuzer perfect */
      browPerfect = !browPerfect;

    else if (!argcmp(argv[i],"-wait",3)) {      /* secs to wait betwn pics */
      if (++i<argc) {
	waitsec = abs(atoi(argv[i]));
	if (waitsec<0) waitsec = 0;
      }
    }
    
    else if (!argcmp(argv[i],"-white",3))	/* white color */
      { if (++i<argc) whitestr = argv[i]; }
    
    else if (!argcmp(argv[i],"-wloop",3))	/* waitloop */
      waitloop = !waitloop;
    
    else if (not_in_first_half) Syntax();
  }



  /*****************************************************************/
  /*            check cmd-line options for validity                */
  /*****************************************************************/

  if (strlen(searchdir)) {  /* got a search directory */
    if (chdir(searchdir)) {
      fprintf(stderr,"xv: unable to cd to directory '%s'.\n",searchdir);
      fprintf(stderr,
       "    Ignoring '-dir' option and/or 'xv.searchDirectory' resource\n");
      searchdir[0] = '\0';
    }
  }


  if (flistName) add_filelist_to_namelist(flistName, namelist, &numnames, 
					  MAXNAMES);

  RANGE(curstype,0,254);
  curstype = curstype & 0xfe;   /* clear low bit to make curstype even */

  if (expand == 0.0) Syntax();
  if (rootMode < 0 || rootMode > RM_MAX) RmodeSyntax();

  if (DEBUG) XSynchronize(theDisp, True);

  /* if using root, generally gotta map ctrl window, 'cause there won't be
     any way to ask for it.  (no kbd or mouse events from rootW) */
  if (useroot && !autoquit) ctrlmap = -1;    

  
  if (abs(autorotate) !=   0 && abs(autorotate) != 90 &&
      abs(autorotate) != 180 && abs(autorotate) != 270) {
    fprintf(stderr,"Invalid auto rotation value (%d) ignored.\n", autorotate);
    fprintf(stderr,"  (Valid values:  0, +-90, +-180, +-270)\n");

    autorotate = 0;
  } 


  if (grabDelay < 0 || grabDelay > 15) {
    fprintf(stderr,
        "Invalid '-grabdelay' value ignored.  Valid range is 0-15 seconds.\n");
    grabDelay = 0;
  }

  if (preset<0 || preset>4) {
    fprintf(stderr,"Invalid default preset value (%d) ignored.\n", preset);
    fprintf(stderr,"  (Valid values:  1, 2, 3, 4)\n");

    preset = 0;
  } 

  if (waitsec < 0) noFreeCols = 0;   /* disallow nfc if not doing slideshow */
  if (noFreeCols && perfect) { perfect = 0;  owncmap = 1; }

  /* decide what default color allocation stuff we've settled on */
  if (rwcolor) allocMode = AM_READWRITE;

  if (perfect) colorMapMode = CM_PERFECT;
  if (owncmap) colorMapMode = CM_OWNCMAP;
  if (stdcmap) colorMapMode = CM_STDCMAP;

  defaultCmapMode = colorMapMode;  /* default mode for 8-bit images */

  if (nopos) { 
    maingeom = infogeom = ctrlgeom = gamgeom = browgeom = textgeom = NULL;
    cmtgeom = NULL;
  }

  /* if -root and -maxp, disallow 'integer' tiling modes */
  if (useroot && fixedaspect && automax && !rmodeset && 
      (rootMode == RM_TILE || rootMode == RM_IMIRROR))
    rootMode = RM_CSOLID;




  /*****************************************************/
  /*** X Setup                                       ***/
  /*****************************************************/
  
  theScreen = DefaultScreen(theDisp);
  theCmap   = DefaultColormap(theDisp, theScreen);
  rootW     = RootWindow(theDisp,theScreen);
  theGC     = DefaultGC(theDisp,theScreen);
  theVisual = DefaultVisual(theDisp,theScreen);
  ncells    = DisplayCells(theDisp, theScreen);
  dispDEEP  = DisplayPlanes(theDisp,theScreen);
  vrWIDE = dispWIDE  = DisplayWidth(theDisp,theScreen);
  vrHIGH = dispHIGH  = DisplayHeight(theDisp,theScreen);
  maxWIDE = dispWIDE;  maxHIGH = dispHIGH;

  if (visualstr) {     /* handle non-default visual */
    int vclass = -1;
    int vid = -1;

    lower_str(visualstr);
    if      (!strcmp(visualstr,"staticgray"))  vclass = StaticGray;
    else if (!strcmp(visualstr,"staticcolor")) vclass = StaticColor;
    else if (!strcmp(visualstr,"truecolor"))   vclass = TrueColor;
    else if (!strcmp(visualstr,"grayscale"))   vclass = GrayScale;
    else if (!strcmp(visualstr,"pseudocolor")) vclass = PseudoColor;
    else if (!strcmp(visualstr,"directcolor")) vclass = DirectColor;

    else if (!strncmp(visualstr,"0x",2)) {  /* specified visual id */
      if (sscanf(visualstr, "0x%x", &vid) != 1) vid = -1;
    }

    else {
      fprintf(stderr,"%s: Unrecognized visual type '%s'.  %s\n",
	      cmd, visualstr, "Using server default.");
    }

    if (vclass >= 0 || vid >= 0) {   /* try to find asked-for visual type */
      XVisualInfo *vinfo, rvinfo;
      long vinfomask;
      int numvis, best;

      if (vclass >= 0) { rvinfo.class = vclass;  vinfomask = VisualClassMask; }
                  else { rvinfo.visualid = vid;  vinfomask = VisualIDMask; }

      vinfo = XGetVisualInfo(theDisp, vinfomask, &rvinfo, &numvis);

      if (vinfo) {       /* choose the 'best' one, if multiple */
	for (i=0, best=0; i<numvis; i++) {
	  if (vinfo[i].depth > vinfo[best].depth) best = i;
	}
	theVisual = vinfo[best].visual;
	if (DEBUG) {
	  fprintf(stderr,"%s: using %s visual, depth = %d, screen = %d\n",
		  cmd, visualstr, vinfo[best].depth, vinfo[best].screen);
	  fprintf(stderr,"\tmasks: (0x%x,0x%x,0x%x), bits_per_rgb=%d\n",
		  vinfo[best].red_mask, vinfo[best].green_mask,
		  vinfo[best].blue_mask, vinfo[best].bits_per_rgb);
	}

	dispDEEP = vinfo[best].depth;
	theScreen = vinfo[best].screen;
	rootW = RootWindow(theDisp, theScreen);
	ncells = vinfo[best].colormap_size;
	theCmap = XCreateColormap(theDisp, rootW, theVisual, AllocNone);

	{
	  /* create a temporary window using this visual so we can
	     create a GC for this visual */

	  Window win;  
	  XSetWindowAttributes xswa;
	  XGCValues xgcv;
	  unsigned long xswamask;

	  XFlush(theDisp);
	  XSync(theDisp, False);

	  xswa.background_pixel = 0;
	  xswa.border_pixel     = 1;
	  xswa.colormap         = theCmap;
	  xswamask = CWBackPixel | CWBorderPixel | CWColormap;

	  win = XCreateWindow(theDisp, rootW, 0, 0, 100, 100, 2, dispDEEP,
			      InputOutput, theVisual, xswamask, &xswa);

	  XFlush(theDisp);
	  XSync(theDisp, False);

	  theGC = XCreateGC(theDisp, win, 0L, &xgcv);
	  /* DefaultGC(theDisp, theScreen); */

	  XDestroyWindow(theDisp, win);
	}

	vrWIDE = dispWIDE  = DisplayWidth(theDisp,theScreen);
	vrHIGH = dispHIGH  = DisplayHeight(theDisp,theScreen);
	maxWIDE = dispWIDE;  maxHIGH = dispHIGH;

	XFree((char *) vinfo);
      }
      else fprintf(stderr,"%s: Visual type '%s' not available.  %s\n",
		   cmd, visualstr, "Using server default.");
    }
  }
    

  /* turn GraphicsExposures OFF in the default GC */
  {
    XGCValues xgcv;
    xgcv.graphics_exposures = False;
    XChangeGC(theDisp, theGC, GCGraphicsExposures, &xgcv);
  }


  if (!useroot && limit2x) { maxWIDE *= 2;  maxHIGH *= 2; }
  if (nolimits) { maxWIDE = 65000; maxHIGH = 65000; }

  XSetErrorHandler(xvErrorHandler);

  /* always search for virtual root window */
  vrootW = rootW;
#ifndef VMS
  __SWM_VROOT = XInternAtom(theDisp, "__SWM_VROOT", False);
  XQueryTree(theDisp, rootW, &rootReturn, &parentReturn, &children,
	     &numChildren);
  for (i = 0; i < numChildren; i++) {
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytesafter;
    Window *newRoot = NULL;
    XWindowAttributes xwa;
    if (XGetWindowProperty (theDisp, children[i], __SWM_VROOT, 0, 1,
	  False, XA_WINDOW, &actual_type, &actual_format, &nitems,
	  &bytesafter, (unsigned char **) &newRoot) == Success && newRoot) {
      vrootW = *newRoot;
      XGetWindowAttributes(theDisp, vrootW, &xwa);
      vrWIDE = xwa.width;  vrHIGH = xwa.height;
      dispDEEP = xwa.depth;
      break;
    }
  }
#else  /* VMS */
  vrootW = pseudo_root(theDisp, theScreen);
#endif




  if (clrroot || useroot) {
    /* have enough info to do a '-clear' now */
    KillOldRootInfo();   /* if any */
    if (resetroot || clrroot) ClearRoot();  /* don't clear on '-noresetroot' */
    if (clrroot) Quit(0);
  }


  arrow     = XCreateFontCursor(theDisp,XC_top_left_arrow);
  cross     = XCreateFontCursor(theDisp,curstype);
  tcross    = XCreateFontCursor(theDisp,XC_tcross);
  zoom      = XCreateFontCursor(theDisp,XC_sizing);

  {
    XColor fc, bc;
    fc.red = fc.green = fc.blue = 0xffff;
    bc.red = bc.green = bc.blue = 0x0000;
    
    XRecolorCursor(theDisp, zoom, &fc, &bc);
  }

  { /* create invisible cursor */
    Pixmap pix;
    static char bits[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    XColor col;

    col.red = col.green = col.blue = 0;

    pix = XCreateBitmapFromData(theDisp, rootW, bits, 8, 8);
    inviso = XCreatePixmapCursor(theDisp, pix, pix, &col, &col, 0,0);
    XFreePixmap(theDisp, pix);
  }



  /* set up white,black colors */
  white = WhitePixel(theDisp,theScreen);  whtRGB = 0xffffff;
  black = BlackPixel(theDisp,theScreen);  blkRGB = 0x000000;

  if (whitestr && XParseColor(theDisp, theCmap, whitestr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef))  {
    white = ecdef.pixel;
    whtRGB = ((ecdef.red>>8)<<16) | (ecdef.green&0xff00) | (ecdef.blue>>8);
  }

  if (blackstr && XParseColor(theDisp, theCmap, blackstr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef))  {
    black = ecdef.pixel;
    blkRGB = ((ecdef.red>>8)<<16) | (ecdef.green&0xff00) | (ecdef.blue>>8);
  }


  /* set up fg,bg colors */
  fg = black;   bg = white;  
  if (fgstr && XParseColor(theDisp, theCmap, fgstr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef)) {
    fg = ecdef.pixel;
  }

  if (bgstr && XParseColor(theDisp, theCmap, bgstr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef))  {
    bg = ecdef.pixel;
  }


  /* set up root fg,bg colors */
  rootfg = white;   rootbg = black;
  if (rootfgstr && XParseColor(theDisp, theCmap, rootfgstr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef))  rootfg = ecdef.pixel;
  if (rootbgstr && XParseColor(theDisp, theCmap, rootbgstr, &ecdef) &&
      xvAllocColor(theDisp, theCmap, &ecdef))  rootbg = ecdef.pixel;


  /* set up hi/lo colors */
  i=0;
  if (dispDEEP > 1) {   /* only if we're on a reasonable display */
    if (histr && XParseColor(theDisp, theCmap, histr, &ecdef) &&
	xvAllocColor(theDisp, theCmap, &ecdef))  { hicol = ecdef.pixel; i|=1; }
    if (lostr && XParseColor(theDisp, theCmap, lostr, &ecdef) &&
	xvAllocColor(theDisp, theCmap, &ecdef))  { locol = ecdef.pixel; i|=2; }
  }

  if      (i==0) ctrlColor = 0;
  else if (i==3) ctrlColor = 1;
  else {  /* only got some of them */
    if (i&1) xvFreeColors(theDisp, theCmap, &hicol, 1, 0L);
    if (i&2) xvFreeColors(theDisp, theCmap, &locol, 1, 0L);
    ctrlColor = 0;
  }

  if (!ctrlColor) { hicol = bg;  locol = fg; }

  XSetForeground(theDisp,theGC,fg);
  XSetBackground(theDisp,theGC,bg);

  infofg = fg;  infobg = bg;

  /* if '-mono' not forced, determine if we're on a grey or color monitor */
  if (!mono) {
    if (DEBUG) fprintf(stderr,"%s: VisualClass = %d\n",cmd, theVisual->class);
    if (theVisual->class == StaticGray || theVisual->class == GrayScale)
      mono = 1;
  }
  


  iconPix = XCreatePixmapFromBitmapData(theDisp, rootW, icon_bits,
	     icon_width, icon_height, 1, 0, 1);

  iconmask = XCreatePixmapFromBitmapData(theDisp, rootW, iconmask_bits,
	     icon_width, icon_height, 1, 0, 1);


  gray50Tile = XCreatePixmapFromBitmapData(theDisp, rootW, cboard50_bits,
				cboard50_width, cboard50_height, 
				infofg, infobg, dispDEEP);
  if (!gray50Tile) FatalError("Unable to create gray50Tile bitmap\n");

  gray25Tile = XCreatePixmapFromBitmapData(theDisp, rootW, gray25_bits,
				gray25_width, gray25_height, 
				infofg, infobg, dispDEEP);
  if (!gray25Tile) FatalError("Unable to create gray25Tile bitmap\n");


  /* try to load fonts */
  if ( (mfinfo = XLoadQueryFont(theDisp,FONT1))==NULL && 
       (mfinfo = XLoadQueryFont(theDisp,FONT2))==NULL && 
       (mfinfo = XLoadQueryFont(theDisp,FONT3))==NULL && 
       (mfinfo = XLoadQueryFont(theDisp,FONT4))==NULL && 
       (mfinfo = XLoadQueryFont(theDisp,FONT5))==NULL) {
    sprintf(str,
	    "couldn't open the following fonts:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s",
	    FONT1, FONT2, FONT3, FONT4, FONT5);
    FatalError(str);
  }
  mfont=mfinfo->fid;
  XSetFont(theDisp,theGC,mfont);

  monofinfo = (XFontStruct *) NULL;

  if (monofontname) {
    monofinfo = XLoadQueryFont(theDisp, monofontname);
    if (!monofinfo) fprintf(stderr,"xv: unable to load font '%s'\n", 
			    monofontname);
  }    

  if (!monofinfo) {
    if ((monofinfo = XLoadQueryFont(theDisp,MFONT1))==NULL && 
	(monofinfo = XLoadQueryFont(theDisp,MFONT2))==NULL && 
	(monofinfo = XLoadQueryFont(theDisp,MFONT3))==NULL && 
	(monofinfo = XLoadQueryFont(theDisp,MFONT4))==NULL) {
      sprintf(str,"couldn't open %s fonts:\n\t%s\n\t%s\n\t%s\n\t%s",
	      "any of the following",
	      MFONT1, MFONT2, MFONT3, MFONT4);
      FatalError(str);
    }
  }

  monofont=monofinfo->fid;
  



  /* if ncols wasn't set, set it to 2^dispDEEP, unless dispDEEP=1, in which
     case ncols = 0;  (ncols = max number of colors allocated.  on 1-bit
     displays, no colors are allocated */

  if (ncols == -1) {
    if (dispDEEP>1) ncols = 1 << ((dispDEEP>8) ? 8 : dispDEEP);
    else ncols = 0;
  }
  else if (ncols>256) ncols = 256;       /* so program doesn't blow up */


  GenerateFSGamma();  /* has to be done before 'OpenBrowse()' is called */



  /* no filenames.  build one-name (stdio) list (if stdinflag) */
  if (numnames==0) {
    if (stdinflag) {  
      /* have to malloc namelist[0] so we can free it in deleteFromList() */
      namelist[0] = (char *) malloc(strlen(STDINSTR) + 1);
      if (!namelist[0]) FatalError("unable to to build namelist[0]");
      strcpy(namelist[0], STDINSTR);
      numnames = 1;
    }
    else namelist[0] = NULL;
  }

  if (numnames) makeDispNames();


  if (viewonly) { 
    imap = ctrlmap = gmap = browmap = cmtmap = 0; 
    novbrowse = 1;
  }


  /* create the info box window */
  CreateInfo(infogeom);
  XSelectInput(theDisp, infoW, ExposureMask | ButtonPressMask | KeyPressMask
	       | StructureNotifyMask);
  InfoBox(imap);     /* map it (or not) */
  if (imap) {
    RedrawInfo(0,0,INFOWIDE,INFOHIGH);  /* explicit draw if mapped */
    XFlush(theDisp);
  }


  /* create the control box window */
  CreateCtrl(ctrlgeom);
  epicMode = EM_RAW;   SetEpicMode();

  XSelectInput(theDisp, ctrlW, ExposureMask | ButtonPressMask | KeyPressMask
	       | StructureNotifyMask);
  if (ctrlmap < 0) {    /* map iconified */
    XWMHints xwmh;
    xwmh.initial_state = IconicState;
    xwmh.flags = StateHint;
    XSetWMHints(theDisp, ctrlW, &xwmh);
    ctrlmap = 1;
  }
  CtrlBox(ctrlmap);     /* map it (or not) */
  if (ctrlmap) {
    RedrawCtrl(0,0,CTRLWIDE,CTRLHIGH);   /* explicit draw if mapped */
    XFlush(theDisp);
  }



  {     /**** FIX dispnames array! ****/
    int i,j;   char *tmp;

    for (i=j=0; i<numnames; i++) {
      char *dname;

      dname = dispnames[i];
      if (StringWidth(dname) > (nList.w-10-16)) {  /* have to trunc. */
	tmp = dname;
	while (1) {
	  tmp = (char *) strchr(tmp,'/'); /* find next '/' in filename */
	  if (!tmp) { tmp = dname;  break; }
	
	  tmp++;                   /* move to char following the '/' */
	  if (StringWidth(tmp) <= (nList.w-10-16)) { /* is cool now */
	    j++;  break;
	  }
	}
	dispnames[i] = tmp;
      }
    }

    if (j) ChangedCtrlList();
  }





  /* create the directory window */
  CreateDirW(NULL);
  XSelectInput(theDisp, dirW, ExposureMask | ButtonPressMask | KeyPressMask);
  browseCB.val = browseMode;
  savenormCB.val = savenorm;

  /* create the gamma window */
  CreateGam(gamgeom, (gamset) ? gamval : -1.0,
	             (cgamset) ? rgamval : -1.0,
	             (cgamset) ? ggamval : -1.0,
	             (cgamset) ? bgamval : -1.0,
	             preset);
  XSelectInput(theDisp, gamW, ExposureMask | ButtonPressMask | KeyPressMask
	       | StructureNotifyMask
	       | (cmapInGam ? ColormapChangeMask : 0));

  GamBox(gmap);     /* map it (or not) */



  stdnfcols = 0;   /* so we don't try to free any if we don't create any */

  if (!novbrowse) {
    MakeBrowCmap();
    /* create the visual browser window */
    CreateBrowse(browgeom, fgstr, bgstr, histr, lostr);

    if (browmap) OpenBrowse();
  }
  else BTSetActive(&but[BVBROWSE], 0);   /* disable visual browser */


  CreateTextWins(textgeom, cmtgeom);
  if (cmtmap) OpenCommentText();


  /* create the ps window */
  CreatePSD(NULL);
  XSetTransientForHint(theDisp, psW, dirW);
  encapsCB.val = preview;
  pscompCB.val = pscomp;


#ifdef HAVE_JPEG
  CreateJPEGW();
  XSetTransientForHint(theDisp, jpegW, dirW);
#endif

#ifdef HAVE_TIFF
  CreateTIFFW();
  XSetTransientForHint(theDisp, tiffW, dirW);
#endif


  LoadFishCursors();
  SetCursors(-1);


  /* if we're not on a colormapped display, turn off rwcolor */
  if (theVisual->class != PseudoColor && 
      theVisual->class != GrayScale && rwcolor) {
    fprintf(stderr,"xv: not a colormapped display.  'rwcolor' turned off.\n");
    allocMode = AM_READONLY;
    dispMB.flags[DMB_COLRW] = 0;  /* de-'check' */
    dispMB.dim[DMB_COLRW] = 1;    /* and dim it */
  }


  if (force24) {
    Set824Menus(PIC24);
    conv24MB.flags[CONV24_LOCK]  = 1;
    picType = PIC24;
  }
  else if (force8) {
    Set824Menus(PIC8);
    conv24MB.flags[CONV24_LOCK]  = 1;
    picType = PIC8;
  }
  else {
    Set824Menus(PIC8);     /* default mode */
    picType = PIC8;
  }



  /* make std colormap, maybe */
  ChangeCmapMode(colorMapMode, 0, 0);


  /* Do The Thing... */
  mainLoop();
  Quit(0);
  return(0);
}




/***********************************/
static int cpos = 0;
static void printoption(st)
     char *st;
{
  if (strlen(st) + cpos > 78) {
    fprintf(stderr,"\n   ");
    cpos = 3;
  }

  fprintf(stderr,"%s ",st);
  cpos = cpos + strlen(st) + 1;
}

static void Syntax()
{
  fprintf(stderr, "Usage:\n");
  printoption(cmd);
  printoption("[-]");
  printoption("[-24]");
  printoption("[-2xlimit]");
  printoption("[-4x3]");
  printoption("[-8]");
  printoption("[-acrop]");
  printoption("[-aspect w:h]");
  printoption("[-best24]");
  printoption("[-bg color]");
  printoption("[-black color]");
  printoption("[-bw width]");
  printoption("[-cecmap]");
  printoption("[-cegeometry geom]");
  printoption("[-cemap]");
  printoption("[-cgamma rval gval bval]");
  printoption("[-cgeometry geom]");
  printoption("[-clear]");
  printoption("[-close]");
  printoption("[-cmap]");
  printoption("[-cmtgeometry geom]");
  printoption("[-cmtmap]");
  printoption("[-crop x y w h]");
  printoption("[-cursor char#]");
  printoption("[-DEBUG level]");
  printoption("[-dir directory]");
  printoption("[-display disp]");
  printoption("[-dither]");
  printoption("[-drift dx dy]");
  printoption("[-expand exp]");
  printoption("[-fg color]");
  printoption("[-fixed]");
  printoption("[-flist fname]");
  printoption("[-gamma val]");
  printoption("[-geometry geom]");
  printoption("[-grabdelay seconds]");
  printoption("[-gsdev str]");
  printoption("[-gsgeom geom]");
  printoption("[-gsres int]");
  printoption("[-help]");
  printoption("[-hflip]");
  printoption("[-hi color]");
  printoption("[-hist]");
  printoption("[-hsv]");
  printoption("[-icgeometry geom]");
  printoption("[-iconic]");
  printoption("[-igeometry geom]");
  printoption("[-imap]");
  printoption("[-lbrowse]");
  printoption("[-lo color]");
  printoption("[-loadclear]");
  printoption("[-max]");
  printoption("[-maxpect]");
  printoption("[-mfn font]");
  printoption("[-mono]");
  printoption("[-name str]");
  printoption("[-ncols #]");
  printoption("[-ninstall]");
  printoption("[-nofreecols]");
  printoption("[-nolimits]");
  printoption("[-nopos]");
  printoption("[-noqcheck]");
  printoption("[-noresetroot]");
  printoption("[-norm]");
  printoption("[-nostat]");
  printoption("[-owncmap]");
  printoption("[-perfect]");
  printoption("[-poll]");
  printoption("[-preset #]");
  printoption("[-quick24]");
  printoption("[-quit]");
  printoption("[-random]");
  printoption("[-raw]");
  printoption("[-rbg color]");
  printoption("[-rfg color]");
  printoption("[-rgb]");
  printoption("[-rmode #]");
  printoption("[-root]");
  printoption("[-rotate deg]");
  printoption("[-rv]");
  printoption("[-rw]");
  printoption("[-slow24]");
  printoption("[-smooth]");
  printoption("[-stdcmap]");
  printoption("[-tgeometry geom]");
  printoption("[-vflip]");
  printoption("[-viewonly]");
  printoption("[-visual type]");
  printoption("[-vsdisable]");
  printoption("[-vsgeometry geom]");
  printoption("[-vsmap]");
  printoption("[-vsperfect]");
  printoption("[-wait seconds]");
  printoption("[-white color]");
  printoption("[-wloop]");
  printoption("[filename ...]");
  fprintf(stderr,"\n\n");
  Quit(1);
}


/***********************************/
static void RmodeSyntax()
{
  fprintf(stderr,"%s: unknown root mode '%d'.  Valid modes are:\n", 
	  cmd, rootMode);
  fprintf(stderr,"\t0: tiling\n");
  fprintf(stderr,"\t1: integer tiling\n");
  fprintf(stderr,"\t2: mirrored tiling\n");
  fprintf(stderr,"\t3: integer mirrored tiling\n");
  fprintf(stderr,"\t4: centered tiling\n");
  fprintf(stderr,"\t5: centered on a solid background\n");
  fprintf(stderr,"\t6: centered on a 'warp' background\n");
  fprintf(stderr,"\t7: centered on a 'brick' background\n");
  fprintf(stderr,"\t8: symmetrical tiling\n");
  fprintf(stderr,"\t9: symmetrical mirrored tiling\n");
  fprintf(stderr,"\n");
  Quit(1);
}


/***********************************/
static int argcmp(a1, a2, minlen)
char *a1, *a2;
int minlen;
{
  /* does a string compare between a1 and a2.  To return '0', a1 and a2 
     must match to the length of a2, and that length has to
     be at least 'minlen'.  Otherwise, return non-zero */

  if (strlen(a1) < minlen || strlen(a2) < minlen) return 1;
  if (strlen(a1) > strlen(a2)) return 1;

  return (strncmp(a1, a2, strlen(a1)));
}


/***********************************/
static int openPic(filenum)
     int filenum;
{
  /* tries to load file #filenum (from 'namelist' list)
   * returns 0 on failure (cleans up after itself)
   * returns '-1' if caller should display DFLTPIC  (shown as text)
   * if successful, returns 1, creates mainW
   *
   * By the way, I'd just like to point out that this procedure has gotten
   * *way* out of hand...
   */

  PICINFO pinfo;
  int   i,filetype,freename, frompipe, frompoll, fromint, killpage;
  int   oldeWIDE, oldeHIGH, oldpWIDE, oldpHIGH;
  int   oldCXOFF, oldCYOFF, oldCWIDE, oldCHIGH, wascropped;
  char *tmp;
  char *fullname,       /* full name of the original file */
        filename[512],  /* full name of file to load (could be /tmp/xxx)*/
        globnm[512];    /* globbed version of fullname of orig file */

  xvbzero((char *) &pinfo, sizeof(PICINFO));

  /* init important fields of pinfo */
  pinfo.pic = (byte *) NULL;
  pinfo.comment = (char *) NULL;
  pinfo.numpages = 1;
  pinfo.pagebname[0] = '\0';


  normaspect = defaspect;
  freename = dfltkludge = frompipe = frompoll = fromint = wascropped = 0;
  oldpWIDE = oldpHIGH = oldCXOFF = oldCYOFF = oldCWIDE = oldCHIGH = 0;
  oldeWIDE = eWIDE;  oldeHIGH = eHIGH;
  fullname = NULL;
  killpage = 0;

  WaitCursor();

  SetISTR(ISTR_INFO,"");
  SetISTR(ISTR_WARNING,"");


  /* if we're not loading next or prev page in a multi-page doc, kill off
     page files */
  if (strlen(pageBaseName) && filenum!=OP_PAGEDN && filenum!=OP_PAGEUP) 
    killpage = 1;


  if (strlen(pageBaseName) && (filenum==OP_PAGEDN || filenum==OP_PAGEUP)) {
    if      (filenum==OP_PAGEUP && curPage>0)          curPage--;
    else if (filenum==OP_PAGEDN && curPage<numPages-1) curPage++;
    else    {
      XBell(theDisp, 0);     /* hit either end */
      SetCursors(-1);
      return 0;
    }

    sprintf(filename, "%s%d", pageBaseName, curPage+1);
    fullname = filename;
    goto HAVE_FILENAME;
  }



  if (filenum == DFLTPIC) {
    filename[0] = '\0';  basefname[0] = '\0';  fullfname[0] = '\0';
    fullname = "";
    LoadDfltPic(&pinfo);

    if (killpage) {      /* kill old page files, if any */
      KillPageFiles(pageBaseName, numPages);
      pageBaseName[0] = '\0';
      numPages = 1;
      curPage = 0;
    }

    goto GOTIMAGE;
  }


  if (filenum == GRABBED) {
    filename[0] = '\0';  basefname[0] = '\0';  fullfname[0] = '\0';
    fullname = "";
    i = LoadGrab(&pinfo);
    if (!i) goto FAILED;   /* shouldn't happen */

    if (killpage) {      /* kill old page files, if any */
      KillPageFiles(pageBaseName, numPages);
      pageBaseName[0] = '\0';
      numPages = 1;
      curPage = 0;
    }

    goto GOTIMAGE;
  }

  
  if (filenum == POLLED) {
    frompoll = 1;
    oldpWIDE = pWIDE;  oldpHIGH = pHIGH;
    wascropped = (cWIDE!=pWIDE || cHIGH!=pHIGH);
    oldCXOFF = cXOFF;  oldCYOFF = cYOFF;  oldCWIDE = cWIDE;  oldCHIGH = cHIGH;
    filenum = curname;
  }
    

  if (filenum == RELOAD) {
    fromint = 1;
    filenum = nList.selected;
  }


  if (filenum != LOADPIC) {
    if (filenum >= nList.nstr || filenum < 0) goto FAILED;
    curname = filenum;
    nList.selected = curname;
    ScrollToCurrent(&nList);  /* have scrl/list show current */
    XFlush(theDisp);    /* update NOW */
  }



  /* set up fullname and basefname */

  if (filenum == LOADPIC) {
    fullname = GetDirFName();
    if (fullname[0] == '~') {
      strcpy(globnm, fullname);
      Globify(globnm);
      fullname = globnm;
    }

    else if (ISPIPE(fullname[0])) {    /* read from a pipe. */
      strcpy(filename, fullname);
      if (readpipe(fullname, filename)) goto FAILED;
      frompipe = 1;
    }

    else if (fullname[0] != '/') {     /* relative fname:  prepend path */
      /* note: we're reusing 'globnm'.  No conflict.  If fullname started
	 with '~' (globbing done) fullname would now start with a '/',
	 and not fall into this block */
      GetDirPath(globnm);

      if (strlen(globnm)==0 || globnm[strlen(globnm)-1]!='/')
	strcat(globnm, "/");

      strcat(globnm, fullname);
      fullname = globnm;
    }
  }
  else fullname = namelist[filenum];


  strcpy(fullfname, fullname);
  tmp = BaseName(fullname);
  strcpy(basefname, tmp);

  /* chop off trailing '.Z' from friendly displayed basefname, if any */
  if (strlen(basefname)>2 && strcmp(basefname+strlen(basefname)-2,".Z")==0) 
    basefname[strlen(basefname)-2]='\0';

  if (filenum == LOADPIC && ISPIPE(fullname[0])) {
    /* if we're reading from a pipe, 'filename' will have the /tmp/xvXXXXXX
       filename, and we can skip a lot of stuff:  (such as prepending 
       'initdir' to relative paths, dealing with reading from stdin, etc. */

    /* at this point, fullname = "! do some commands",
                      filename = "/tmp/xv123456",
		  and basefname= "xv123456" */
  }

  else {  /* NOT reading from a PIPE */

    /* if fullname doesn't start with a '/' (ie, it's a relative path), 
       (and it's not LOADPIC and it's not the special case '<stdin>') 
       then we need to prepend a directory name to it:
       
       prepend 'initdir', 
       if we have a searchdir (ie, we have multiple places to look),
             see if such a file exists (via fopen()),
       if it does, we're done.
       if it doesn't, and we have a searchdir, try prepending searchdir
             and see if file exists.
       if it does, we're done.
       if it doesn't, remove all prepended directories, and fall through
             to error code below.  */
    
    if (filenum!=LOADPIC && fullname[0]!='/' && strcmp(fullname,STDINSTR)!=0) {
      char *tmp;

      /* stick 'initdir ' onto front of filename */

#ifndef VMS
      tmp = (char *) malloc(strlen(fullname) + strlen(initdir) + 2);
      if (!tmp) FatalError("malloc 'filename' failed");
      sprintf(tmp,"%s/%s", initdir, fullname);
#else  /* it is VMS */
      tmp = (char *) malloc(strlen(fullname) + 2);
      if (!tmp) FatalError("malloc 'filename' failed");
      sprintf(tmp,"%s", fullname);
#endif

      if (!strlen(searchdir)) {            /* no searchdir, don't check */
	fullname = tmp;
	freename = 1;
      }
      else {                     	   /* see if said file exists */
	FILE *fp;
	fp = fopen(tmp, "r");
	if (fp) {                          /* initpath/fullname exists */
	  fclose(fp);
	  fullname = tmp;
	  freename = 1;
	}
	else {                             /* doesn't:  try searchdir */
	  free(tmp);
#ifndef VMS
	  tmp = (char *) malloc(strlen(fullname) + strlen(searchdir) + 2);
	  if (!tmp) FatalError("malloc 'filename' failed");
	  sprintf(tmp,"%s/%s", searchdir, fullname);
#else  /* it is VMS */
	  tmp = (char *) malloc(strlen(fullname) + 2);
	  if (!tmp) FatalError("malloc 'filename' failed");
	  sprintf(tmp,"%s", fullname);
#endif

	  fp = fopen(tmp, "r");
	  if (fp) {                        /* searchpath/fullname exists */
	    fclose(fp);
	    fullname = tmp;
	    freename = 1;
	  }
	  else free(tmp);                  /* doesn't exist either... */
	}
      }
    }
    
    strcpy(filename, fullname);
    
    
    /* if the file is STDIN, write it out to a temp file */

    if (strcmp(filename,STDINSTR)==0) {
      FILE *fp;

#ifndef VMS      
      sprintf(filename,"%s/xvXXXXXX",tmpdir);
#else /* it is VMS */
      sprintf(filename, "Sys$Disk:[]xvXXXXXX");
#endif
      mktemp(filename);
      
      fp = fopen(filename,"w");
      if (!fp) FatalError("openPic(): can't write temporary file");
    
      while ( (i=getchar()) != EOF) putc(i,fp);
      fclose(fp);

      /* and remove it from list, since we can never reload from stdin */
      if (strcmp(namelist[0], STDINSTR)==0) deleteFromList(0);
    }
  }



 HAVE_FILENAME:

  /******* AT THIS POINT 'filename' is the name of an actual data file
    (no pipes or stdin, though it could be compressed) to be loaded */
  filetype = ReadFileType(filename);


  if (filetype == RFT_COMPRESS) {   /* a compressed file.  uncompress it */
    char tmpname[128];

    if (
#ifndef VMS
	UncompressFile(filename, tmpname)
#else
	UncompressFile(basefname, tmpname)
#endif
	) {

      filetype = ReadFileType(tmpname);    /* and try again */
      
      /* if we made a /tmp file (from stdin, etc.) won't need it any more */
      if (strcmp(fullname,filename)!=0) unlink(filename);

      strcpy(filename, tmpname);
    }
    else filetype = RFT_ERROR;

    WaitCursor();
  }


  if (filetype == RFT_ERROR) {
    char  str[512];
    sprintf(str,"Can't open file '%s'\n\n  %s.",filename, ERRSTR(errno));

    if (!polling) ErrPopUp(str, "\nBummer!");

    goto FAILED;  /* couldn't get magic#, screw it! */
  }


  if (filetype == RFT_UNKNOWN) {
    /* view as a text/hex file */
    TextView(filename);
    SetISTR(ISTR_INFO,"'%s' not in a recognized format.", basefname);
    /* Warning();  */
    goto SHOWN_AS_TEXT;
  }

  if (filetype < RFT_ERROR) {
    SetISTR(ISTR_INFO,"'%s' not in a readable format.", basefname);
    Warning();
    goto FAILED;
  }


  /****** AT THIS POINT: the filetype is a known, readable format */

  /* kill old page files, if any */
  if (killpage) {
    KillPageFiles(pageBaseName, numPages);
    pageBaseName[0] = '\0';
    numPages = 1;
    curPage = 0;
  }


  SetISTR(ISTR_INFO,"Loading...");

  i = ReadPicFile(filename, filetype, &pinfo, 0);

  if (!i) {
    if (filetype == RFT_XBM) {   /* probably just a '.h' file... */
      TextView(filename);
      goto SHOWN_AS_TEXT;
    }

    SetISTR(ISTR_INFO,"Couldn't load file '%s'.",filename);
    Warning();
    goto FAILED;
  }



  WaitCursor();

  if (pinfo.w==0 || pinfo.h==0) {  /* shouldn't happen, but let's be safe */
    SetISTR(ISTR_INFO,"Image size '0x0' not allowed.");
    Warning();
    if (pinfo.pic)     free(pinfo.pic);      pinfo.pic     = (byte *) NULL;
    if (pinfo.comment) free(pinfo.comment);  pinfo.comment = (char *) NULL;
    goto FAILED;
  }


  /**************/
  /* SUCCESS!!! */
  /**************/
    

 GOTIMAGE:
  /* successfully read this picture.  No failures from here on out
     (well, once the pic has been converted if we're locked in a mode) */


  state824 = 0;

  /* if we're locked into a mode, do appropriate conversion */
  if (conv24MB.flags[CONV24_LOCK]) {  /* locked */
    if (pinfo.type==PIC24 && picType==PIC8) {           /* 24 -> 8 bit */
      byte *pic8;
      pic8 = Conv24to8(pinfo.pic, pinfo.w, pinfo.h, ncols, 
		       pinfo.r, pinfo.g, pinfo.b);
      free(pinfo.pic);
      pinfo.pic = pic8;
      pinfo.type = PIC8;

      state824 = 1;
    }

    else if (pinfo.type!=PIC24 && picType==PIC24) {    /* 8 -> 24 bit */
      byte *pic24;
      pic24 = Conv8to24(pinfo.pic, pinfo.w, pinfo.h, 
			pinfo.r, pinfo.g, pinfo.b);
      free(pinfo.pic);
      pinfo.pic  = pic24;
      pinfo.type = PIC24;
    }
  }
  else {    /* not locked.  switch 8/24 mode */
    picType = pinfo.type;
    Set824Menus(picType);
  }


  if (!pinfo.pic) {  /* must've failed in the 8-24 or 24-8 conversion */
    SetISTR(ISTR_INFO,"Couldn't do %s conversion.",
	    (picType==PIC24) ? "8->24" : "24->8");
    if (pinfo.comment) free(pinfo.comment);  pinfo.comment = (char *) NULL;
    Warning();
    goto FAILED;
  }



  /* ABSOLUTELY no failures from here on out... */


  if (strlen(pinfo.pagebname)) {
    strcpy(pageBaseName, pinfo.pagebname);
    numPages = pinfo.numpages;
    curPage = 0;
  }


  if (mainW && !useroot) {
    /* avoid generating excess configure events while we resize the window */
    XSelectInput(theDisp, mainW, ExposureMask | KeyPressMask 
                 | ButtonPressMask | KeyReleaseMask
                 | EnterWindowMask | LeaveWindowMask);
    XFlush(theDisp);
  }

  /* kill off OLD picture, now that we've succesfully loaded a new one */
  KillOldPics();
  SetInfoMode(INF_STR);


  /* get info out of the PICINFO struct */
  pic   = pinfo.pic;
  pWIDE = pinfo.w;
  pHIGH = pinfo.h;
  if (pinfo.frmType >=0) SetDirRButt(F_FORMAT, pinfo.frmType);
  if (pinfo.colType >=0) SetDirRButt(F_COLORS, pinfo.colType);
  
  SetISTR(ISTR_FORMAT, pinfo.fullInfo);
  strcpy(formatStr, pinfo.shrtInfo);
  picComments = pinfo.comment;
  ChangeCommentText();

  for (i=0; i<256; i++) {
    rMap[i] = pinfo.r[i];
    gMap[i] = pinfo.g[i];
    bMap[i] = pinfo.b[i];
  }



  AlgInit();

  /* stick this file in the 'ctrlW' name list */
  if (filenum == LOADPIC && !frompipe) StickInCtrlList();

  if (polling && !frompoll) InitPoll();

  /* turn off 'frompoll' if the pic has changed size */
  if (frompoll && (pWIDE != oldpWIDE || pHIGH != oldpHIGH)) frompoll = 0;


  if (!browseCB.val && filenum == LOADPIC) DirBox(0);   /* close the DirBox */


  /* if we read a /tmp file, delete it.  won't be needing it any more */
  if (fullname && strcmp(fullname,filename)!=0) unlink(filename);


  SetISTR(ISTR_INFO,formatStr);
	
  SetInfoMode(INF_PART);
  SetISTR(ISTR_FILENAME, (filenum==DFLTPIC || filenum==GRABBED || frompipe)
	                   ? "<none>" : basefname);

  SetISTR(ISTR_RES,"%d x %d",pWIDE,pHIGH);
  SetISTR(ISTR_COLOR,"");

  /* adjust button in/activity */
  BTSetActive(&but[BCROP],0);  /* new picture, draw no cropping rectangle */
  BTSetActive(&but[BUNCROP], 0);
  ActivePrevNext();



  /* handle various 'auto-whatever' command line options
     Note that if 'frompoll' is set, things that have to do with 
     setting the expansion factor are skipped, as we'll want it to
     display in the (already-existing) window at the same size */


  if (frompoll) {
    cpic = pic;  cWIDE = pWIDE;  cHIGH = pHIGH;  cXOFF = cYOFF = 0;
    epic = cpic; eWIDE = cWIDE;  eHIGH = cHIGH;

    if (wascropped) DoCrop(oldCXOFF, oldCYOFF, oldCWIDE, oldCHIGH);
    SetCropString(but[BCROP].active);
  }
  else {
    int w,h,aspWIDE,aspHIGH,oldemode;

    oldemode = epicMode;
    epicMode = EM_RAW;   /* be in raw mode for all intermediate conversions */
    cpic = pic;  cWIDE = pWIDE;  cHIGH = pHIGH;  cXOFF = cYOFF = 0;
    epic = cpic; eWIDE = cWIDE;  eHIGH = cHIGH;

    SetCropString(but[BCROP].active);

    /*****************************************/
    /* handle aspect options:  -aspect, -4x3 */
    /*****************************************/

    if (normaspect != 1.0) {  /* -aspect */
      FixAspect(1, &w, &h);
      eWIDE = w;  eHIGH = h;
    }

    if (auto4x3) {
      w = eWIDE;  h = (w*3) / 4;
      eWIDE = w;  eHIGH = h;
    }
    

    /**************************************/
    /* handle cropping (-acrop and -crop) */
    /**************************************/

    if (autocrop) DoAutoCrop();
    if (acrop)    DoCrop(acropX, acropY, acropW, acropH);


    /********************************/
    /* handle rotation and flipping */
    /********************************/

    if (autorotate) {
      /* figure out optimal rotation.  (ie, instead of +270, do -90) */
      if      (autorotate ==  270) autorotate = -90;
      else if (autorotate == -270) autorotate =  90;

      while (autorotate) {
	if (autorotate < 0) {   /* rotate CW */
	  DoRotate(0);
	  autorotate += 90;
	}
	else {  /* rotate CCW */
	  DoRotate(1);
	  autorotate -= 90;
	}
      }
    }

    if (autohflip) Flip(0);  /* horizontal flip */
    if (autovflip) Flip(1);  /* vertical flip */


    /********************************************/
    /* handle expansion options:                */
    /*   -expand, -max, -maxpect, -fixed, -geom */
    /********************************************/

    /* at this point, treat whatever eWIDE,eHIGH is as 1x1 expansion,
       (due to earlier aspect-ratio option handling).  Note that
       all that goes on here is that eWIDE/eHIGH are modified.  No
       images are generated. */

    aspWIDE = eWIDE;  aspHIGH = eHIGH;   /* aspect-corrected eWIDE,eHIGH */

    if (expand < 0.0) {     /* negative:  reciprocal */
      eWIDE=(int)(aspWIDE/-expand);  
      eHIGH=(int)(aspHIGH/-expand);
    }
    else {
      eWIDE=(int)(aspWIDE * expand);  
      eHIGH=(int)(aspHIGH * expand);
    }



    if (maingeom) {
      /* deal with geometry spec.  Note, they shouldn't have given us
       *both* an expansion factor and a geomsize.  The geomsize wins out */
    
      int i,x,y,w,h,gewide,gehigh;

      gewide = eWIDE;  gehigh = eHIGH;
      i = XParseGeometry(maingeom,&x,&y,&w,&h);

      if (i&WidthValue)  gewide = w;
      if (i&HeightValue) gehigh = h;
      
      /* handle case where the pinheads only specified width *or * height */
      if (( i&WidthValue && ~i&HeightValue) ||
	  (~i&WidthValue &&  i&HeightValue)) {
    
	if (i&WidthValue) { gehigh = (aspHIGH * gewide) / pWIDE; }
	             else { gewide = (aspWIDE * gehigh) / pHIGH; }
      }

      /* specified a 'maximum size, but please keep your aspect ratio */
      if (fixedaspect && i&WidthValue && i&HeightValue) {
	if (aspWIDE > gewide || aspHIGH > gehigh) {
	  /* shrink aspWIDE,HIGH accordingly... */
	  double r,wr,hr;

	  wr = ((double) aspWIDE) / gewide;
	  hr = ((double) aspHIGH) / gehigh;

	  r = (wr>hr) ? wr : hr;   /* r is the max(wr,hr) */
	  aspWIDE = (int) ((aspWIDE / r) + 0.5);
	  aspHIGH = (int) ((aspHIGH / r) + 0.5);
	}

	/* image is now definitely no larger than gewide,gehigh */
	/* should now expand it so that one axis is of specified size */

	if (aspWIDE != gewide && aspHIGH != gehigh) {  /* is smaller */
	  /* grow aspWIDE,HIGH accordingly... */
	  double r,wr,hr;

	  wr = ((double) aspWIDE) / gewide;
	  hr = ((double) aspHIGH) / gehigh;

	  r = (wr>hr) ? wr : hr;   /* r is the max(wr,hr) */
	  aspWIDE = (int) ((aspWIDE / r) + 0.5);
	  aspHIGH = (int) ((aspHIGH / r) + 0.5);

	}

	eWIDE = aspWIDE;  eHIGH = aspHIGH;
      }
      else { eWIDE = gewide;  eHIGH = gehigh; }
    }


    if (automax) {   /* -max and -maxpect */
      eWIDE = dispWIDE;  eHIGH = dispHIGH;
      if (fixedaspect) FixAspect(0,&eWIDE,&eHIGH);
    }


    /* now, just make sure that eWIDE/eHIGH aren't too big... */
    /* shrink eWIDE,eHIGH preserving aspect ratio, if so... */
    if (eWIDE>maxWIDE || eHIGH>maxHIGH) {
      /* the numbers here can get big.  use floats */
      double r,wr,hr;

      wr = ((double) eWIDE) / maxWIDE;
      hr = ((double) eHIGH) / maxHIGH;

      r = (wr>hr) ? wr : hr;   /* r is the max(wr,hr) */
      eWIDE = (int) ((eWIDE / r) + 0.5);
      eHIGH = (int) ((eHIGH / r) + 0.5);
    }

    if (eWIDE < 1) eWIDE = 1;    /* just to be safe... */
    if (eHIGH < 1) eHIGH = 1;

    /* if we're using an integer tiled root mode, truncate eWIDE/eHIGH to
       be an integer divisor of the display size */
      
    if (useroot && (rootMode == RM_TILE || rootMode == RM_IMIRROR)) {
      /* make picture size a divisor of the rootW size.  round down */
      i = (dispWIDE + eWIDE-1) / eWIDE;   eWIDE = (dispWIDE + i-1) / i;
      i = (dispHIGH + eHIGH-1) / eHIGH;   eHIGH = (dispHIGH + i-1) / i;
    }


    /********************************************/
    /* handle epic generation options:          */
    /*   -raw, -dith, -smooth                   */
    /********************************************/

    if (autodither && ncols>0) epicMode = EM_DITH;

    /* if in CM_STDCMAP mode, and *not* in '-wait 0', then autodither */
    if (colorMapMode == CM_STDCMAP && waitsec != 0) epicMode = EM_DITH;

    /* if -smooth or image has been shrunk to fit screen */
    if (autosmooth || (pWIDE >maxWIDE || pHIGH>maxHIGH)
	           || (cWIDE != eWIDE || cHIGH != eHIGH)) epicMode = EM_SMOOTH;

    if (autoraw) epicMode = EM_RAW;

    /* 'dithering' makes no sense in 24-bit mode */
    if (picType == PIC24 && epicMode == EM_DITH) epicMode = EM_RAW;
    
    SetEpicMode();
  } /* end of !frompoll */



  /* at this point eWIDE,eHIGH are correct, but a proper epic (particularly
     if in DITH or SMOOTH mode) doesn't exist yet.  Will be made once the
     colors have been picked. */



  /* clear old image (window/root) before we start changing colors... */
  if ((theVisual->class == PseudoColor || theVisual->class == GrayScale) &&
      clearonload && 
      picType == PIC8 &&
      colorMapMode != CM_STDCMAP) {

    if (mainW && !useroot) {
      XClearArea(theDisp, mainW, 0, 0, eWIDE, eHIGH, True);
      XFlush(theDisp);
    }

    if (useroot) {
      mainW = vrootW;
      ClearRoot();
    }
  }


  if (useroot) mainW = vrootW;

  NewPicGetColors(autonorm, autohisteq); 

  GenerateEpic(eWIDE, eHIGH);     /* want to dither *after* color allocs */
  CreateXImage();

  WaitCursor();
  HandleDispMode();   /* create root pic, or mainW, depending... */


  if (LocalCmap) {
    XSetWindowAttributes xswa;
    xswa.colormap = LocalCmap;

    if (!ninstall) XInstallColormap(theDisp,LocalCmap);
    XChangeWindowAttributes(theDisp,mainW,CWColormap,&xswa);
    if (cmapInGam) XChangeWindowAttributes(theDisp,gamW,CWColormap,&xswa);
  }



  tmp = GetISTR(ISTR_COLOR);
  SetISTR(ISTR_INFO,"%s  %s",formatStr, tmp);
	
  SetInfoMode(INF_FULL);
  if (freename) free(fullname);


  SetCursors(-1);


  if (dirUp!=BLOAD) {
    /* put current filename into the 'save-as' filename */
    if      (strcmp(filename,STDINSTR)==0)   SetDirFName("stdin");
    else if (frompipe || filenum == LOADPIC || filenum == GRABBED ||
	     filenum == DFLTPIC) {}             /* leave it alone */
    else SetDirFName(basefname);
  }


  /* since we're now using BitGravity to keep parts of the image window
     valid when the window is resized, we have to force an expose event
     on the *previous* size of the window, as this portion of the new
     window will *not* be given an expose event

     UNLESS the window hasn't changed size at all, in which case an
     appropriate expose event will be generated by CreateMainWindow().

     Yech! */

  if (mainW && !useroot && !clearonload && oldeWIDE>0 && oldeHIGH>0 &&
      (oldeWIDE != eWIDE || oldeHIGH != eHIGH) )
    GenExpose(mainW, 0, 0, oldeWIDE, oldeHIGH);

  return 1;

  
 FAILED:
  SetCursors(-1);
  KillPageFiles(pinfo.pagebname, pinfo.numpages);

  if (fullname && strcmp(fullname,filename)!=0) 
    unlink(filename);   /* kill /tmp file */
  if (freename) free(fullname);

  if (!fromint && !polling && filenum>=0 && filenum<nList.nstr) 
    deleteFromList(filenum);

  return 0;


 SHOWN_AS_TEXT:    /* file wasn't in recognized format... */
  SetCursors(-1);

  if (strcmp(fullname,filename)!=0) unlink(filename);   /* kill /tmp file */
  if (freename) free(fullname);

  return 1;       /* we've displayed the file 'ok' */
}



/********************************/
int ReadFileType(fname)
     char *fname;
{
  /* opens fname (which *better* be an actual file by this point!) and
     reads the first couple o' bytes.  Figures out what the file's likely
     to be, and returns the appropriate RFT_*** code */


  FILE *fp;
  byte  magicno[8];    /* first 8 bytes of file */
  int   rv;


  if (!fname) return RFT_ERROR;   /* shouldn't happen */

#ifndef VMS
  fp = fopen(fname, "r");
#else  
  fp = fopen(fname,"rb","ctx=stm");   /* mutant VMS options... */
#endif

  if (!fp) return RFT_ERROR;

  rv = fread(magicno,8,1,fp);  
  fclose(fp);

  if (rv!=1) return RFT_UNKNOWN;    /* files less than 8 bytes long... */

  rv = RFT_UNKNOWN;

  if (strncmp((char *) magicno,"GIF87a",6)==0 ||
      strncmp((char *) magicno,"GIF89a",6)==0) rv = RFT_GIF;

  else if (strncmp((char *) magicno,"VIEW",4)==0 ||
	   strncmp((char *) magicno,"WEIV",4)==0) rv = RFT_PM;

  else if (magicno[0] == 'P' && magicno[1]>='1' && 
	   magicno[1]<='6') rv = RFT_PBM;

  else if (strncmp((char *) magicno,"#define",7)==0) rv = RFT_XBM;

  else if (magicno[0]==0x59 && (magicno[1]&0x7f)==0x26 &&
	   magicno[2]==0x6a && (magicno[3]&0x7f)==0x15) rv = RFT_SUNRAS;

  else if (magicno[0] == 'B' && magicno[1] == 'M') rv = RFT_BMP;

  else if (magicno[0]==0x52 && magicno[1]==0xcc) rv = RFT_UTAHRLE;

  else if ((magicno[0]==0x01 && magicno[1]==0xda) ||
	   (magicno[0]==0xda && magicno[1]==0x01)) rv = RFT_IRIS;

  else if (magicno[0]==0x1f && magicno[1]==0x9d) rv = RFT_COMPRESS;

  else if (magicno[0]==0x0a && magicno[1] <= 5) rv = RFT_PCX;

#ifdef HAVE_JPEG
  else if (magicno[0]==0xff && magicno[1]==0xd8 && 
	   magicno[2]==0xff) rv = RFT_JFIF;
#endif

#ifdef HAVE_TIFF
  else if ((magicno[0]=='M' && magicno[1]=='M') ||
	   (magicno[0]=='I' && magicno[1]=='I')) rv = RFT_TIFF;
#endif

#ifdef HAVE_PDS
  else if (strncmp((char *) magicno,  "NJPL1I00",8)==0 || /* fixed-len pds */
	   strncmp((char *) magicno+2,"NJPL1I",  6)==0 || /* vger+other pds */
           strncmp((char *) magicno,  "CCSD3ZF", 7)==0 || /* vikng pds browse*/
	   strncmp((char *) magicno+2,"CCSD3Z",  6)==0 || /* vik. huffman pds*/
	   strncmp((char *) magicno,  "LBLSIZE=",8)==0)   /* vicar */
      rv = RFT_PDSVICAR;
#endif

#ifdef GS_PATH
  else if (magicno[0] == '%' && magicno[1] == '!') rv = RFT_PS;
#endif

  return rv;
}



/********************************/
int ReadPicFile(fname, ftype, pinfo, quick)
     char    *fname;
     int      ftype, quick;
     PICINFO *pinfo;
{
  /* if quick is set, we're being called to generate icons, or something
     like that.  We should load the image as quickly as possible.  Currently,
     this only affects the LoadPS routine, which, if quick is set, only
     generates the page file for the first page of the document */

  int rv = 0;

  /* by default, most formats aren't multi-page */
  pinfo->numpages = 1;
  pinfo->pagebname[0] = '\0';

  switch (ftype) {
  case RFT_GIF:     rv = LoadGIF   (fname, pinfo);  break;
  case RFT_PM:      rv = LoadPM    (fname, pinfo);  break;
  case RFT_PBM:     rv = LoadPBM   (fname, pinfo);  break;
  case RFT_XBM:     rv = LoadXBM   (fname, pinfo);  break;
  case RFT_SUNRAS:  rv = LoadSunRas(fname, pinfo);  break;
  case RFT_BMP:     rv = LoadBMP   (fname, pinfo);  break;
  case RFT_UTAHRLE: rv = LoadRLE   (fname, pinfo);  break;
  case RFT_IRIS:    rv = LoadIRIS  (fname, pinfo);  break;
  case RFT_PCX:     rv = LoadPCX   (fname, pinfo);  break;

#ifdef HAVE_JPEG
  case RFT_JFIF:    rv = LoadJFIF  (fname, pinfo);  break;
#endif

#ifdef HAVE_TIFF
  case RFT_TIFF:    rv = LoadTIFF  (fname, pinfo);  break;
#endif

#ifdef HAVE_PDS
  case RFT_PDSVICAR: rv = LoadPDS(fname, pinfo);    break;
#endif

#ifdef GS_PATH
  case RFT_PS:       rv = LoadPS(fname, pinfo, quick);    break;
#endif

  }
  return rv;
}


/********************************/
int UncompressFile(name, uncompname)
     char *name, *uncompname;
{
  /* returns '1' on success, with name of uncompressed file in uncompname
     returns '0' on failure */

  char namez[128], *fname, buf[512];

  fname = name;
  namez[0] = '\0';


#ifndef VMS
  /* see if compressed file name ends with '.Z'.  If it *doesn't* we need
     temporarily rename it so it *does*, uncompress it, and rename *back*
     to what it was.  necessary because uncompress doesn't handle files
     that don't end with '.Z' */

  if (strlen(name)>=2 && strcmp(name + strlen(name)-2,".Z")!=0 &&
                         strcmp(name + strlen(name)-2,".z")!=0) {
    strcpy(namez, name);
    strcat(namez,".Z");

    if (rename(name, namez) < 0) {
      sprintf(buf, "Error renaming '%s' to '%s':  %s",
	      name, namez, ERRSTR(errno));
      ErrPopUp(buf, "\nBummer!");
      return 0;
    }

    fname = namez;
  }
#endif   /* not VMS */


  
#ifndef VMS
  sprintf(uncompname, "%s/xvuXXXXXX", tmpdir);
  mktemp(uncompname);
  sprintf(str,"%s -c %s >%s", UNCOMPRESS, fname, uncompname);
#else /* it IS VMS */
  strcpy(uncompname, "Sys$Disk:[]xvuXXXXXX");
  mktemp(uncompname);
#  ifdef HAVE_LZW
  sprintf(str,"%s %s %s", UNCOMPRESS, fname, uncompname);
#  else
  sprintf(str,"%s %s", UNCOMPRESS, fname);
#  endif
#endif

  SetISTR(ISTR_INFO, "Uncompressing '%s'...", BaseName(fname));
#ifndef VMS
  if (system(str)) {
#else
  if (!system(str)) {
#endif
    SetISTR(ISTR_INFO, "Unable to uncompress '%s'.", BaseName(fname));
    Warning();
    return 0;
  }
  
#ifdef VMS
  sprintf(str,"Rename %s %s", fname, uncompname);
  SetISTR(ISTR_INFO,"Renaming '%s'...", fname);
  if (!system(str)) {
    SetISTR(ISTR_INFO,"Unable to rename '%s'.", fname);
    Warning();
    return 0;
  }
#endif


#ifndef VMS
  if (strlen(namez)) {   /* put file back to original name */
    if (rename(namez, name) < 0) {
      sprintf(buf, "Error renaming '%s' to '%s':  %s",
	      namez, name, ERRSTR(errno));
      ErrPopUp(buf, "\nBummer!");
    }
  }
#endif /* not VMS */

  
  return 1;
}


/********************************/
void KillPageFiles(bname, numpages)
  char *bname;
  int   numpages;
{
  /* deletes any page files (numbered 1 through numpages) that might exist */
  char tmp[128];
  int  i;

  if (strlen(bname) == 0) return;   /* no page files */

  for (i=1; i<=numpages; i++) {
    sprintf(tmp, "%s%d", bname, i);
    unlink(tmp);
  }
}


/********************************/
void NewPicGetColors(donorm, dohist)
     int donorm, dohist;
{
  int i;

  /* some stuff that necessary whenever running an algorithm or 
     installing a new 'pic' (or switching 824 modes) */

  numcols = 0;   /* gets set in SortColormap:  set to 0 for PIC24 images */

  if (picType == PIC8) SortColormap();

  if (picType == PIC8) {
    /* see if image is a b/w bitmap.  
       If so, use '-black' and '-white' colors */
    if (numcols == 2) {
      if ((rMap[0] == gMap[0] && rMap[0] == bMap[0] && rMap[0] == 255) &&
	  (rMap[1] == gMap[1] && rMap[1] == bMap[1] && rMap[1] ==   0)) {
	/* 0=wht, 1=blk */
	rMap[0] = (whtRGB>>16)&0xff;  
	gMap[0] = (whtRGB>>8)&0xff;  
	bMap[0] = whtRGB&0xff;

	rMap[1] = (blkRGB>>16)&0xff;
	gMap[1] = (blkRGB>>8)&0xff;  
	bMap[1] = blkRGB&0xff;
      }

      else if ((rMap[0] == gMap[0] && rMap[0] == bMap[0] && rMap[0] ==   0) &&
	       (rMap[1] == gMap[1] && rMap[1] == bMap[1] && rMap[1] == 255)) {
	/*0=blk,1=wht*/
	rMap[0] = (blkRGB>>16)&0xff;
	gMap[0] = (blkRGB>>8)&0xff;
	bMap[0] = blkRGB&0xff;

	rMap[1] = (whtRGB>>16)&0xff;
	gMap[1] = (whtRGB>>8)&0xff;
	bMap[1]=whtRGB&0xff;
      }
    }
  }


  if (picType == PIC8) {
    /* reverse image, if desired */
    if (revvideo) {
      for (i=0; i<numcols; i++) {
	rMap[i] = 255 - rMap[i];
	gMap[i] = 255 - gMap[i];
	bMap[i] = 255 - bMap[i];
      }
    }

    /* save the desired RGB colormap (before dicking with it) */
    for (i=0; i<numcols; i++) { 
      rorg[i] = rcmap[i] = rMap[i];  
      gorg[i] = gcmap[i] = gMap[i];  
      borg[i] = bcmap[i] = bMap[i];  
    }
  }

  else if (picType == PIC24 && revvideo) {
    if (pic)                        InvertPic24(pic,     pWIDE, pHIGH);
    if (cpic && cpic!=pic)          InvertPic24(cpic,    cWIDE, cHIGH);
    if (epic && epic!=cpic)         InvertPic24(epic,    eWIDE, eHIGH);
    if (egampic && egampic != epic) InvertPic24(egampic, eWIDE, eHIGH);
  }


  NewCMap();

  if (donorm) DoNorm();
  if (dohist) DoHistEq();

  GammifyColors();

  if (picType == PIC24) ChangeCmapMode(CM_STDCMAP, 0, 1);
                   else ChangeCmapMode(defaultCmapMode, 0, 1);

  ChangeEC(0);
}



/***********************************/
static int readpipe(cmd, fname)
     char *cmd, *fname;
{
  /* cmd is something like: "! bggen 100 0 0"
   *
   * runs command (with "> /tmp/xv******" appended).  
   * returns "/tmp/xv******" in fname
   * returns '0' if everything's cool, '1' on error
   */

  char fullcmd[512], tmpname[64], str[512];
  int i;

  if (!cmd || strlen(cmd)<2) return 1;

  sprintf(tmpname,"%s/xvXXXXXX", tmpdir);
  mktemp(tmpname);
  if (tmpname[0] == '\0') {   /* mktemp() blew up */
    sprintf(str,"Unable to create temporary filename.");
    ErrPopUp(str, "\nHow unlikely!");
    return 1;
  }

  /* build command */
  strcpy(fullcmd, cmd+1);  /* skip the leading '!' character in cmd */
  strcat(fullcmd, " > ");
  strcat(fullcmd, tmpname);

  /* execute the command */
  sprintf(str, "Doing command: '%s'", fullcmd);
  OpenAlert(str);
  i = system(fullcmd);
  if (i) {
    sprintf(str, "Unable to complete command:\n  %s\n\n  exit status: %d",
	    fullcmd, i);
    CloseAlert();
    ErrPopUp(str, "\nThat Sucks!");
    unlink(tmpname);      /* just in case it was created */
    return 1;
  }

  CloseAlert();
  strcpy(fname, tmpname);
  return 0;
}






/****************/
static void openFirstPic()
{
  int i;

  if (!numnames) {  openPic(DFLTPIC);  return; }

  i = 0;
  if (!randomShow) {
    while (numnames>0) {
      if (openPic(0)) return;  /* success */
      else {
	if (polling && !i) 
	  fprintf(stderr,"%s: POLLING: Waiting for file '%s' \n\tto %s\n",
		  cmd, namelist[0], "be created, or whatever...");
	i = 1;
      }
    }
  }

  else {    /* pick random first picture */
    for (i=findRandomPic();  i>=0;  i=findRandomPic())
      if (openPic(i)) return;    /* success */
  }

  if (numnames>1) FatalError("couldn't open any pictures");
  else Quit(-1);
}


/****************/
static void openNextPic()
{
  int i;

  if (curname>=0) i = curname+1;
  else if (nList.selected >= 0 && nList.selected < numnames) 
       i = nList.selected;
  else i = 0;

 
  while (i<numnames && !openPic(i));
  if (i<numnames) return;    /* success */

  openPic(DFLTPIC);
}


/****************/
static void openNextQuit()
{
  int i;

  if (!randomShow) {
    if (curname>=0) i = curname+1;
    else if (nList.selected >= 0 && nList.selected < numnames) 
      i = nList.selected;
    else i = 0;

    while (i<numnames && !openPic(i));
    if (i<numnames) return;    /* success */
  }
  else {
    for (i=findRandomPic(); i>=0; i=findRandomPic())
      if (openPic(i)) return;
  }

  Quit(0);
}


/****************/
static void openNextLoop()
{
  int i,j,loop;

  j = loop = 0;
  while (1) {
    if (!randomShow) {

      if (curname>=0) i = curname+1;
      else if (nList.selected >= 0 && nList.selected < numnames) 
	i = nList.selected;
      else i = 0;

      if (loop) {  i = 0;   loop = 0; }

      while (i<numnames && !openPic(i));
      if (i<numnames) return;
    }
    else {
      for (i=findRandomPic(); i>=0; i=findRandomPic())
	if (openPic(i)) return;
    }

    loop = 1;        /* back to top of list */
    if (j) break;                         /* we're in a 'failure loop' */
    j++;
  }

  openPic(DFLTPIC);
}


/****************/
static void openPrevPic()
{
  int i;

  if (curname>0) i = curname-1;
  else if (nList.selected>0 && nList.selected < numnames) 
    i = nList.selected - 1;
  else i = numnames-1;

  for ( ; i>=0; i--) {
    if (openPic(i)) return;    /* success */
  }

  openPic(DFLTPIC);
}


/****************/
static void openNamedPic()
{
  /* if (!openPic(LOADPIC)) openPic(DFLTPIC); */
  openPic(LOADPIC);
}




/****************/
static int findRandomPic()
/****************/
{
  static byte *loadList;
  static int   left_to_load, listLen = -1;
  int          k;

  /* picks a random name out of the list, and returns it's index.  If there
     are no more names to pick, it returns '-1' and resets itself */

  if (!loadList || numnames!=listLen) {
    if (loadList) free(loadList);
    else srandom(time((time_t *)NULL)); /* seed the random */

    left_to_load = listLen = numnames;
    loadList = (byte *) malloc(listLen);
    for (k=0; k<listLen; k++) loadList[k] = 0;
  }
  
  if (left_to_load <= 0) {   /* we've loaded all the pics */
    for (k=0; k<listLen; k++) loadList[k] = 0;   /* clear flags */
    left_to_load = listLen;
    return -1;   /* 'done' return */
  }

  for (k=abs(random()) % listLen;  loadList[k];  k = (k+1) % listLen);
  
  left_to_load--;
  loadList[k] = TRUE;

  return k;
}

/****************/
static void mainLoop()
{
  /* search forward until we manage to display a picture, 
     then call EventLoop.  EventLoop will eventually return 
     NEXTPIC, PREVPIC, NEXTQUIT, QUIT, or, if >= 0, a filenum to GOTO */

  int i;

  /* if curname<0 (there is no 'current' file), 'Next' means view the 
     selected file (or the 0th file, if no selection either), and 'Prev' means
     view the one right before the selected file */

  openFirstPic();   /* find first displayable picture, exit if none */

  if (!pic)  {  /* must've opened a text file...  display dflt pic */
    openPic(DFLTPIC);
    if (mainW && !useroot) RaiseTextWindows();
  }

  if (useroot && autoquit) Quit(0);

  while ((i=EventLoop()) != QUIT) {
    if      (i==NEXTPIC) {
      if ((curname<0 && numnames>0) ||
	  (curname<numnames-1))  openNextPic();
    }

    else if (i==PREVPIC) {
      if (curname>0 || (curname<0 && nList.selected>0)) 
	openPrevPic();
    }

    else if (i==NEXTQUIT) openNextQuit();
    else if (i==NEXTLOOP) openNextLoop();
    else if (i==LOADPIC)  openNamedPic();

    else if (i==DELETE)   {  /* deleted currently-viewed image */
      curname = -1;
      ActivePrevNext();
      FakeButtonPress(&but[BNEXT]);  /* load selected image, if any */
    }

    else if (i==THISNEXT) {  /* open current sel, 'next' until success */
      int j;
      j = nList.selected;  
      if (j<0) j = 0;
      while (j<numnames && !openPic(j));
      if (!pic) openPic(DFLTPIC);
    }

    else if (i>=0 || i==GRABBED || i==POLLED || i==RELOAD ||
	     i==OP_PAGEUP || i==OP_PAGEDN) {
      openPic(i);
      /* if (!openPic(i)) openPic(DFLTPIC); */
    }
  }
}



/***********************************/
static void createMainWindow(geom, name)
     char *geom, *name;
{
  XSetWindowAttributes xswa;
  unsigned int         xswamask;
  XWindowAttributes    xwa;
  XWMHints             xwmh;
  XSizeHints           hints;
  XClassHint           classh;
  int                  i,x,y;
  unsigned int         w,h;
  static int           firstTime = 1;

  /*
   * this function mainly deals with parsing the geometry spec correctly.
   * More trouble than it should be, and probably more trouble than
   * it has to be, but who can tell these days, what with all those
   * Widget-usin' Weenies out there...
   *
   * Note:  eWIDE,eHIGH have the correct, desired window size.  Ignore the
   *        w,h fields in the geometry spec, as they've already been dealt with
   */

  x = y = w = h = 1;
  i = XParseGeometry(geom,&x,&y,&w,&h);

  hints.flags = 0;
  if ((i&XValue || i&YValue)) hints.flags = USPosition;

  if (i&XValue && i&XNegative) x = vrWIDE - eWIDE - abs(x);
  if (i&YValue && i&YNegative) y = vrHIGH - eHIGH - abs(y);

  if (x+eWIDE > vrWIDE) x = vrWIDE - eWIDE;   /* keep on screen */
  if (y+eHIGH > vrHIGH) y = vrHIGH - eHIGH;


#define VROOT_TRANS
#ifdef VROOT_TRANS
  if (vrootW != rootW) { /* virtual window manager running */
    int x1,y1;
    Window child;
    XTranslateCoordinates(theDisp, rootW, vrootW, x, y, &x1, &y1, &child);
    if (DEBUG) fprintf(stderr,"translate:  %d,%d -> %d,%d\n",x,y,x1,y1);
    x = x1;  y = y1;
  }
#endif

  hints.x = x;                  hints.y = y;
  hints.width = eWIDE;          hints.height = eHIGH;
  hints.max_width  = maxWIDE;   hints.max_height = maxHIGH;
  hints.flags |= USSize | PMaxSize;
    
  xswa.bit_gravity = StaticGravity;
  xswa.background_pixel = bg;
  xswa.border_pixel     = fg;
  xswa.colormap         = theCmap;
  
  xswa.backing_store    = WhenMapped;

  /* NOTE: I've turned 'backing-store' off on the image window, as some
     servers (HP 9000/300 series running X11R4) don't properly deal with
     things when the image window changes size.  It isn't a big performance
     improvement anyway (for the image window), unless you're on a slow
     network.  In any event, I'm not *turning off* backing store, I'm
     just not explicitly turning it *on*.  If your X server is set up
     that windows, by default, have backing-store turned on, then the 
     image window will, too */
  
  xswamask = CWBackPixel | CWBorderPixel | CWColormap /* | CWBackingStore */;
  if (!clearonload) xswamask |= CWBitGravity;

  if (mainW) {
    GetWindowPos(&xwa);

    /* generate an expose event if window hasn't changed size */
    if (xwa.width == eWIDE && xwa.height == eHIGH) 
      GenExpose(mainW, 0,0, eWIDE, eHIGH);  

    xwa.width = eWIDE;  xwa.height = eHIGH;
    SetWindowPos(&xwa);
    hints.flags = PSize | PMaxSize;
  } 

  else {
    mainW = XCreateWindow(theDisp,rootW,x,y,eWIDE,eHIGH,bwidth,dispDEEP,
			  InputOutput, theVisual, xswamask, &xswa);
    if (!mainW) FatalError("can't create window!");

    XSetCommand(theDisp, mainW, mainargv, mainargc);

    if (LocalCmap) {
      xswa.colormap = LocalCmap;
      XChangeWindowAttributes(theDisp,mainW,CWColormap,&xswa);
    }
  }

  XSetStandardProperties(theDisp,mainW,"","",None,NULL,0,&hints);
  setWinIconNames(name);

  xwmh.input = True;
  xwmh.flags = InputHint;

  if (iconPix) { 
    xwmh.icon_pixmap = iconPix;  
    xwmh.icon_mask   = iconmask;  
    xwmh.flags |= ( IconPixmapHint | IconMaskHint) ;
  }

  if (startIconic && firstTime) {
    xwmh.initial_state = IconicState;
    xwmh.flags |= StateHint;

    if (icongeom) {
      int i,x,y,w,h;
      i = XParseGeometry(icongeom, &x, &y, &w, &h);   /* ignore w,h */
      if (i&XValue && i&YValue) {
	if (i&XValue && i&XNegative) x = vrWIDE - icon_width - abs(x);
	if (i&YValue && i&YNegative) y = vrHIGH - icon_height - abs(y);

	xwmh.icon_x = x;  xwmh.icon_y = y;
	xwmh.flags |= (IconPositionHint);
      }
    }
  }
  XSetWMHints(theDisp, mainW, &xwmh);

  classh.res_name = "xv";
  classh.res_class = "XVroot";
  XSetClassHint(theDisp, mainW, &classh);

  firstTime = 0;
}


/***********************************/
static void setWinIconNames(name)
     char *name;
{
  char winname[256], iconname[256];

  if (winTitle) {
    strcpy(winname, winTitle);
    strcpy(iconname, winTitle);
  }

  else if (name[0] == '\0') {
    sprintf(winname, "xv %s",VERSTR);
    sprintf(iconname,"xv");
  }

  else {
    sprintf(winname,"xv %s: %s", VERSTR, name);
    sprintf(iconname,"%s",name);
  }

  if (mainW) {
    XStoreName(theDisp, mainW, winname);
    XSetIconName(theDisp, mainW, iconname);
  }
}


/***********************************/
void FixAspect(grow,w,h)
int   grow;
int   *w, *h;
{
  /* computes new values of eWIDE and eHIGH which will have aspect ratio
     'normaspect'.  If 'grow' it will preserve aspect by enlarging, 
     otherwise, it will shrink to preserve aspect ratio.  
     Returns these values in 'w' and 'h' */

  float xr,yr,curaspect,a,exp;

  *w = eWIDE;  *h = eHIGH;

  /* xr,yr are expansion factors */
  xr = ((float) eWIDE) / cWIDE;
  yr = ((float) eHIGH) / cHIGH;
  curaspect  = xr / yr;

  /* if too narrow & shrink, shrink height.  too wide and grow, grow height */
  if ((curaspect < normaspect && !grow) || 
      (curaspect > normaspect &&  grow)) {    /* modify height */
    exp = curaspect / normaspect;
    *h = (int) (eHIGH * exp + .5);
  }

  /* if too narrow & grow, grow width.  too wide and shrink, shrink width */
  if ((curaspect < normaspect &&  grow) || 
      (curaspect > normaspect && !grow)) {    /* modify width */
    exp = normaspect / curaspect;
    *w = (int) (eWIDE * exp + .5);
  }


  /* shrink to fit screen without changing aspect ratio */
  if (*w>maxWIDE) {
    int i;
    a = (float) *w / maxWIDE;
    *w = maxWIDE;
    i = (int) (*h / a + .5);        /* avoid freaking some optimizers */
    *h = i;
  }

  if (*h>maxHIGH) {
    a = (float) *h / maxHIGH;
    *h = maxHIGH;
    *w = (int) (*w / a + .5);
  }

  if (*w < 1) *w = 1;
  if (*h < 1) *h = 1;
}


/***********************************/
static void makeDispNames()
{
  int   prelen, n, i, done;
  char *suffix;

  suffix = namelist[0];
  prelen = 0;   /* length of prefix to be removed */
  n = i = 0;    /* shut up pesky compiler warnings */

  done = 0;
  while (!done) {
    suffix = (char *) strchr(suffix,'/');    /* find next '/' in file name */
    if (!suffix) break;

    suffix++;                       /* go past it */
    n = suffix - namelist[0];
    for (i=1; i<numnames; i++) {
      if (strncmp(namelist[0], namelist[i], n)!=0) { done=1; break; }
    }

    if (!done) prelen = n;
  }

  for (i=0; i<numnames; i++) 
    dispnames[i] = namelist[i] + prelen;
}


/***********************************/
void StickInCtrlList()
{
  /* stick current name (from 'load/save' box) and current working directory
     into 'namelist' */

  char *name;
  char  cwd[MAXPATHLEN];

  name = GetDirFName();
  GetDirPath(cwd);

  AddFNameToCtrlList(cwd, name);

  nList.selected = numnames-1;
  curname = numnames - 1;

  ChangedCtrlList();
}


/***********************************/
void AddFNameToCtrlList(fpath,fname)
     char *fpath, *fname;
{
  /* stick given path/name into 'namelist' */

  char *fullname, *dname;
  char cwd[MAXPATHLEN], globnm[MAXPATHLEN+100];
  int i;

  if (!fpath) fpath = "";  /* bulletproofing... */
  if (!fname) fname = "";  

  if (numnames == MAXNAMES) return;  /* full up */

  /* handle globbing */
  if (fname[0] == '~') {
    strcpy(globnm, fname);
    Globify(globnm);
    fname = globnm;
  }
    
  if (fname[0] != '/') {  /* prepend path */
    strcpy(cwd, fpath);   /* copy it to a modifiable place */

    /* make sure fpath has a trailing '/' char */
    if (strlen(cwd)==0 || cwd[strlen(cwd)-1]!='/') strcat(cwd, "/");

    fullname = (char *) malloc(strlen(cwd) + strlen(fname) + 2);
    if (!fullname) FatalError("couldn't alloc name in AddFNameToCtrlList()\n");

    sprintf(fullname, "%s%s", cwd, fname);
  }
  else {                 /* copy name to fullname */
    fullname = (char *) malloc(strlen(fname) + 1);
    if (!fullname) FatalError("couldn't alloc name in AddFNameToCtrlList()\n");
    strcpy(fullname, fname);
  }


  /* see if this name is a duplicate.  Don't add it if it is. */
  for (i=0; i<numnames; i++)
    if (strcmp(fullname, namelist[i]) == 0) {
      free(fullname);
      return;
    }
  
  namelist[numnames] = fullname;

  /* if there are names in the list already, and a common prefix has been
     lopped off, see if fullname has the same common prefix, and lop it
     off as well */

  dname = fullname;
  if (numnames) {
    int prelen;
    prelen = dispnames[0] - namelist[0];

    if (prelen) {
      if (strncmp(namelist[0], fullname, prelen)==0) {
	dname = fullname + prelen;
      }
    }
  }

  /* figure out how much of name can be shown */
  if (StringWidth(dname) > (nList.w-10-16)) {   /* has to be truncated */
    char *tmp;
    int   prelen = 0;

    tmp = dname;
    while (1) {
      tmp = (char *) strchr(tmp,'/'); /* find next (forward) '/' in filename */
      if (!tmp) break;

      tmp++;                   /* move to char following the '/' */
      prelen = tmp - dname;
      if (StringWidth(tmp) <= (nList.w-10-16)) break;   /* we're cool now */
    }

    dispnames[numnames] = dname + prelen;
  }
  else dispnames[numnames] = dname;

  numnames++;
}


/***********************************/
void ChangedCtrlList()
{
  /* called when the namelist/dispnames arrays have changed, and list needs
     to be re-displayed */

  int cname, lsel;

  if (numnames>0) BTSetActive(&but[BDELETE],1);
  if (numnames>0) BTSetActive(&but[BTXVIEW],1);

  cname = curname;  lsel = nList.selected;  /* get blown away in LSNewData */
  LSNewData(&nList, dispnames, numnames);
  curname = cname;  nList.selected = lsel;  /* restore prev values */

  ActivePrevNext();

  ScrollToCurrent(&nList);
  XClearArea(theDisp, ctrlW, 0, 0, 100, 30, False);  /* redraw 'n files' */
  DrawCtrlNumFiles();
}


/***********************************/
void ActivePrevNext()
{
  /* called to enable/disable the Prev/Next buttons whenever curname and/or
     numnames and/or nList.selected change */

  /* if curname<0 (there is no 'current' file), 'Next' means view the 
     selected file (or the 0th file, if no selection either), and 'Prev' means
     view the one right before the selected file */

  if (curname<0) {  /* light things based on nList.selected, instead */
    BTSetActive(&but[BNEXT], (numnames>0));
    BTSetActive(&but[BPREV], (nList.selected>0));
  }
  else {
    BTSetActive(&but[BNEXT], (curname<numnames-1));
    BTSetActive(&but[BPREV], (curname>0));
  }
}
  

/***********************************/
int DeleteCmd()
{
  /* 'delete' button was pressed.  Pop up a dialog box to determine
     what should be deleted, then do it.
     returns '1' if THE CURRENTLY VIEWED entry was deleted from the list, 
     in which case the 'selected' filename on the ctrl list is now 
     different, and should be auto-loaded, or something */

  static char *bnames[] = { "\004Disk File", "\nList Entry", "\033Cancel" };
  char str[512];
  int  del, i, delnum, rv;

  /* failsafe */
  delnum = nList.selected;
  if (delnum < 0 || delnum >= numnames) return 0;

  sprintf(str,"Delete '%s'?\n\n%s%s",
	  namelist[delnum],
	  "'List Entry' deletes selection from list.\n",
	  "'Disk File' deletes file associated with selection.");

  del = PopUp(str, bnames, 3);
  
  if (del == 2) return 0;   /* cancel */
  
  if (del == 0) {           /* 'Disk File' */
    char *name;
    if (namelist[delnum][0] != '/') {    /* prepend 'initdir' */
      name = (char *) malloc(strlen(namelist[delnum]) + strlen(initdir) + 2);
      if (!name) FatalError("malloc in DeleteCmd failed\n");
      sprintf(name,"%s/%s", initdir, namelist[delnum]);
    }
    else name = namelist[delnum];

    i = unlink(name);
    if (i) {
      sprintf(str,"Can't delete file '%s'\n\n  %s.", name, ERRSTR(errno));
      ErrPopUp(str, "\nPity");
      if (name != namelist[delnum]) free(name);
      return 0;
    }

    XVDeletedFile(name);
    if (name != namelist[delnum]) free(name);
  }

  deleteFromList(delnum);

  rv = 0;
  if (delnum == curname) {      /* deleted the viewed file */
    curname = nList.selected;
    rv = 1;                     /* auto-load currently 'selected' filename */
  }
  else if (delnum < curname) curname = (curname > 0) ? curname-1 : 0;

  return rv;
}


/********************************************/
static void deleteFromList(delnum)
     int delnum;
{
  int i;

  /* remove from list on either 'List Entry' or (successful) 'Disk File' */

  /* determine if namelist[delnum] needs to be freed or not */
  for (i=0; i<mainargc && mainargv[i] != namelist[delnum]; i++) ;
  if (i == mainargc) {  /* not found.  free it */
    free(namelist[delnum]);
  }

  if (delnum != numnames-1) {
    /* snip out of namelist and dispnames lists */
    xvbcopy((char *) &namelist[delnum+1], (char *) &namelist[delnum], 
	  (numnames - delnum - 1) * sizeof(namelist[0]));

    xvbcopy((char *) &dispnames[delnum+1], (char *) &dispnames[delnum], 
	  (numnames - delnum - 1) * sizeof(dispnames[0]));
  }
  
  numnames--;
  if (numnames==0) BTSetActive(&but[BDELETE],0);
  if (numnames==0) BTSetActive(&but[BTXVIEW],0);

  nList.nstr = numnames;
  nList.selected = delnum;

  if (nList.selected >= numnames) nList.selected = numnames-1;
  if (nList.selected < 0) nList.selected = 0;

  SCSetRange(&nList.scrl, 0, numnames - nList.nlines, 
	     nList.scrl.val, nList.nlines-1);
  ScrollToCurrent(&nList);
  XClearArea(theDisp, ctrlW, 0, 0, 200, 40, True);  /* redraw part of ctrlW */

  ActivePrevNext();
}


/***********************************/
void HandleDispMode()
{
  /* handles a change in the display mode (windowed/root).
     Also, called to do the 'right' thing when opening a picture
     displays epic, in current size, UNLESS we've selected an 'integer'
     root tiling thingy, in which case we resize epic appropriately */

  static int haveoldinfo = 0;
  static Window            oldMainW;
  static int               oldCmapMode;
  static XSizeHints        oldHints;
  static XWindowAttributes oldXwa;
  int i;


  WaitCursor();

  /****************************************************************/
  /*** DMB_WINDOW windowed mode                                   */
  /****************************************************************/


  if (dispMode == DMB_WINDOW) {        /* windowed */
    char fnam[256];

    if (fullfname[0] == '\0') fnam[0] = '\0';
    else {
      char *tmp;
      int   i, state;

      /* find beginning of next-to-last pathname component, ie,
	 if fullfname is "/foo/bar/snausage", we'd like "bar/snausage" */

      state = 0;
      for (i=strlen(fullfname); i>0 && state!=2; i--) {
	if (fullfname[i] == '/') state++;
      }

      if (state==2) tmp = fullfname + i + 2;
      else tmp = fullfname;

      strcpy(fnam, tmp);

      /* if we're viewing a multi-page doc, add page # to title */
      if (strlen(pageBaseName) && numPages>1) {
	char foo[64];
	sprintf(foo, "  Page %d of %d", curPage+1, numPages);
	strcat(fnam, foo);
      }

    }

    if (useroot && resetroot) ClearRoot();

    if (mainW == (Window) NULL || useroot) {  /* window not visible */
      useroot = 0;  

      if (haveoldinfo) {             /* just remap mainW and resize it */
	XWMHints xwmh;

	mainW = oldMainW;

	/* enable 'perfect' and 'owncmap' options */
	dispMB.dim[DMB_COLPERF] = (picType == PIC8) ? 0 : 1;
	dispMB.dim[DMB_COLOWNC] = (picType == PIC8) ? 0 : 1;

	XSetStandardProperties(theDisp,mainW,"","",None,NULL,0,&oldHints);
	setWinIconNames(fnam);

	xwmh.initial_state = NormalState;
	xwmh.input = True;
	xwmh.flags = InputHint;

	if (iconPix) { 
	  xwmh.icon_pixmap = iconPix;  
	  xwmh.icon_mask   = iconmask;  
	  xwmh.flags |= ( IconPixmapHint | IconMaskHint) ;
	}

	xwmh.flags |= StateHint;
	XSetWMHints(theDisp, mainW, &xwmh);

	oldXwa.width = eWIDE;  oldXwa.height = eHIGH;
	SetWindowPos(&oldXwa);
	XResizeWindow(theDisp, mainW, eWIDE, eHIGH);
	XMapWindow(theDisp, mainW);
      }

      else {                         /* first time.  need to create mainW */
	mainW = (Window) NULL;
	createMainWindow(maingeom, fnam);
	XSelectInput(theDisp, mainW, ExposureMask | KeyPressMask 
		     | StructureNotifyMask | ButtonPressMask
		     | KeyReleaseMask | ColormapChangeMask
		     | EnterWindowMask | LeaveWindowMask );

	StoreDeleteWindowProp(mainW);
	XMapWindow(theDisp,mainW);
      }
    }

    else {                            /* mainW already visible */
      createMainWindow(maingeom, fnam);
      XSelectInput(theDisp, mainW, ExposureMask | KeyPressMask 
		   | StructureNotifyMask | ButtonPressMask
		   | KeyReleaseMask | ColormapChangeMask
		   | EnterWindowMask | LeaveWindowMask );

      if (LocalCmap) {                /* AllocColors created local colormap */
	XSetWindowColormap(theDisp, mainW, LocalCmap);
      }
    }

    useroot = 0;
  }


  /****************************************************************/
  /*** ROOT mode                                                  */
  /****************************************************************/


  else if (dispMode > DMB_WINDOW && dispMode <= /* DMB_CBRICK */ 99) {
    int ew, eh, regen;

    regen = 0;
    if (!useroot) {                  /* have to hide mainW, etc. */

      /* disable 'perfect' and 'owncmap' options */
      dispMB.dim[DMB_COLPERF] = 1;
      dispMB.dim[DMB_COLOWNC] = 1;

      /* save current window stuff */
      haveoldinfo = 1;
      oldMainW = mainW;
      oldCmapMode = colorMapMode;

      GetWindowPos(&oldXwa);
      if (!XGetNormalHints(theDisp, mainW, &oldHints)) oldHints.flags = 0;
      oldHints.x=oldXwa.x;  oldHints.y=oldXwa.y;  oldHints.flags|=USPosition;


      if (LocalCmap) regen = 1;  /* have to regen Ximage */
    
      /* if using perfect or owncmap, switch to normal */
      if (colorMapMode != CM_STDCMAP && colorMapMode != CM_NORMAL) 
	ChangeCmapMode(CM_NORMAL, 0, 0);

      XUnmapWindow(theDisp, mainW);
      mainW = vrootW;

      if (!ctrlUp) {    /* make sure ctrl is up when going to 'root' mode */
	XWMHints xwmh;
	xwmh.initial_state = IconicState;
	xwmh.flags = StateHint;
	XSetWMHints(theDisp, ctrlW, &xwmh);
	CtrlBox(1);
      }
    }
      
    useroot = 1;
    rootMode = dispMode - DMB_ROOT;
    ew = eWIDE;  eh = eHIGH;

    RANGE(ew,1,maxWIDE);  RANGE(eh,1,maxHIGH);

    if (rootMode == RM_TILE || rootMode == RM_IMIRROR) {
      /* make picture size a divisor of the rootW size.  round down */
      i = (dispWIDE + ew-1) / ew;   ew = (dispWIDE + i-1) / i;
      i = (dispHIGH + eh-1) / eh;   eh = (dispHIGH + i-1) / i;
    }

    if (ew != eWIDE || eh != eHIGH) {  /* changed size... */
      GenerateEpic(ew, eh);
      CreateXImage();
    }
    else {
      /* didn't regen XImage.  If we were using LocalCmap we have to */
      if (regen) CreateXImage();                    
    }

    KillOldRootInfo();
    MakeRootPic();
    SetCursors(-1);
  }

  else {
    fprintf(stderr,"unknown dispMB value '%d' in HandleDispMode()\n",
	    dispMode);
  }

  SetCursors(-1);
}


/*******************************************************/
static void add_filelist_to_namelist(flist, nlist, numn, maxn)
     char  *flist;
     char **nlist;
     int   *numn, maxn;
{
  /* written by Brian Gregory  (bgregory@megatest.com) */

  FILE *fp;

  fp = fopen(flist,"r");
  if (!fp) {
    fprintf(stderr,"Can't open filelist '%s': %s\n", flist, ERRSTR(errno));
    return;
  }

  while (*numn < maxn) {
    char *s, *nlp, fbuf[MAXPATHLEN];
    if (!fgets(fbuf, MAXPATHLEN, fp) ||
	!(s = (char *) malloc(strlen(fbuf)))) break;

    nlp = (char *) rindex(fbuf, '\n');
    if (nlp) *nlp = '\0';
    strcpy(s, fbuf);

    namelist[*numn] = s;  (*numn)++;
  }


  if (*numn == maxn) {
    fprintf(stderr, "%s: too many filenames.  Only using first %d.\n",
	    flist, maxn);
  }

  fclose(fp);
}




/************************************************************************/

/***********************************/
char *lower_str(str)
     char *str;
{
  char *p;
  for (p=str; *p; p++) if (isupper(*p)) *p = tolower(*p);
  return str;
}


/***********************************/
int rd_int(name)
char *name;
{
  /* returns '1' if successful.  result in def_int */

  if (rd_str_cl(name, "")) {     /* sets def_str */
    if (sscanf(def_str, "%ld", &def_int) == 1) return 1;
    else {
      fprintf(stderr, "%s: couldn't read integer value for %s resource\n", 
	      cmd, name);
      return 0;
    }
  }
  else return 0;
}


/***********************************/
int rd_str(name)
char *name;
{
  return rd_str_cl(name, "");
}


/***********************************/
int rd_flag(name)
char *name;
{
  /* returns '1' if successful.  result in def_int */
  
  char buf[256];

  if (rd_str_cl(name, "")) {  /* sets def_str */
    strcpy(buf, def_str);
    lower_str(buf);

    def_int = (strcmp(buf, "on")==0) || 
              (strcmp(buf, "1")==0) ||
	      (strcmp(buf, "true")==0) ||
	      (strcmp(buf, "yes")==0);
    return 1;
    }

  else return 0;
}
    



static int xrm_initted = 0;
 
/***********************************/
int rd_str_cl (name_str, class_str)
     char *name_str;
     char *class_str;
{
  /* note: *all* X resource reading goes through this routine... */

  /* returns '1' if successful, result in def_str */

  char     q_name[BUFSIZ], q_class[BUFSIZ];
  char    *type;
  XrmValue result;

  static XrmDatabase def_resource;

  if (!xrm_initted) {
    char *xrm_str;

    XrmInitialize();
    xrm_initted = 1;
    def_resource = (XrmDatabase) 0;

    xrm_str = XResourceManagerString(theDisp);  /* get RESOURCE_MANAGER prop */
    if (xrm_str) {
      def_resource = XrmGetStringDatabase(xrm_str);
      if (DEBUG) fprintf(stderr,"rd_str_cl: Using RESOURCE_MANAGER prop.\n");
    }

    else {    /* no RESOURCE_MANAGER prop.  read from 'likely' file */
      char foo[256], *homedir, *xenviron;
      XrmDatabase res1;

#ifdef VMS
      strcpy(foo, "SYS$LOGIN:DECW$XDEFAULTS.DAT");
#else
      homedir = (char *) getenv("HOME");
      if (!homedir) homedir = ".";
      sprintf(foo,"%s/.Xdefaults", homedir);
#endif

      def_resource = XrmGetFileDatabase(foo);

      if (DEBUG) {
	fprintf(stderr,"rd_str_cl: No RESOURCE_MANAGER prop.\n");
	fprintf(stderr,"rd_str_cl: Using file '%s' (%s)  ",
		foo, (def_resource) ? "success" : "failure");
      }


      /* merge file pointed to by XENVIRONMENT */
      xenviron = (char *) getenv("XENVIRONMENT");
      if (xenviron) {
	res1 = XrmGetFileDatabase(xenviron);

	if (DEBUG) {
	  fprintf(stderr,"merging XENVIRONMENT='%s' (%s)  ",
		  xenviron, (res1) ? "success" : "failure");
	}

	if (res1) {    /* merge databases */
	  if (!def_resource) def_resource = res1;
	  else XrmMergeDatabases(res1, &def_resource);
	}
      }

      else {  /* look in $HOME/.Xdefaults-machinename */
	char hname[128];
	gethostname(hname, 128);
	hname[127] = '\0';

	sprintf(foo, "%s/.Xdefaults-%s", homedir, hname);
	res1 = XrmGetFileDatabase(foo);

	if (DEBUG) {
	  fprintf(stderr,"merging file '%s' (%s)  ",
		  foo, (res1) ? "success" : "failure");
	}

	if (res1) {
	  if (!def_resource) def_resource = res1;
	  else XrmMergeDatabases(res1, &def_resource);
	}
      }

      if (DEBUG) fprintf(stderr,"\n\n");
    }
  }


  if (!def_resource) return 0;     /* no resource database to search! */


  strcpy (q_name, PROGNAME);
  strcat (q_name, ".");
  strcat (q_name, name_str);
  
  strcpy (q_class, "Program");
  strcat (q_class, ".");
  strcat (q_class, class_str);

  (void) XrmGetResource(def_resource, q_name, q_class, &type, &result);
  
  def_str = result.addr;
  if (def_str) return (1);
  else return (0);
}


