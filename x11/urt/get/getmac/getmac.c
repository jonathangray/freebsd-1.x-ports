/* 
 * showImage.c - Read an RLE file and show it on the Mac screen under MPW
 * 
 * Author:	Spencer Thomas (LSC version)
 *		University of Utah
 *
 *		Port to MPW by
 *		John Peterson
 * 		Apple Computer Inc.
 * Date:	Mon Apr 11 1988
 *
 *	17-Jul-89 - Fixed to run with 32 Bit Quickdraw and MPW 3.0
 *
 */

#define __ALLNU__		/* Enable IM v.5 stuff.  Gag. */

#include <QuickDraw.h>
#include <Windows.h>
#include <Dialogs.h>
#include <Menus.h>
#include <Events.h>
#include <Packages.h>
#include <Desk.h>
#include <CursorCtl.h>
#include <math.h>
#include <stdio.h>
#include <rle.h>

/*
 * Global variables
 */

GrafPtr	screenPort;		/* Window manager's port */


Point	dlogOrg;		/* Where to put dialog windows */

FILE * rleFile;			/* input file */

char rleName[64];		/* Name of input file */

Boolean doneFlag;
Boolean dodither;		/* Dither or not? */

char * malloc();		/* Needs to be said */

/* Restrict new color to 0::255 for dithering */
#define ADJUST( amt )	( (amt) < 0 ? 0 : ((amt) > 255 ? 255 : (amt)) )

struct imgBuffers {
    rle_pixel *buffers[3];
    CGrafPort imgPort;
    int pixMaxY;
    GDHandle pixDevice;
};

static WindowPtr imageWin = nil;
WindowPtr makeWindow();

/*
 * The existence of this macro provides some clue as to how ghastly
 * debugging Macintosh programs is.  Real memory management is coming
 * one of these years...
 */

#ifdef DEBUG
# define MSG(msg) fprintf(stderr, "Debug--> %s\n", msg)
#else
# define MSG(msg) 
#endif

int disp_y = -1;		/* How far we got before being interrupted */

showImage()
{
    if ( imageWin != nil )
    {
	imgWinClose();
	imageWin = nil;
    }

    rle_dflt_hdr.rle_file = rleFile;
 
    if ( rle_get_error( rle_get_setup( &rle_dflt_hdr ), "GetMac", rleName ) )
	return;
    RLE_CLR_BIT( rle_dflt_hdr, RLE_ALPHA );

    if ((rle_dflt_hdr.ncolors != 1) && (rle_dflt_hdr.ncolors != 3))
    {
        fprintf(stderr, "getmac: Sorry - only monochrome and RGB images\n");
	exit(-1);
    }
    
    MSG("opened the_hdr");
    if ( (imageWin = makeWindow( &rle_dflt_hdr )) == nil )
	return;
    MSG("made window");

    readImage( &rle_dflt_hdr );

    MSG("Drawing image");
    disp_y = 0;
    drawImage( &rle_dflt_hdr );
}

redrawImage()
{
    if ( disp_y == -1 )
    {
	disp_y = 0;
    }
	
}

imgWinUpdate()
{
    disp_y = 0;
    drawImage( &rle_dflt_hdr );
}

imgWinClose()
{
    struct imgBuffers *buffers;
    int i;
    
    if ( imageWin )
    {
	buffers = (struct imgBuffers *)((WindowPeek)imageWin)->refCon;
    	
	for (i = 0; i < rle_dflt_hdr.ncolors; i++)
	    free( buffers->buffers[i] );

	if ( buffers->pixMaxY > 0 )
	{
	    free( (*buffers->imgPort.portPixMap)->baseAddr );
	    CloseCPort( &buffers->imgPort );
	}
	free( buffers );
    
	CloseWindow( imageWin );
	imageWin = nil;
    }
}

myWindowClobber()
{
    WindowPtr theWindow;
    
    GetPort( &theWindow );
    DisposeWindow( theWindow );
}

imgWinMouse( thePoint )
Point thePoint;
{
    int xsize, ysize;
    long idx;
    struct imgBuffers *buffers =
	(struct imgBuffers *)((WindowPeek)imageWin)->refCon;
    int r, g, b;
    RGBColor theColor;
    
    ysize = rle_dflt_hdr.ymax;
    xsize = rle_dflt_hdr.xmax + 1;
    idx = (long)xsize * (long)(ysize - thePoint.v) + (long)thePoint.h;
    r = buffers->buffers[0][idx];
    if (rle_dflt_hdr.ncolors == 3)
    {
        g = buffers->buffers[1][idx];
        b = buffers->buffers[2][idx];
    }
    else
        g = b = buffers->buffers[0][idx];
    
    GetCPixel( thePoint.h, thePoint.v, &theColor );
    fprintf( stderr, "Pixel at ( %d, %d ) = [ %u, %u, %u ] -> [ %u, %u, %u ]\n",
	     thePoint.h, ysize - thePoint.v, r<<8, g<<8, b<<8,
	     theColor.red, theColor.green, theColor.blue );
#ifdef notdef
    if ( buffers->pixMaxY > 0 )
    {
	GetPort( &oldPort );
	SetPort( &buffers->imgPort );
	pixcol.rgb = theColor;
	pixcol.value = Color2Index( &pixcol.rgb );;
	gotcol.value = pixcol.value;
	Index2Color( gotcol.value, &gotcol.rgb );
	fprintf( stderr, "	   color maps to index %d -> [ %u, %u, %u ]\n",
		 pixcol.value, gotcol.rgb.red, gotcol.rgb.green,
		 gotcol.rgb.blue );
	SetPort( oldPort );
    }
#endif
};

imgWinIdle()
{
    if ( disp_y != -1 )
	drawImage( &rle_dflt_hdr );
}
    
WindowPtr
makeWindow( the_hdr )
rle_hdr * the_hdr;
{
    WindowPtr win;
    Rect wRect;
    Str255 wTitle;
    int xsize, ysize;
    struct imgBuffers *buffers;
    int i;
    
    the_hdr->xmax -= the_hdr->xmin;
    the_hdr->xmin = 0;
    the_hdr->ymax -= the_hdr->ymin;
    the_hdr->ymin = 0;

    xsize = the_hdr->xmax - the_hdr->xmin + 1;
    ysize = the_hdr->ymax - the_hdr->ymin + 1;
    MSG("Allocating buffers");
    buffers = (struct imgBuffers *) malloc( sizeof(struct imgBuffers) );
    
    for ( i = 0; i < 3; i++ ) buffers->buffers[i] = nil;
    
    for ( i = 0; i < the_hdr->ncolors; i++)
        buffers->buffers[i] = (rle_pixel *)malloc( (long)xsize * (long)(ysize+1) );

    buffers->pixMaxY = -1;  /* Nothing there yet */
    MSG("Got buffers");    

    if ( buffers->buffers[0] == nil || 
        ((the_hdr->ncolors == 3) && (buffers->buffers[1] == nil || buffers->buffers[2] == nil )))
    {
	if ( buffers->buffers[0] != nil )
	    free( buffers->buffers[0] );
	if ( buffers->buffers[1] != nil )
	    free( buffers->buffers[1] );
	free( buffers );
	fprintf( stderr, "Not enough memory to save image\n" );
	return nil;
    }
    MSG("Really got buffers");

    wRect.left = 10;
    wRect.top = 40;
    wRect.right = wRect.left + xsize;
    wRect.bottom = wRect.top + ysize;
    strcpy( wTitle, rleName );
    c2pstr( wTitle );

    MSG("Making new window");
    /* wTitle doesn't work for some reason. */
    win = NewCWindow( nil, &wRect, wTitle, true, 
    		      noGrowDocProc, (WindowPtr) -1, true, (long) buffers );
    MSG("Window made");
    if ( win == nil )
	return nil;
    return win;
}

readImage( the_hdr )
rle_hdr * the_hdr;
{
    struct imgBuffers *buffers = (struct imgBuffers *)((WindowPeek)imageWin)->refCon;
    rle_pixel *scans[3];
    int y, i, xsize;

    fprintf(stderr, "Reading file..." );
    xsize = the_hdr->xmax + 1;
    for ( y = 0; y <= the_hdr->ymax; y++ )
    {
	SpinCursor( (short) 1 );
	for ( i = 0; i < the_hdr->ncolors; i++ )
	    scans[i] = &buffers->buffers[i][(long)y * (long)xsize];
	rle_getrow( the_hdr, scans );
    }
    fprintf( stderr, "\n" );
}

drawImage( the_hdr )
rle_hdr * the_hdr;
{
    int ysize, xsize, x, y;
    register int cval;
    struct imgBuffers *buffers = (struct imgBuffers *)((WindowPeek)imageWin)->refCon;
    register rle_pixel *rptr, *gptr, *bptr;
    long rdiff, gdiff, bdiff;
    EventRecord theEvent;
    CGrafPtr oldPort;
    Rect srcDestRect;
    ColorSpec pixcol, gotcol;
    GDHandle oldDevice;
    long pixidx;
    int pixoffset, mask, depth;
    char * pixel;

    ysize = the_hdr->ymax;
    xsize = the_hdr->xmax + 1;
    if ( buffers->pixMaxY < ysize )
    {
	MSG("GetOldPort");
	GetPort( (GrafPtr *) &oldPort );
	MSG("Got Port");
	oldDevice = GetGDevice();
	MSG("Got port & device");

	if ( buffers->pixMaxY == -1 )
	    createPix( buffers, xsize, ysize + 1 );
	if ( buffers->pixMaxY != -2 )
	{
	    if ( buffers->pixMaxY >= 0 )
	    {
		SetRect( &srcDestRect, 0, ysize - buffers->pixMaxY,
			 xsize, ysize + 1 );
		CopyBits( (BitMap *)(*buffers->imgPort.portPixMap),
			  &imageWin->portBits,
			  &srcDestRect, &srcDestRect, 64, nil );
		disp_y = buffers->pixMaxY + 1;
	    }
	    
	    SetPort( (GrafPtr) &buffers->imgPort );
	    /* Set theGDevice to device with maximum pixel depth */
	    SetGDevice( buffers->pixDevice );
	}
	else
	    SetPort( (GrafPtr) oldPort );
	MSG("Set up Port");
	if (disp_y < ysize) fprintf( stderr, "Drawing Image...\n" );

	rptr = &buffers->buffers[0][disp_y*xsize];
	if (the_hdr->ncolors == 3)
	{
	    gptr = &buffers->buffers[1][disp_y*xsize];
	    bptr = &buffers->buffers[2][disp_y*xsize];
	}
	else
	{
	    gptr = rptr;
	    bptr = rptr; 	/* Mild kludge to get monochrome */
	}
    
	depth = (*buffers->imgPort.portPixMap)->pixelSize;
	mask = ((1 << depth) - 1);
	
	if (depth == 32)	/* Hi Bruce! (32 bit QD) */
	{
		for (y = disp_y; y <= ysize; y++ )
		{
		    SpinCursor( (short) -1 );
		    pixidx = ((long)(ysize - y) * 
				(long)((*buffers->imgPort.portPixMap)->rowBytes & 0x7fff));
		    pixel = (char *)((*buffers->imgPort.portPixMap)->baseAddr) + pixidx;

		    for ( x = 0; x < xsize; x++ )
		    {
		        *pixel++ = 0;		/* QD32 ignores alpha */
			*pixel++ = *rptr++;
			*pixel++ = *gptr++;
			*pixel++ = *bptr++;
		    }
		    if ( buffers->pixMaxY != -2 )
		    {
			SetRect( &srcDestRect, 0, ysize - y, xsize, ysize - y + 1 );
			CopyBits( (BitMap *)(*buffers->imgPort.portPixMap), &imageWin->portBits,
				    &srcDestRect, &srcDestRect, 64, nil );
			buffers->pixMaxY = y;
		    }
		    if ( EventAvail( (short) mDownMask|mUpMask|keyDownMask, &theEvent))
		    {
			disp_y = y + 1; /* for later continuation */
			break;
		    }
		}
	}
	else
		for ( y = disp_y; y <= ysize; y++ )
		{
		    SpinCursor( (short) -1 );
		    pixidx = ((long)(ysize - y) * 
				(long)((*buffers->imgPort.portPixMap)->rowBytes & 0x7fff));
		    pixel = (char *)((*buffers->imgPort.portPixMap)->baseAddr) + pixidx;
		    pixoffset = 8;
	
		    for ( x = 0; x < xsize; x++ )
		    {
			pixcol.rgb.red = *rptr++ << 8;
			pixcol.rgb.green = *gptr++ << 8;
			pixcol.rgb.blue = *bptr++ << 8;
			pixcol.value = (short) Color2Index( &pixcol.rgb );
			
			pixoffset -= depth;
			if ( pixoffset < 0 )
			{
			    pixoffset += 8;
			    pixel++;
			}
			*pixel &= ~mask << pixoffset;
			*pixel |= (pixcol.value & mask) << pixoffset;
	
			if ( dodither )
			{
			    Index2Color( pixcol.value, &gotcol.rgb );
			    rdiff = (((long)gotcol.rgb.red - (long)pixcol.rgb.red) / 3) >> 8;
			    gdiff = (((long)gotcol.rgb.green - (long)pixcol.rgb.green) / 3) >> 8;
			    bdiff = (((long)gotcol.rgb.blue - (long)pixcol.rgb.blue) / 3) >> 8;
			    
			    cval = (int)*(rptr + xsize - 1) - rdiff;
			    *(rptr + xsize - 1) = ADJUST( cval );
			    cval = (int)*(gptr + xsize - 1) - gdiff;
			    *(gptr + xsize - 1) = ADJUST( cval );
			    cval = (int)*(bptr + xsize - 1) - bdiff;
			    *(bptr + xsize - 1) = ADJUST( cval );
	
			    cval = (int)*(rptr) - rdiff;
			    *(rptr) = ADJUST( cval );
			    cval = (int)*(gptr) - gdiff;
			    *(gptr) = ADJUST( cval );
			    cval = (int)*(bptr) - bdiff;
			    *(bptr) = ADJUST( cval);
	
			    cval = (int)*(rptr + xsize) - rdiff;
			    *(rptr + xsize) = ADJUST( cval );
			    cval = (int)*(gptr + xsize) - gdiff;
			    *(gptr + xsize) = ADJUST( cval );
			    cval = (int)*(bptr + xsize) - bdiff;
			    *(bptr + xsize) = ADJUST( cval );
			}
		    }

		    if ( buffers->pixMaxY != -2 )
		    {
			SetRect( &srcDestRect, 0, ysize - y, xsize, ysize - y + 1 );
			CopyBits( (BitMap *)(*buffers->imgPort.portPixMap), &imageWin->portBits,
				    &srcDestRect, &srcDestRect, 64, nil );
			buffers->pixMaxY = y;
		    }
		    if ( EventAvail( (short) mDownMask|mUpMask|keyDownMask, &theEvent))
		    {
			disp_y = y + 1; /* for later continuation */
			break;
		    }
		}

	SetGDevice( oldDevice );
	SetPort( (GrafPtr) oldPort );
	
	if ( y >= ysize )
	{
	    disp_y = -1;
	}
    }
    else
    {
	SetRect( &srcDestRect, 0, 0, xsize, ysize + 1 );
	CopyBits( (BitMap *)(*buffers->imgPort.portPixMap), &imageWin->portBits,
		    &srcDestRect, &srcDestRect, 64, nil );
	disp_y = -1;
    }
}

/* Code from TN120 */
createPix( buffers, xsize, ysize )
struct imgBuffers * buffers;
int xsize, ysize;
{
    Rect globRect, bRect;
    Ptr myBits;
    long sizeOfOff, offRowBytes;
    int theDepth, i, err;
    CTabHandle ourCMHandle;
    GDHandle theMaxDevice, oldDevice;
    PixMap * myPix;

    MSG("createPix");
    /* Way big for many screens.  May lose if screens are on the left. */
    SetRect( &globRect, -3000, -3000, 3000, 3000 );
    SetRect( &bRect, 0, 0, xsize, ysize );
    
    /* Figure out how much space we need
     * Call GetMaxDevice and get the pixel map from that --
     * we do this to cover the case where the pixel image
     * spans multiple devices (of possibly different depths
     */
    theMaxDevice = GetMaxDevice( &globRect );
    buffers->pixDevice = theMaxDevice;
    
    /* Set theGDevice to device with maximum pixel depth */
    oldDevice = GetGDevice();
    SetGDevice( theMaxDevice );
    
    /* Open a CGrafPort for offscreen drawing */
    OpenCPort( &buffers->imgPort );
    theDepth = (*buffers->imgPort.portPixMap)->pixelSize;
    
    /* Calculate size of pixel image */
    offRowBytes = ((((long)theDepth * (long)xsize) + 15L) / 16L) * 2L;
    sizeOfOff = (long)ysize * offRowBytes;
    myBits = (Ptr)malloc( sizeOfOff );
    if ( myBits == nil)
    {
    	fprintf( stderr, "Not enough memory for pixmap\n" );
	buffers->pixMaxY = -2;	/* No Pixmap at all */
	goto done;
    }
    
    /* Fix up location/size info */
    myPix = *buffers->imgPort.portPixMap;
    myPix->baseAddr = myBits;
    myPix->rowBytes = offRowBytes | 0x8000; /* Mark as pixmap */
    myPix->bounds = bRect;
    
    /* Clone color table from MaxDevice */
    ourCMHandle = (*(*theMaxDevice)->gdPMap)->pmTable;
    err = HandToHand((Handle *)&ourCMHandle);
    if ( err != 0 )
    {
    	fprintf( stderr, "Error cloning color table: %d\n", err );
	free( myBits );
	buffers->pixMaxY = -2;
	goto done;
    }
    
    for ( i = 0; i <= (*ourCMHandle)->ctSize; i++ )
	(*ourCMHandle)->ctTable[i].value = i;	/* Put in indices */
    /* Clear high bit of transIndex to indicate it's a PixMap color table */
    (*ourCMHandle)->ctFlags &= 0x7fff;
    
    (*buffers->imgPort.portPixMap)->pmTable = ourCMHandle;  /* Put color table into offscreen map */
    
done:
    /* Restore GDevice */
    SetGDevice( oldDevice );
}

/*
 * Process Mac events.  Spencer (wisely) did all of this with the TranSkel
 * package.  I've re-invented it here to reduce the volume of distributed
 * source code.
 *
 * Friends, let me quote from the MPW manual:
 *
 * "The creation of windows, use of graphics, and event processing by tools is
 * largely unexplored area in the MPW environment.  MPW aims to support these
 * types of tools; however, little work has been done so far in this area, and
 * unknown restrictions may exist."
 *
 * Makes ya feel good, huh?
 */

#define GrayRgn (*((RgnHandle *) 0x9ee))  /* Indulge.  Have a magic cookie. */

processEvent( theEvent )
EventRecord * theEvent;
{
    Point evtPt;
    GrafPtr evtPort, tmpPort;
    short evtPart;
    Rect r;

    evtPt = theEvent->where;

    switch ( theEvent->what )
    {
    case nullEvent:
	break;

    case mouseDown:
	evtPart = FindWindow( evtPt, (WindowPtr *) &evtPort );

	switch ( evtPart )
	{

	case inSysWindow:
	    SystemClick( theEvent, evtPort );
	    break;

        case inGrow:
	    /* DoGrow */
	    break;

	case inDrag:
	    r = (*GrayRgn)->rgnBBox;
	    InsetRect( &r, 4, 4 );
	    DragWindow( evtPort, evtPt, &r );
	    break;

	case inGoAway:
	    doneFlag = true;
	    break;

	case inContent:
	{
	    if (evtPort != FrontWindow() )
		SelectWindow( evtPort );
	    else
		imgWinMouse( evtPt );
	    break;
	}
    	}

    case keyDown:
    	if ((theEvent->message & charCodeMask) == 'q')
	{
	    doneFlag = true;
	}	    
    case updateEvt:

	/* Note - while running as an MPW tool, this will get update events
	 * for MPW windows.  Since it can't update them for MPW, it
	 * ignores them until them until it exits.
	 */
	GetPort( &tmpPort );
	SetPort( imageWin );
	BeginUpdate( imageWin );
	imgWinUpdate ();
	EndUpdate( imageWin );
	SetPort( tmpPort );
	break;

    case activateEvt:
	SetPort( imageWin );
	break;
    }
}

main( argc, argv, env )
int argc;
char **argv;
char **env;
{
    Boolean haveEvent;
    EventRecord theEvent;
    short eventMask = everyEvent;
    int ditherFlag = 0;
    char * filename = NULL;
    
    doneFlag = false;
    
/*     FlushEvents (everyEvent - diskMask, 0 ); */
    InitGraf (&qd.thePort);
    InitCursorCtl( NULL );

    if (! scanargs( argc, argv, "% d%- rlefile%s", &ditherFlag, &filename ))
	exit(-1);
    dodither = (Boolean) ! ditherFlag;

    if (filename)
    {
	rleFile = fopen( argv[1], "r" );
	strcpy( rleName, argv[1] );
    }
    else
    {
	rleFile = stdin;
	strcpy( rleName, "RLE Image" );
    }

    if (! rleFile)
    {
	fprintf( stderr, "Can't open %s\n", argv[1] );
	exit(-1);
    }

    showImage ();
    fclose( rleFile );

    doneFlag = false;

    Show_Cursor( ARROW_CURSOR );
    while ( ! doneFlag )
    {
	SystemTask();

	haveEvent = GetNextEvent( eventMask, &theEvent );

	if (haveEvent) processEvent( &theEvent );
    }
    imgWinClose();
    fprintf( stderr, "Done.\n" );
    exit( 0 );
}


