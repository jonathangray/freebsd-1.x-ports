#ifdef HAVE_TIFF
/*
 * xvtiff.c - load routine for 'TIFF' format pictures
 *
 * LoadTIFF(fname, numcols)  -  load a TIFF file
 */

#ifndef va_start
#define NEEDSARGS
#endif

#include "xv.h"
#include "tiffio.h"     /* has to be after xv.h, as it needs varargs/stdarg */


static byte *loadPalette();
static byte *loadColor();
static int loadImage();
static void _TIFFerr();
static void _TIFFwarn();

static long filesize;
static byte *rmap, *gmap, *bmap;
static char *filename;



/*******************************************/
int LoadTIFF(fname, pinfo)
     char *fname;
     PICINFO *pinfo;
/*******************************************/
{
  /* returns '1' on success, '0' on failure */

  TIFF  *tif;
  long   w, h;
  short	 bps, spp, photo, orient;
  FILE  *fp;
  byte  *pic8;
  char  *desc, oldpath[MAXPATHLEN+1], tmppath[MAXPATHLEN+1], *sp;

  pinfo->type = PIC8;

  TIFFSetErrorHandler(_TIFFerr);
  TIFFSetWarningHandler(_TIFFwarn);

  /* open the stream to find out filesize (for info box) */
  fp=fopen(fname,"r");
  if (!fp) {
    TIFFError("LoadTIFF()", "couldn't open file");
    return 0;
  }

  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fclose(fp);

  rmap = pinfo->r;  gmap = pinfo->g;  bmap = pinfo->b;

  /* a kludge:  temporarily cd to the directory that the file is in (if
     the file has a path), and cd back when done, as I can't make the
     various TIFF error messages print the simple filename */

  filename = fname;    /* use fullname unless it all works out */
  oldpath[0] = '\0';
  if (fname[0] == '/') {
    GETWD(oldpath);
    strcpy(tmppath, fname);
    sp = BaseName(tmppath);
    if (sp != tmppath) {
      sp[-1] = '\0';     /* truncate before last '/' char */
      if (chdir(tmppath)) {
	oldpath[0] = '\0';
      }
      else filename = BaseName(fname);
    }
  }
      
    

  tif=TIFFOpen(filename,"r");
  if (!tif) return 0;

  /* flip orientation so that image comes in X order */
  TIFFGetFieldDefaulted(tif, TIFFTAG_ORIENTATION, &orient);
  switch (orient) {
  case ORIENTATION_TOPLEFT:
  case ORIENTATION_TOPRIGHT:
  case ORIENTATION_LEFTTOP:
  case ORIENTATION_RIGHTTOP:   orient = ORIENTATION_BOTLEFT;   break;

  case ORIENTATION_BOTRIGHT:
  case ORIENTATION_BOTLEFT:
  case ORIENTATION_RIGHTBOT:
  case ORIENTATION_LEFTBOT:    orient = ORIENTATION_TOPLEFT;   break;
  }

  TIFFSetField(tif, TIFFTAG_ORIENTATION, orient);

  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
  TIFFGetFieldDefaulted(tif, TIFFTAG_BITSPERSAMPLE, &bps);
  TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photo);
  TIFFGetFieldDefaulted(tif, TIFFTAG_SAMPLESPERPIXEL, &spp);
  if (spp == 1) {
      pic8 = loadPalette(tif, w, h, photo, bps, pinfo);
  } else {
      pic8 = loadColor(tif, w, h, photo, bps, pinfo);
  }

  /* try to get comments, if any */
  pinfo->comment = (char *) NULL;
  
  desc = (char *) NULL;

  TIFFGetField(tif, TIFFTAG_IMAGEDESCRIPTION, &desc);
  if (desc && strlen(desc)>0) {
    /* kludge:  tiff library seems to return bizarre comments */
    if (strlen(desc)==4 && strcmp(desc, "\367\377\353\370")==0) {} 
    else {
      pinfo->comment = (char *) malloc(strlen(desc) + 1);
      if (pinfo->comment) strcpy(pinfo->comment, desc);
    }
  }
    
  TIFFClose(tif);

  /* un-kludge */
  if (oldpath[0] != '\0') chdir(oldpath);

  pinfo->pic = pic8;
  pinfo->w = w;  pinfo->h = h;
  pinfo->frmType = F_TIFF;


  if (pinfo->pic) return 1;

  /* failed.  if we malloc'd a comment, free it */
  if (pinfo->comment) free(pinfo->comment);
  pinfo->comment = (char *) NULL;

  return 0;
}  


/*******************************************/
static byte *loadPalette(tif, w, h, photo, bps, pinfo)
     TIFF *tif;
     long  w,h;
     int   photo,bps;
     PICINFO *pinfo;
{
  byte *pic8;

  switch (photo) {
  case PHOTOMETRIC_PALETTE:
    pinfo->colType = F_FULLCOLOR;
    sprintf(pinfo->fullInfo, "TIFF, %u-bit, palette format.  (%ld bytes)",
	    bps, filesize);
    break;

  case PHOTOMETRIC_MINISWHITE:
  case PHOTOMETRIC_MINISBLACK:
    pinfo->colType = (bps==1) ? F_BWDITHER : F_GREYSCALE;
    sprintf(pinfo->fullInfo,"TIFF, %u-bit, %s format.  (%ld bytes)", 
	    bps,
	    photo == PHOTOMETRIC_MINISWHITE ? "min-is-white" :
	    "min-is-black",
	    filesize);
    break;
  }

  sprintf(pinfo->shrtInfo, "%lux%lu TIFF.",w,h);

  pic8 = (byte *) malloc(w*h);
  if (!pic8) FatalError("loadPalette() - couldn't malloc 'pic8'");

  if (loadImage(tif, w, h, pic8, 0)) return pic8;

  return (byte *) NULL;
}


/*******************************************/
static byte *loadColor(tif, w, h, photo, bps, pinfo)
     TIFF *tif;
     long  w,h;
     int   photo,bps;
     PICINFO *pinfo;
{
  byte *pic24, *pic8;

  pinfo->colType = F_FULLCOLOR;
  sprintf(pinfo->fullInfo, "TIFF, %u-bit, %s format.  (%ld bytes)", 
	  bps,
	  (photo == PHOTOMETRIC_RGB ?	"RGB" :
	   photo == PHOTOMETRIC_YCBCR ?	"YCbCr" :
	   "???"),
	  filesize);

  sprintf(pinfo->shrtInfo, "%lux%lu TIFF.",w,h);

  /* allocate 24-bit image */
  pic24 = (byte *) malloc(w*h*3);
  if (!pic24) FatalError("loadColor() - couldn't malloc 'pic24'");

  pic8 = (byte *) NULL;

  if (loadImage(tif, w, h, pic24, 0)) {
    pinfo->type = PIC24;
    pic8 = pic24;
  }
  else free(pic24);

  return pic8;
}


/*******************************************/
static void _TIFFerr(module, fmt, ap)
     char *module;
     char *fmt;
     va_list ap;
{
  char buf[2048];
  char *cp = buf;

  if (module != NULL) {
    sprintf(cp, "%s: ", module);
    cp = (char *) strchr(cp, '\0');
  }

  vsprintf(cp, fmt, ap);
  strcat(cp, ".");

  SetISTR(ISTR_WARNING,buf);
}


/*******************************************/
static void _TIFFwarn(module, fmt, ap)
     char *module;
     char *fmt;
     va_list ap;
{
  char buf[2048];
  char *cp = buf;

  if (module != NULL) {
    sprintf(cp, "%s: ", module);
    cp = (char *) strchr(cp, '\0');
  }
  strcpy(cp, "Warning, ");
  cp = (char *) strchr(cp, '\0');
  vsprintf(cp, fmt, ap);
  strcat(cp, ".");

  SetISTR(ISTR_WARNING,buf);
}


typedef	byte RGBvalue;

static	u_short bitspersample;
static	u_short samplesperpixel;
static	u_short photometric;
static	u_short orientation;

/* colormap for pallete images */
static	u_short *redcmap, *greencmap, *bluecmap;
static	int stoponerr;			/* stop on read error */

/* YCbCr support */
static	u_short YCbCrHorizSampling;
static	u_short YCbCrVertSampling;
static	float *YCbCrCoeffs;
static	float *refBlackWhite;

static	byte **BWmap;
static	byte **PALmap;

static	int gt();

static int loadImage(tif, rwidth, rheight, raster, stop)
     TIFF *tif;
     u_long rwidth, rheight;
     byte *raster;
     int stop;
{
  int ok;
  u_long width, height;

  TIFFGetFieldDefaulted(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
  switch (bitspersample) {
  case 1: 
  case 2: 
  case 4:
  case 8: 
  case 16:  break;

  default:
    TIFFError(TIFFFileName(tif),
	      "Sorry, can not handle %d-bit pictures", bitspersample);
    return (0);
  }


  TIFFGetFieldDefaulted(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
  switch (samplesperpixel) {
  case 1: 
  case 3: 
  case 4:  break;

  default:
    TIFFError(TIFFFileName(tif),
	      "Sorry, can not handle %d-channel images", samplesperpixel);
    return (0);
  }


  if (!TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric)) {
    switch (samplesperpixel) {
    case 1:  photometric = PHOTOMETRIC_MINISBLACK;   break;

    case 3:
    case 4:  photometric = PHOTOMETRIC_RGB;          break;

    default:
      TIFFError(TIFFFileName(tif),
		"Missing needed \"PhotometricInterpretation\" tag");
      return (0);
    }

    TIFFError(TIFFFileName(tif),
	      "No \"PhotometricInterpretation\" tag, assuming %s\n",
	      photometric == PHOTOMETRIC_RGB ? "RGB" : "min-is-black");
  }

  /* XXX maybe should check photometric? */
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);

  /* XXX verify rwidth and rheight against width and height */
  stoponerr = stop;
  BWmap = NULL;
  PALmap = NULL;
  ok = gt(tif, rwidth, height, raster + (rheight-height)*rwidth);
  if (BWmap)
    free((char *)BWmap);
  if (PALmap)
    free((char *)PALmap);
  return (ok);
}


static int checkcmap(n, r, g, b)
     int n;
     u_short *r, *g, *b;
{
  while (n-- >= 0)
    if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256) return (16);

  TIFFWarning(filename, "Assuming 8-bit colormap");
  return (8);
}


static	gtTileContig();
static	gtTileSeparate();
static	gtStripContig();
static	gtStripSeparate();
static	void initYCbCrConversion();

static gt(tif, w, h, raster)
     TIFF *tif;
     int w, h;
     u_long *raster;
{
  u_short minsamplevalue, maxsamplevalue, planarconfig;
  RGBvalue *Map;
  int bpp = 1, e;
  int x, range;

  TIFFGetFieldDefaulted(tif, TIFFTAG_MINSAMPLEVALUE, &minsamplevalue);
  TIFFGetFieldDefaulted(tif, TIFFTAG_MAXSAMPLEVALUE, &maxsamplevalue);
  Map = NULL;
  
  switch (photometric) {
  case PHOTOMETRIC_YCBCR:
    TIFFGetFieldDefaulted(tif, TIFFTAG_YCBCRCOEFFICIENTS,
			  &YCbCrCoeffs);
    TIFFGetFieldDefaulted(tif, TIFFTAG_YCBCRSUBSAMPLING,
			  &YCbCrHorizSampling, &YCbCrVertSampling);
    TIFFGetFieldDefaulted(tif, TIFFTAG_REFERENCEBLACKWHITE,
			  &refBlackWhite);
    initYCbCrConversion();
    /* fall thru... */
	
  case PHOTOMETRIC_RGB:
    bpp *= 3;
    if (minsamplevalue == 0 && maxsamplevalue == 255)
      break;
	
    /* fall thru... */
  case PHOTOMETRIC_MINISBLACK:
  case PHOTOMETRIC_MINISWHITE:
    range = maxsamplevalue - minsamplevalue;
    Map = (RGBvalue *)malloc((range + 1) * sizeof (RGBvalue));
    if (Map == NULL) {
      TIFFError(filename,
		"No space for photometric conversion table");
      return (0);
    }

    if (photometric == PHOTOMETRIC_MINISWHITE) {
      for (x = 0; x <= range; x++)
	rmap[x] = gmap[x] = bmap[x] =
	  Map[x] = (255*(range-x))/range;
    } else {
      for (x = 0; x <= range; x++)
	rmap[x] = gmap[x] = bmap[x] = Map[x] = (255*x)/range;
    }

    if (photometric != PHOTOMETRIC_RGB && bitspersample <= 8) {
      /*
       * Use photometric mapping table to construct
       * unpacking tables for samples <= 8 bits.
       */
      if (!makebwmap())
	return (0);
      /* no longer need Map, free it */
      free((char *)Map);
      Map = NULL;
    }
    break;

  case PHOTOMETRIC_PALETTE:
    if (!TIFFGetField(tif, TIFFTAG_COLORMAP,
		      &redcmap, &greencmap, &bluecmap)) {
      TIFFError(filename,
		"Missing required \"Colormap\" tag");
      return (0);
    }

    /*
     * Convert 16-bit colormap to 8-bit (unless it looks
     * like an old-style 8-bit colormap).
     */
    range = (1<<bitspersample)-1;
    if (checkcmap(range, redcmap, greencmap, bluecmap) == 16) {

#define	CVT(x)		(((x) * 255) / ((1L<<16)-1))

      for (x = range; x >= 0; x--) {
	rmap[x] = CVT(redcmap[x]);
	gmap[x] = CVT(greencmap[x]);
	bmap[x] = CVT(bluecmap[x]);
      }
    } else {
      for (x = range; x >= 0; x--) {
	rmap[x] = redcmap[x];
	gmap[x] = greencmap[x];
	bmap[x] = bluecmap[x];
      }
    }

    if (bitspersample <= 8) {
      /*
       * Use mapping table to construct
       * unpacking tables for samples < 8 bits.
       */
      if (!makecmap())
	return (0);
    }
    break;

  default:
    TIFFError(filename, "Unknown photometric tag %u", photometric);
    return (0);
  }

  TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarconfig);
  if (planarconfig == PLANARCONFIG_SEPARATE && samplesperpixel > 1) {
    e = TIFFIsTiled(tif) ?
        gtTileSeparate(tif, raster, Map, h, w, bpp) :
	gtStripSeparate(tif, raster, Map, h, w, bpp);
  } else {
    e = TIFFIsTiled(tif) ? 
        gtTileContig(tif, raster, Map, h, w, bpp) :
	gtStripContig(tif, raster, Map, h, w, bpp);
  }

  if (Map) free((char *)Map);
  return (e);
}


u_long setorientation(tif, h)
     TIFF *tif;
     u_long h;
{
  u_long y;

  TIFFGetFieldDefaulted(tif, TIFFTAG_ORIENTATION, &orientation);
  switch (orientation) {
  case ORIENTATION_BOTRIGHT:
  case ORIENTATION_RIGHTBOT:	/* XXX */
  case ORIENTATION_LEFTBOT:	/* XXX */
    TIFFWarning(filename, "using bottom-left orientation");
    orientation = ORIENTATION_BOTLEFT;

    /* fall thru... */
  case ORIENTATION_BOTLEFT:
    y = 0;
    break;

  case ORIENTATION_TOPRIGHT:
  case ORIENTATION_RIGHTTOP:	/* XXX */
  case ORIENTATION_LEFTTOP:	/* XXX */
  default:
    TIFFWarning(filename, "using top-left orientation");
    orientation = ORIENTATION_TOPLEFT;
    /* fall thru... */
  case ORIENTATION_TOPLEFT:
    y = h-1;
    break;
  }
  return (y);
}



#if USE_PROTOTYPES
typedef void (*tileContigRoutine)
    (byte*, u_char*, RGBvalue*, u_long, u_long, int, int);
static tileContigRoutine pickTileContigCase(RGBvalue*);
#else
typedef void (*tileContigRoutine)();
static tileContigRoutine pickTileContigCase();
#endif



/*
 * Get an tile-organized image that has
 *	PlanarConfiguration contiguous if SamplesPerPixel > 1
 * or
 *	SamplesPerPixel == 1
 */	
static
gtTileContig(tif, raster, Map, h, w, bpp)
     TIFF *tif;
     byte *raster;
     RGBvalue *Map;
     u_long h, w;
     int bpp;
{
  u_long col, row, y;
  u_long tw, th;
  u_char *buf;
  int fromskew, toskew;
  u_int nrow;
  tileContigRoutine put;

  put = pickTileContigCase(Map);
  if (put == 0) return (0);

  buf = (u_char *)malloc(TIFFTileSize(tif));
  if (buf == 0) {
    TIFFError(filename, "No space for tile buffer");
    return (0);
  }

  TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tw);
  TIFFGetField(tif, TIFFTAG_TILELENGTH, &th);
  y = setorientation(tif, h);
  toskew = (orientation == ORIENTATION_TOPLEFT ? -tw + -w : -tw + w);

  for (row = 0; row < h; row += th) {
    nrow = (row + th > h ? h - row : th);
    for (col = 0; col < w; col += tw) {
      if (TIFFReadTile(tif, buf, col, row, 0, 0) < 0 && stoponerr) break;

      if (col + tw > w) {
	/*
	 * Tile is clipped horizontally.  Calculate
	 * visible portion and skewing factors.
	 */
	u_long npix = w - col;
	fromskew = tw - npix;
	(*put)(raster + (y*w + col)*bpp, buf, Map,
	       npix, nrow,
	       fromskew, (toskew + fromskew)*bpp);
      } else
	(*put)(raster + (y*w + col)*bpp, buf, Map,
	       tw, nrow,
	       0, toskew*bpp);
    }

    y += (orientation == ORIENTATION_TOPLEFT ? -nrow : nrow);
  }
  free(buf);
  return (1);
}


#if USE_PROTOTYPES
typedef void (*tileSeparateRoutine)
    (byte*, u_char*, u_char*, u_char*, RGBvalue*, u_long, u_long, int, int);
static tileSeparateRoutine pickTileSeparateCase(RGBvalue*);
#else
typedef void (*tileSeparateRoutine)();
static tileSeparateRoutine pickTileSeparateCase();
#endif

/*
 * Get an tile-organized image that has
 *	 SamplesPerPixel > 1
 *	 PlanarConfiguration separated
 * We assume that all such images are RGB.
 */	
static gtTileSeparate(tif, raster, Map, h, w, bpp)
     TIFF *tif;
     byte *raster;
     RGBvalue *Map;
     u_long h, w;
     int bpp;
{
	u_long col, row, y;
	u_long tw, th;
	u_char *buf;
	u_char *r, *g, *b;
	int tilesize;
	int fromskew, toskew;
	u_int nrow;
	tileSeparateRoutine put;

	put = pickTileSeparateCase(Map);
	if (put == 0)
		return (0);
	tilesize = TIFFTileSize(tif);
	buf = (u_char *)malloc(3*tilesize);
	if (buf == 0) {
		TIFFError(filename, "No space for tile buffer");
		return (0);
	}
	r = buf;
	g = r + tilesize;
	b = g + tilesize;
	TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tw);
	TIFFGetField(tif, TIFFTAG_TILELENGTH, &th);
	y = setorientation(tif, h);
	toskew = (orientation == ORIENTATION_TOPLEFT ? -tw + -w : -tw + w);
	for (row = 0; row < h; row += th) {
		nrow = (row + th > h ? h - row : th);
		for (col = 0; col < w; col += tw) {
			if (TIFFReadTile(tif, r, col, row,0,0) < 0 && stoponerr)
				break;
			if (TIFFReadTile(tif, g, col, row,0,1) < 0 && stoponerr)
				break;
			if (TIFFReadTile(tif, b, col, row,0,2) < 0 && stoponerr)
				break;
			if (col + tw > w) {
				/*
				 * Tile is clipped horizontally.  Calculate
				 * visible portion and skewing factors.
				 */
				u_long npix = w - col;
				fromskew = tw - npix;
				(*put)(raster + (y*w + col)*bpp, r, g, b, Map,
				    npix, nrow,
				    fromskew, (toskew + fromskew)*bpp);
			} else
				(*put)(raster + (y*w + col)*bpp, r, g, b, Map,
				    tw, nrow, 0, toskew*bpp);
		}
		y += (orientation == ORIENTATION_TOPLEFT ? -nrow : nrow);
	}
	free(buf);
	return (1);
}

/*
 * Get a strip-organized image that has
 *	PlanarConfiguration contiguous if SamplesPerPixel > 1
 * or
 *	SamplesPerPixel == 1
 */	
static
gtStripContig(tif, raster, Map, h, w, bpp)
	TIFF *tif;
	byte *raster;
	RGBvalue *Map;
	u_long h, w;
	int bpp;
{
	u_long row, y, nrow;
	u_char *buf;
	tileContigRoutine put;
	u_long rowsperstrip;
	u_long imagewidth;
	int scanline;
	int fromskew, toskew;

	put = pickTileContigCase(Map);
	if (put == 0)
		return (0);
	buf = (u_char *)malloc(TIFFStripSize(tif));
	if (buf == 0) {
		TIFFError(filename, "No space for strip buffer");
		return (0);
	}
	y = setorientation(tif, h);
	toskew = (orientation == ORIENTATION_TOPLEFT ? -w + -w : -w + w);
	TIFFGetFieldDefaulted(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip);
	TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &imagewidth);
	scanline = TIFFScanlineSize(tif);
	fromskew = (w < imagewidth ? imagewidth - w : 0);
	for (row = 0; row < h; row += rowsperstrip) {
		nrow = (row + rowsperstrip > h ? h - row : rowsperstrip);
		if (TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, row, 0),
		    buf, nrow*scanline) < 0 && stoponerr)
			break;
		(*put)(raster + y*w*bpp, buf, Map, w, nrow,
		    fromskew, toskew*bpp);
		y += (orientation == ORIENTATION_TOPLEFT ? -nrow : nrow);
	}
	free(buf);
	return (1);
}

/*
 * Get a strip-organized image with
 *	 SamplesPerPixel > 1
 *	 PlanarConfiguration separated
 * We assume that all such images are RGB.
 */
static
gtStripSeparate(tif, raster, Map, h, w, bpp)
	TIFF *tif;
	byte *raster;
	register RGBvalue *Map;
	u_long h, w;
	int bpp;
{
	u_char *buf;
	u_char *r, *g, *b;
	u_long row, y, nrow;
	int scanline;
	tileSeparateRoutine put;
	u_long rowsperstrip;
	u_long imagewidth;
	u_int stripsize;
	int fromskew, toskew;

	stripsize = TIFFStripSize(tif);
	r = buf = (u_char *)malloc(3*stripsize);
	if (buf == 0)
		return (0);
	g = r + stripsize;
	b = g + stripsize;
	put = pickTileSeparateCase(Map);
	if (put == 0) {
		TIFFError(filename, "Can not handle format");
		return (0);
	}
	y = setorientation(tif, h);
	toskew = (orientation == ORIENTATION_TOPLEFT ? -w + -w : -w + w);
	TIFFGetFieldDefaulted(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip);
	TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &imagewidth);
	scanline = TIFFScanlineSize(tif);
	fromskew = (w < imagewidth ? imagewidth - w : 0);
	for (row = 0; row < h; row += rowsperstrip) {
		nrow = (row + rowsperstrip > h ? h - row : rowsperstrip);
		if (TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, row, 0),
		    r, nrow*scanline) < 0 && stoponerr)
			break;
		if (TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, row, 1),
		    g, nrow*scanline) < 0 && stoponerr)
			break;
		if (TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, row, 2),
		    b, nrow*scanline) < 0 && stoponerr)
			break;
		(*put)(raster + y*w*bpp, r, g, b, Map, w, nrow,
		    fromskew, toskew*bpp);
		y += (orientation == ORIENTATION_TOPLEFT ? -nrow : nrow);
	}
	free(buf);
	return (1);
}

/*
 * Greyscale images with less than 8 bits/sample are handled
 * with a table to avoid lots of shifts and masks.  The table
 * is setup so that put*bwtile (below) can retrieve 8/bitspersample
 * pixel values simply by indexing into the table with one
 * number.
 */
makebwmap()
{
	register int i;
	int nsamples = 8 / bitspersample;
	register byte *p;

	BWmap = (byte **)malloc(
	    256*sizeof (byte *)+(256*nsamples*sizeof(byte)));
	if (BWmap == NULL) {
		TIFFError(filename, "No space for B&W mapping table");
		return (0);
	}
	p = (byte *)(BWmap + 256);
	for (i = 0; i < 256; i++) {
		BWmap[i] = p;
		switch (bitspersample) {
#define	GREY(x)	*p++ = x;
		case 1:
			GREY(i>>7);
			GREY((i>>6)&1);
			GREY((i>>5)&1);
			GREY((i>>4)&1);
			GREY((i>>3)&1);
			GREY((i>>2)&1);
			GREY((i>>1)&1);
			GREY(i&1);
			break;
		case 2:
			GREY(i>>6);
			GREY((i>>4)&3);
			GREY((i>>2)&3);
			GREY(i&3);
			break;
		case 4:
			GREY(i>>4);
			GREY(i&0xf);
			break;
		case 8:
			GREY(i);
			break;
		}
#undef	GREY
	}
	return (1);
}

/*
 * Palette images with <= 8 bits/sample are handled
 * with a table to avoid lots of shifts and masks.  The table
 * is setup so that put*cmaptile (below) can retrieve 8/bitspersample
 * pixel values simply by indexing into the table with one
 * number.
 */
makecmap()
{
	register int i;
	int nsamples = 8 / bitspersample;
	register byte *p;

	PALmap = (byte **)malloc(
	    256*sizeof (byte *)+(256*nsamples*sizeof(byte)));
	if (PALmap == NULL) {
		TIFFError(filename, "No space for Palette mapping table");
		return (0);
	}
	p = (byte *)(PALmap + 256);
	for (i = 0; i < 256; i++) {
		PALmap[i] = p;
#define	CMAP(x)	*p++ = x;
		switch (bitspersample) {
		case 1:
			CMAP(i>>7);
			CMAP((i>>6)&1);
			CMAP((i>>5)&1);
			CMAP((i>>4)&1);
			CMAP((i>>3)&1);
			CMAP((i>>2)&1);
			CMAP((i>>1)&1);
			CMAP(i&1);
			break;
		case 2:
			CMAP(i>>6);
			CMAP((i>>4)&3);
			CMAP((i>>2)&3);
			CMAP(i&3);
			break;
		case 4:
			CMAP(i>>4);
			CMAP(i&0xf);
			break;
		case 8:
			CMAP(i);
			break;
		}
#undef CMAP
	}
	return (1);
}

/*
 * The following routines move decoded data returned
 * from the TIFF library into rasters filled with packed
 * ABGR pixels (i.e. suitable for passing to lrecwrite.)
 *
 * The routines have been created according to the most
 * important cases and optimized.  pickTileContigCase and
 * pickTileSeparateCase analyze the parameters and select
 * the appropriate "put" routine to use.
 */
#define	REPEAT8(op)	REPEAT4(op); REPEAT4(op)
#define	REPEAT4(op)	REPEAT2(op); REPEAT2(op)
#define	REPEAT2(op)	op; op
#define	CASE8(x,op)				\
	switch (x) {				\
	case 7: op; case 6: op; case 5: op;	\
	case 4: op; case 3: op; case 2: op;	\
	case 1: op;				\
	}
#define	CASE4(x,op)	switch (x) { case 3: op; case 2: op; case 1: op; }

#define	UNROLL8(w, op1, op2) {		\
	register u_long x;		\
	for (x = w; x >= 8; x -= 8) {	\
		op1;			\
		REPEAT8(op2);		\
	}				\
	if (x > 0) {			\
		op1;			\
		CASE8(x,op2);		\
	}				\
}
#define	UNROLL4(w, op1, op2) {		\
	register u_long x;		\
	for (x = w; x >= 4; x -= 4) {	\
		op1;			\
		REPEAT4(op2);		\
	}				\
	if (x > 0) {			\
		op1;			\
		CASE4(x,op2);		\
	}				\
}
#define	UNROLL2(w, op1, op2) {		\
	register u_long x;		\
	for (x = w; x >= 2; x -= 2) {	\
		op1;			\
		REPEAT2(op2);		\
	}				\
	if (x) {			\
		op1;			\
		op2;			\
	}				\
}
			

#define	SKEW(r,g,b,skew)	{ r += skew; g += skew; b += skew; }

/*
 * 8-bit palette => colormap/RGB
 */
static void
put8bitcmaptile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	while (h-- > 0) {
		UNROLL8(w,0, *cp++ = PALmap[*pp++][0]);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 4-bit palette => colormap/RGB
 */
static void
put4bitcmaptile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 2;
	while (h-- > 0) {
		UNROLL2(w, bw = PALmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 2-bit palette => colormap/RGB
 */
static void
put2bitcmaptile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 4;
	while (h-- > 0) {
		UNROLL4(w, bw = PALmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 1-bit palette => colormap/RGB
 */
static void
put1bitcmaptile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 8;
	while (h-- > 0) {
		UNROLL8(w, bw = PALmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 8-bit greyscale => colormap/RGB
 */
static void
putgreytile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	while (h-- > 0) {
		register u_long x;
		for (x = w; x-- > 0;)
			*cp++ = BWmap[*pp++][0];
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 1-bit bilevel => colormap/RGB
 */
static void
put1bitbwtile(cp, pp, Map, w, h, fromskew, toskew)
	byte *cp;
	u_char *pp;
	RGBvalue **Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 8;
	while (h-- > 0) {
		UNROLL8(w, bw = BWmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 2-bit greyscale => colormap/RGB
 */
static void
put2bitbwtile(cp, pp, Map, w, h, fromskew, toskew)
	byte *cp;
	u_char *pp;
	RGBvalue **Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 4;
	while (h-- > 0) {
		UNROLL4(w, bw = BWmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 4-bit greyscale => colormap/RGB
 */
static void
put4bitbwtile(cp, pp, Map, w, h, fromskew, toskew)
	byte *cp;
	u_char *pp;
	RGBvalue **Map;
	u_long w, h;
	int fromskew, toskew;
{
	register byte *bw;

	fromskew /= 2;
	while (h-- > 0) {
		UNROLL2(w, bw = BWmap[*pp++], *cp++ = *bw++);
		cp += toskew;
		pp += fromskew;
	}
}

/*
 * 8-bit packed samples => RGB
 */
static void
putRGBcontig8bittile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	fromskew *= samplesperpixel;
	if (Map) {
		while (h-- > 0) {
			register u_long x;
			for (x = w; x-- > 0;) {
				*cp++ = Map[pp[0]];
				*cp++ = Map[pp[1]];
				*cp++ = Map[pp[2]];
				pp += samplesperpixel;
			}
			pp += fromskew;
			cp += toskew;
		}
	} else {
		while (h-- > 0) {
			UNROLL8(w,0,
			    *cp++ = pp[0];
			    *cp++ = pp[1];
			    *cp++ = pp[2];
			    pp += samplesperpixel);
			cp += toskew;
			pp += fromskew;
		}
	}
}

/*
 * 16-bit packed samples => RGB
 */
static void
putRGBcontig16bittile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_short *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	register u_int x;

	fromskew *= samplesperpixel;
	if (Map) {
		while (h-- > 0) {
			for (x = w; x-- > 0;) {
				*cp++ = Map[pp[0]];
				*cp++ = Map[pp[1]];
				*cp++ = Map[pp[2]];
				pp += samplesperpixel;
			}
			cp += toskew;
			pp += fromskew;
		}
	} else {
		while (h-- > 0) {
			for (x = w; x-- > 0;) {
				*cp++ = pp[0];
				*cp++ = pp[1];
				*cp++ = pp[2];
				pp += samplesperpixel;
			}
			cp += toskew;
			pp += fromskew;
		}
	}
}

/*
 * 8-bit unpacked samples => RGB
 */
static void
putRGBseparate8bittile(cp, r, g, b, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *r, *g, *b;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;

{
	if (Map) {
		while (h-- > 0) {
			register u_long x;
			for (x = w; x > 0; x--) {
				*cp++ = Map[*r++];
				*cp++ = Map[*g++];
				*cp++ = Map[*b++];
			}
			SKEW(r, g, b, fromskew);
			cp += toskew;
		}
	} else {
		while (h-- > 0) {
			UNROLL8(w,0,
			    *cp++ = *r++;
			    *cp++ = *g++;
			    *cp++ = *b++;
			);
			SKEW(r, g, b, fromskew);
			cp += toskew;
		}
	}
}

/*
 * 16-bit unpacked samples => RGB
 */
static void
putRGBseparate16bittile(cp, r, g, b, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_short *r, *g, *b;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	register u_long x;

	if (Map) {
		while (h-- > 0) {
			for (x = w; x > 0; x--) {
				*cp++ = Map[*r++];
				*cp++ = Map[*g++];
				*cp++ = Map[*b++];
			}
			SKEW(r, g, b, fromskew);
			cp += toskew;
		}
	} else {
		while (h-- > 0) {
			for (x = 0; x < w; x++) {
				*cp++ = *r++;
				*cp++ = *g++;
				*cp++ = *b++;
			}
			SKEW(r, g, b, fromskew);
			cp += toskew;
		}
	}
}

/* #define Code2V(c, RB, RW, CR)  ((((c)-(int)RB)*(float)CR)/(float)(RW-RB)) */
#define Code2V(c, RB, RW, CR)     ((((c)-RB)*(float)CR)/(float)(RW-RB))

#define	CLAMP(f,min,max) \
    (int)((f)+.5 < (min) ? (min) : (f)+.5 > (max) ? (max) : (f)+.5)

#define	LumaRed		YCbCrCoeffs[0]
#define	LumaGreen	YCbCrCoeffs[1]
#define	LumaBlue	YCbCrCoeffs[2]

static	float D1, D2;
static	float D3, D4;

static void
initYCbCrConversion()
{
	D1 = 2 - 2*LumaRed;
	D2 = D1*LumaRed / LumaGreen;
	D3 = 2 - 2*LumaBlue;
	D4 = D2*LumaBlue / LumaGreen;
}

static void
putRGBContigYCbCrClump(cp, pp, cw, ch, w, n, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	int cw, ch;
	u_long w;
	int n, fromskew, toskew;
{
	float Cb, Cr;
	int j, k;

	Cb = Code2V(pp[n],   refBlackWhite[2], refBlackWhite[3], 127);
	Cr = Code2V(pp[n+1], refBlackWhite[4], refBlackWhite[5], 127);
	for (j = 0; j < ch; j++) {
		for (k = 0; k < cw; k++) {
			float Y, R, G, B;
			Y = Code2V(*pp++,
			    refBlackWhite[0], refBlackWhite[1], 255);
			R = Y + Cr*D1;
			B = Y + Cb*D3;
			G = Y - Cb*D4 - Cr*D2;
			cp[3*k+0] = CLAMP(R,0,255);
			cp[3*k+1] = CLAMP(G,0,255);
			cp[3*k+2] = CLAMP(B,0,255);
		}
		cp += 3*w+toskew;
		pp += fromskew;
	}
}
#undef LumaBlue
#undef LumaGreen
#undef LumaRed
#undef CLAMP
#undef Code2V

/*
 * 8-bit packed YCbCr samples => RGB
 */
static void
putcontig8bitYCbCrtile(cp, pp, Map, w, h, fromskew, toskew)
	register byte *cp;
	register u_char *pp;
	register RGBvalue *Map;
	u_long w, h;
	int fromskew, toskew;
{
	u_int Coff = YCbCrVertSampling * YCbCrHorizSampling;
	byte *tp;
	u_long x;

	/* XXX adjust fromskew */
	while (h >= YCbCrVertSampling) {
		tp = cp;
		for (x = w; x >= YCbCrHorizSampling; x -= YCbCrHorizSampling) {
			putRGBContigYCbCrClump(tp, pp,
			    YCbCrHorizSampling, YCbCrVertSampling,
			    w, Coff, 0, toskew);
			tp += 3*YCbCrHorizSampling;
			pp += Coff+2;
		}
		if (x > 0) {
			putRGBContigYCbCrClump(tp, pp,
			    x, YCbCrVertSampling,
			    w, Coff, YCbCrHorizSampling - x, toskew);
			pp += Coff+2;
		}
		cp += YCbCrVertSampling*(3*w + toskew);
		pp += fromskew;
		h -= YCbCrVertSampling;
	}
	if (h > 0) {
		tp = cp;
		for (x = w; x >= YCbCrHorizSampling; x -= YCbCrHorizSampling) {
			putRGBContigYCbCrClump(tp, pp, YCbCrHorizSampling, h,
			    w, Coff, 0, toskew);
			tp += 3*YCbCrHorizSampling;
			pp += Coff+2;
		}
		if (x > 0)
			putRGBContigYCbCrClump(tp, pp, x, h,
			    w, Coff, YCbCrHorizSampling - x, toskew);
	}
}

/*
 * Select the appropriate conversion routine for packed data.
 */
static tileContigRoutine
pickTileContigCase(Map)
	RGBvalue* Map;
{
	tileContigRoutine put = 0;

	switch (photometric) {
	case PHOTOMETRIC_RGB:
		put = (bitspersample == 8 ?
		    putRGBcontig8bittile : putRGBcontig16bittile);
		break;
	case PHOTOMETRIC_PALETTE:
		switch (bitspersample) {
		case 8:	put = put8bitcmaptile; break;
		case 4: put = put4bitcmaptile; break;
		case 2: put = put2bitcmaptile; break;
		case 1: put = put1bitcmaptile; break;
		}
		break;
	case PHOTOMETRIC_MINISWHITE:
	case PHOTOMETRIC_MINISBLACK:
		switch (bitspersample) {
		case 8:	put = putgreytile; break;
		case 4: put = put4bitbwtile; break;
		case 2: put = put2bitbwtile; break;
		case 1: put = put1bitbwtile; break;
		}
		break;
	case PHOTOMETRIC_YCBCR:
		switch (bitspersample) {
		case 8: put = putcontig8bitYCbCrtile; break;
		}
		break;
	}
	if (put == 0)
		TIFFError(filename, "Can not handle format");
	return (put);
}

/*
 * Select the appropriate conversion routine for unpacked data.
 *
 * NB: we assume that unpacked single channel data is directed
 *	 to the "packed routines.
 */
static tileSeparateRoutine
pickTileSeparateCase(Map)
	RGBvalue* Map;
{
	tileSeparateRoutine put = 0;

	switch (photometric) {
	case PHOTOMETRIC_RGB:
		put = (bitspersample == 8 ?
		    putRGBseparate8bittile : putRGBseparate16bittile);
		break;
	}
	if (put == 0)
		TIFFError(filename, "Can not handle format");
	return (put);
}

#endif /* HAVE_TIFF */
