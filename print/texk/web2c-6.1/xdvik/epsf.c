/*
 * Drawing routine for \special commands generated
 * by epsf.sty and epsf.tex, i.e., of the form:
 *   psfile=/u11/doug/foo.ps llx=165 lly=346 urx=461 ury=434 rwi=2663
 *
 * No Copyright.  Public domain.
 * Doug Bryan, dbryan@stanford.edu, January 1993. 
 *
 * History:
 *  06/07/93: Support for psfig added
 *            Case sensitivity of the PSfile= flag removed
 *            Norm Walsh, walsh@cs.umass.edu
 *  08/30/93: Changed include file "xdvi.h" to "config.h" so
 *            that it will work with xdvik.
 *            Alan DeWeerd, deweerd@wisnuf.physics.wisc.edu
 *
 * It'd be way cool to someday do greyscale anti-aliasing on the bitmaps
 * gs generates, and then display the pixmap.
 */

#include "config.h"
#include <kpathsea/c-stat.h>
#include <kpathsea/pathsearch.h>
#include <kpathsea/tex-file.h>

#include "epsf.h"
extern void put_grey_rectangle ();

extern long magnification; /* From dvi_init.c. */

#ifndef L_tmpnam
#define L_tmpnam 256
#endif
#ifndef GSEXEC
#define GSEXEC "gs"
#endif
#define XMALLOC_LABEL "gs bitmap"
#define	RESOLUTION  (pixels_per_inch/shrink_factor) 
  /* current pixels per inch */
#define FRESOLUTION ((float)RESOLUTION)
#define PT2PXL(x) ((int)((float)(x) * FRESOLUTION / 72.0))
  /* convert points (i.e., 1/72 inches) to pixels */

/*
 * Print error message.
 */
static void
Moan(fmt, msg)
  char    *fmt, *msg;
{
    Fprintf(stderr, "%s: epsf: ", prog);
    Fprintf(stderr, fmt, msg);
    (void) fputc('\n', stderr);
}


/* Read a possibly-quoted filename from *SPECIAL, returning it in
   FILENAME, and advance SPECIAL past it.  */

static Boolean
parse_filename (special, filename)
    char **special, *filename;
{
  char match_char;
  char *fname_end;
  
  if (**special == '"' || **special == '\'')
    { /* Filename is "quoted" or 'quoted'.  */
      match_char = **special;
      (*special)++;
    }
  else
    { /* No quotes, just take everything up to a space.  */
      match_char = ' ';
    }
  
  for (fname_end = *special; *fname_end && *fname_end != match_char;
       fname_end++)
    ;
  
  if (*fname_end == match_char)
    {
      strncpy (filename, *special, fname_end - *special);
      filename[fname_end - *special] = 0;
      *special += fname_end - *special + 1;
      
      while (**special == ' ')
        (*special)++;
        
      return True;
    }

  return False;
}


/*
 * Simple sscanf version of finding \special parametes.
 * labrea.Stanford.EDU:~ftp/pub/tv001.tar.Z has some better parsing code
 * if you'd like.
 */
static Boolean
parse_special(orig_special, psfilename, llx, lly, urx, ury, rwi)
  char *orig_special, *psfilename;
  int *llx, *lly, *urx, *ury, *rwi;
{
  float llx_f, lly_f, urx_f, ury_f, rwi_f;
  char *special = orig_special;
  
  special += 7; /* skip the ``psfile='' which may be case-mixed */
  
  if (parse_filename (&special, psfilename))
    {
      if (sscanf(special, "llx=%f lly=%f urx=%f ury=%f rwi=%f",
                  &llx_f, &lly_f, &urx_f, &ury_f, &rwi_f) == 5)
        {
          *llx = llx_f + .5;
          *lly = lly_f + .5;
          *urx = urx_f + .5;
          *ury = ury_f + .5;
          *rwi = rwi_f + .5;
          return True;
        }
    }

  Moan("\"%s\" not understood.", orig_special);
  return False;
}

/*
 * Simple sscanf version of finding psfig \special parameters.
 */
static Boolean
parse_psfig_special(special, width, height, llx, lly, urx, ury, rwi)
  char *special;
  int *width, *height, *llx, *lly, *urx, *ury, *rwi;
{
  if (sscanf(special, "ps::[begin] %d %d %d %d %d %d",
              width, height, llx, lly, urx, ury) == 6) {
    return True;
  } else {
    Moan("\"%s\" not understood.", special);
    return False;
  }
}

/*
 * Simple sscanf version of finding psfig angle parameters.
 */
static Boolean
parse_psfig_angle(special,angle)
  char *special;
  float  *angle;
 
{
  Boolean ret;
  
  if (no_epsf)
    ret = True;
  
  else if (sscanf(special, "ps:: %f rotate", angle) == 1) {
    ret = True;

  } else {
    Moan("\"%s\" not understood.", special);
    ret = False;
  }
  return ret;
}

struct cache_node {
  char   *filename;
  time_t mtime;
  float  scale, angle;
  int    resolution;
  struct bitmap *bm;
  struct cache_node *next;
};

static struct cache_node *cache = NULL;
/* 
 * Cache bitmaps so we don't call gs so much.  Actully, epsfile() is
 * called way too much.  xdvi should use backing store and let X11 do
 * the caching.  Oh well.  On a 30MIP machine who cares?
 */

static time_t 
mod_time_of(filename)
  char *filename;
{
  struct stat stat_buff;

  if (stat(filename, &stat_buff)==0) {
    return stat_buff.st_mtime;
  } else {
    return 0;
  }
}

/*
 * Free one item in the cache.
 * Could be clever and flush the one with the highest
 * resolution, or biggest bitmap, or...
 */
void flush_one_gsbm()
{
  struct cache_node *head = cache;

  if (head) {
    cache = cache->next;
    free(head->filename);
    free(head->bm->bits);
    free(head->bm);
    free(head);
  }
}

/* 
 * Free all items in the cache.
 */
void flush_gsbm_cache()
{
  while (cache) flush_one_gsbm();
}

/*
 * Try to find a bitmap for psfname, at the given scale and res, in the cache. 
 */
static struct bitmap *
find_bitmap(psfname, x_scale, y_scale, angle, res)
  char  *psfname;
  float x_scale, y_scale, angle;
  int   res;
{
  struct cache_node *ll, *pred;

  for (ll=cache, pred=NULL;  ll;  pred=ll, ll=ll->next) {
    if (ll->resolution==res && ll->scale==x_scale && ll->angle==angle&&
	strcmp(ll->filename, psfname)==0) {	
      if (mod_time_of(psfname) == ll->mtime) {
	return ll->bm;
      } else {
	/* File has been modified.  Remove it from cache. */
	if (!pred) {
	  cache = cache->next;
	} else {
	  pred->next = ll->next;
	}
	free(ll->filename);
	free(ll->bm->bits);
	free(ll->bm);
	free(ll);
	return NULL;
      }
    }
  }
  return NULL;
}

/*
 * Find a bitmap for psfname at the given scale and current resolution.
 * Get it from the cache, or generate it using ghostscript.
 */
static struct bitmap *
get_bitmap(psfname, x_scale, y_scale, llx, lly, urx, ury,angle)
  /* 
   * When psfname is interpreted at scale, it'll produce 
   * a bounding box of (llx,lly), (urx,ury).  Scale and (llx,lly),(urx,ury)
   * do not include shrink_factor.  We'll run gs at the same resolution
   * xdvi is running at to take care of shrink_factor.
   */
  char *psfname;
  float x_scale, y_scale, angle;
  int llx, lly, urx, ury;
{ 
  int line, status, decompress;
  char command[500], tempfilename[L_tmpnam];
  FILE *gs_out;
  char *name;
  struct bitmap *result;

  if (*psfname == '`')
    {
      decompress = 1;
      psfname++;
    }
  else
    decompress = 0;

  result = find_bitmap(psfname, x_scale, y_scale, angle, RESOLUTION);
  if (result) return result;

  if (!decompress)
    { /* Only do path searching for normal files, not commands.  */
      name = kpse_path_search (KPSE_NONFONT_PATH (DEFAULT_FIG_PATH,
                                                  KPSE_TEX_ENVS),
                               psfname, true);
      if (!name) {
        Moan("Cannot open %s.", psfname);
        return NULL;
      }
    }

  result = (struct bitmap *) xmalloc(sizeof(struct bitmap), XMALLOC_LABEL);
  while (!result && cache) {
    flush_one_gsbm();
    result = (struct bitmap *) xmalloc(sizeof(struct bitmap), XMALLOC_LABEL);
  }
  if (!result) {
    Moan("Cannot malloc.", NULL);
    return NULL;
  }

  result->w = (short) PT2PXL(urx-llx);
  result->h = (short) PT2PXL(ury-lly);

  alloc_bitmap(result);
  while (!result->bits && cache) {
    flush_one_gsbm();
    alloc_bitmap(result);
  }
  if (!result->bits) {
    Moan("Cannot malloc", NULL);
    free(result);
    return NULL;
  }

  /* Translate the figure to the bottom left corner (next to the PostScript
   * origin).  Scale as called for by \epsf.  Append a showpage in case the
   * psfile doesn't have one.   Run ghostscript at the same resolution xdvi
   * is running at.  Have ghostscript output a "geometry" (aka 2d matrix of
   * pixels) the same as the bitmap we're after.  Note that ghostscript's bit
   * device driver always right-pads output scan lines to the nearst byte.
   *
   * Even when -q is used, ghostscript yells some error messages to stdout.
   * So we don't write the bitmap to stdout; instead write it to a file  
   * and send stdout and stderr to /dev/null.  (We could use stdout if we'd
   * rewrite gs_fonts.ps et al to strictly obey -q.)
   
     The only error message I see in gs_fonts.ps that doesn't obey -q
     is if the default font can't be found, in which case Ghostscript must
     be hopelessly misconfigured. So I'm tempted to try the pipe now.
     --kb@cs.umb.edu, 25feb94.
     
     But vojta says the next xdvi will have completely different epsf
     and other special support anyway, so don't bother. (24mar94)
   */

  tmpnam(tempfilename);
  sprintf(command, 
	  "(echo %d %d translate %f %f scale %f rotate; \
%s %s; \
echo showpage) \
| %s -q -sDEVICE=bit -r%d -g%dx%d -sOutputFile=%s -",
	  -llx, -lly, x_scale, y_scale, angle, 
	  decompress ? "" : "cat", decompress ? psfname : name,
	  GSEXEC, RESOLUTION, (int)result->w, (int)result->h, tempfilename);

  if (!(debug & DBG_EPS))
    strcat (command, ">/dev/null 2>&1");
  
  if (debug & DBG_EPS)
    fprintf (stderr, "Making EPS bitmap with cmd: %s\n", command);

  status = system(command);
  if (status) {             /* Something went wrong. */
    char buff[1000];
    sprintf(buff, "sh command \"%s\" failed with status %d.",
	    command, status);
    Moan(buff, (char*)NULL);
    free(result->bits);
    free(result);
    return NULL;
  }

  gs_out = fopen(tempfilename, "r");
  if (!gs_out) {
    Moan("Cannot read %s.", tempfilename);
    free(result->bits);
    free(result);
    return NULL;
  }     

  {
    int line_size = (int)(ROUNDUP(result->w, 8));
 
    for (line=0; line < ((int)result->h); line++)
      fread(&(result->bits[line*result->bytes_wide]), line_size, 1, gs_out); 
  }

  fclose(gs_out);
  unlink(tempfilename);

  {
    /* The following array used to appear, by another name, in pxl.c.  */
    static  unsigned char reverse_bits[0x100] = { 
        0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
        0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
        0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
        0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
        0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
        0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
        0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
        0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
        0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
        0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
        0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
        0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
        0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
        0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
        0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
        0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
        0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
        0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
        0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
        0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
        0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
        0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
        0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
        0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
        0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
        0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
        0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
        0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
        0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
        0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
        0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
        0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
	};
    int i, max=((int)result->bytes_wide) * ((int)result->h);

    for (i=0; i<max; i++)
      result->bits[i] = reverse_bits[(unsigned char)(result->bits[i])];

#if 0 /* used to be SPARC, I don't know why */
    {
        int temp;
      /* Sparc machines use a different BYTE ordering.  The code below does
         the appropriate swapping.  In a hacky, crude manner, I admit. 
         Norm 6/7/93
       */

      for (i=0; i<max; i += 2)
        {
          temp = result->bits[i];
          result->bits[i] = result->bits[i+1];
          result->bits[i+1] = temp;
        }

      for (i=0; i<max; i += 4)
        {
          temp = result->bits[i+0];
          result->bits[i+0] = result->bits[i+2];
          result->bits[i+2] = temp;

          temp = result->bits[i+1];
          result->bits[i+1] = result->bits[i+3];
          result->bits[i+3] = temp;
        }
    }
#endif /* 0 */

   /* Don't know what you have to do to make this work on a LSB machine.
    * Don't know what gs outputs on LSB, but probably bits in the usual
    * left-right, top-down order.  The above loop switches
    * bit order in each result->bits[] byte.  Might also have to switch
    * byte order in each BYTES_PER_BMUNIT group.
    
    I don't understand any of the above comments -- Ghostscript's bit
    device gives identical output on LittleEndian and BigEndian
    machines, as far as I can see, and the above bit reversal sufficed
    to display my EPS files on both LittleEndian and BigEndian displays.
    --karl, 3de93
    */
  }

 /* Add it to the cache. */
 {struct cache_node *new_head = (struct cache_node *)xmalloc(
				sizeof(struct cache_node), XMALLOC_LABEL);
  
  if (new_head) {
    new_head->filename   = (char *) xstrdup(psfname);
    new_head->mtime      = mod_time_of(psfname);
    new_head->scale      = x_scale;
    new_head->angle      = angle;
    new_head->resolution = RESOLUTION;
    new_head->bm         = result;
    new_head->next       = cache;
    cache = new_head;
  }
 }
 return result;
}

/*
 * Execute /special commands of the form 
 *   psfile=/u11/doug/foo.ps llx=165 lly=346 urx=461 ury=434 rwi=2663
 */
void
epsfile(special)
  char *special;
{
  int llx, lly, urx, ury, rwi;
  char psfilename[100];
  float epsf_scale;
  struct bitmap *bitmap;

  if (no_epsf) return;
  if (!parse_special(special, psfilename, &llx, &lly, &urx, &ury, &rwi)) 
    return;

  /* rwi is 10 times the desired width, in points (but scaled by the
     magnification).  epsf_scale is thus the scale according to the
     \special command.  (For epsfile.tex specials, x and y scale are
     always the same.) */
  epsf_scale = (rwi * (magnification / 10000.0)) / (urx - llx + 1);

  /* 
   * Scale the bounding box.  The floating point calculations have
   * potential round-down errors, so add an extra pixel to compensate. 
   */
  llx = (int) ((float)(llx) * epsf_scale);
  lly = (int) ((float)(lly) * epsf_scale);
  urx = (int) ((float)(urx) * epsf_scale);
  ury = (int) ((float)(ury) * epsf_scale);
  urx++; ury++;

  if (epsf_grey) {
    put_grey_rectangle(PXL_H, PXL_V - PT2PXL(ury-lly), 
		       PT2PXL(urx-llx+1), PT2PXL(ury-lly+1));
  } else {
    bitmap = get_bitmap(psfilename, epsf_scale, epsf_scale, llx, lly, urx, ury,0);
    if (bitmap) {
      put_bitmap(bitmap, PXL_H, PXL_V - PT2PXL(ury-lly));
    } else {
      put_grey_rectangle(PXL_H, PXL_V - PT2PXL(ury-lly), 
			 PT2PXL(urx-llx+1), PT2PXL(ury-lly+1));
    }
  }
}

/*
 * Execute /special commands of the form 
 *   ps::[begin] width height llx lly urx ury ...
 * 
 * where each parameter is in scaled points (65536sp/TeXpt)
 */

static int psfig_ok = 0, psfig_angle_ok = 0;
static int psfig_llx, psfig_lly, psfig_urx, psfig_ury;
static int psfig_width, psfig_height;
static float psfig_angle;

void
psfig_setup(special)
  char *special;
{
  if (no_epsf) return;
  psfig_angle =0.0;  /* default angle is zero */
  psfig_ok = parse_psfig_special(special, &psfig_width, &psfig_height,
			                  &psfig_llx, &psfig_lly, 
			                  &psfig_urx, &psfig_ury);
}

void
psfig_setup_angle(special)
  char *special;
{
  if (no_epsf) return;
  psfig_angle_ok = parse_psfig_angle(special, &psfig_angle);
}

void
psfig(special)
     char *special;
{
  char psfilename[512];
  char *p;
  float x_scale, y_scale;
  struct bitmap *bitmap;
  int llx, lly, urx, ury;
  int width, height;
  int scaled_width, scaled_height;

  if (!psfig_ok)
    return;
  if (sscanf(special, "ps: plotfile %s ", psfilename) != 1)
    {
      Moan("\"%s\" not understood.", special);
      return;
    }

  /* special command is "ps: plotfile" and file may be compressed */
  p = special;
  p += 13;
  while (*p == ' ')
    p++;
  if (!parse_filename (&p, psfilename))
    {
      Moan ("\"%s\" not understood.", special);
      return;
    }

  llx = (int) ((float)(psfig_llx) / 65536.0);
  lly = (int) ((float)(psfig_lly) / 65536.0);
  urx = (int) ((float)(psfig_urx) / 65536.0);
  ury = (int) ((float)(psfig_ury) / 65536.0);
  scaled_width = (int) ((float)(psfig_width) / 65536.0);
  scaled_height = (int) ((float)(psfig_height) / 65536.0);

  width = (urx - llx) + 1;
  height = (ury - lly) + 1;

  x_scale = (float)(psfig_width )/ (float)(psfig_urx - psfig_llx );
  y_scale = (float)(psfig_height)/ (float)(psfig_ury - psfig_lly );

  /* The division by 65781.76 is made for psfig-files in dvips in
     special.lpro. */

  llx = floor(((float)(psfig_llx) * x_scale / 65781.76));
  lly = floor(((float)(psfig_lly) * y_scale / 65781.76));
  urx = ceil (((float)(psfig_urx) * x_scale / 65781.76));
  ury = ceil (((float)(psfig_ury) * y_scale / 65781.76));

  if (epsf_grey) {
    put_grey_rectangle(PXL_H, PXL_V,
		       PT2PXL(urx-llx), PT2PXL(ury-lly));
  } else {
    bitmap = get_bitmap(psfilename, x_scale, y_scale, llx, lly, urx,
                        ury, psfig_angle);
    if (bitmap) {
      put_bitmap(bitmap, PXL_H, PXL_V);
    } else {
      put_grey_rectangle(PXL_H, PXL_V,
			 PT2PXL(urx-llx), PT2PXL(ury-lly));
    }
  }
}
