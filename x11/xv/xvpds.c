#ifdef HAVE_PDS
/*
 * xvpds.c - load routine for astronomical PDS/VICAR format pictures
 * version of 3-13-92
 *
 * Anthony A. Datri
 * Convex Computer Corp
 * 3000 Waterview Parkway
 * Ricardson, TX 75080
 * datri@convex.com
 *
 * 9-2-91    began integration.	 Much of this code is lifted from vicar.c,
 *	     which I wrote for xloadimage.  This is a little simpler, though.
 *
 * 10-17-91  pdsuncomp is called with system(), which typically feeds the
 *	     commandline to sh.  Make sure that your .profile adds wherever
 *	     you have pdsuncomp to the PATH, like
 *
 *		PATH=$PATH:/usr/local/bin
 *
 * 11-15-91  substituted vdcomp from Viking CD's for pdsuncomp. I added
 *           recognition of - and shut off various messages
 *
 * 1-5-92    merged into xv rel 2
 *
 * 3-11-92   cleaned up some comments
 *
 * 3-24-92   Got some new CD's from NASA of mosics and other processed Viking
 *           stuff.  There are actually records terminated with CRNLCR in
 *           these images, as well as ones that identify the spacecraft name
 *           as {VIKING_ORBITER_1, VIKING_ORBITER_2}.  I hacked up the code
 *           yet further to deal with these.  There's a Sun 4 XView binary for
 *           an image display program on these discs, but it's nowhere near as
 *           neat as the good Mr. Bradley's XV.
 *
 *
 * Sources of these CD's:
 *
 *  National Space Science Data Center
 *  Goddard Space Flight Center
 *  Code 933.4
 *  Greenbelt, Maryland  
 *  (301) 286-6695
 *   or call
 *  (301) 286-9000 (300,1200,2400 bps)
 *   or telnet
 *  nssdca.gsfc.nasa.gov (128.183.10.4) and log in as 'NODIS' (no password).
 *
 *  Randy Davis
 *  LASP
 *  University of Colorado
 *  Boulder CO 80309-0392
 *  (303) 492-6867
 *
 * These CD's are reasonably priced.  Encourage the continue production
 * of them by buying a set.
 *
 * There are three types of files that we deal with here.  I'll call them
 * PDS-labeled, PDS-labeled Huffman-encoded, and VICAR.  Each consists of data
 * prefixed with a set of ASCII headers.  PDS-labeled and VICAR files are raw
 * grayscale data, the dimensions of which can be found from the headers.
 * PDS-labeled, Huffman-encoded files have the image information adaptively
 * Huffman-encoded, and the encoding histogram follows the ASCII headers.
 * To decode these, we use a slightly modified version of "vdcomp.c" from the
 * NASA Viking CD-ROMS.  For xv to work, you need to have vdcomp compiled
 * and in your search path.  vdcomp.c should be included with this distribution.
 *
 * I've heard that newer discs have FITS images on them.  If they do, support
 * for them will be added when I get one.  Until then, you can use fitstopgm.
 *
 * LoadPDS(fname, numcols)  -  coerces a PDS/VICAR image
 * WriteVICAR(fp, pic, w, h, r,g,b, numcols, style)
 */

/*
 * Copyright 1989, 1990 by Anthony A. Datri
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright   
 * notice appear in all copies, that both the copyright notice and this
 * permission notice appear in supporting documentation.
 * 
 * In exception to the above, permission to John Bradley is hereby granted to
 * distribute this code as he sees fit within the context of his "xv" image
 * viewer.
 *
 * This software is provided "as is" without any express or implied warranty.
 */


#include "xv.h"


#define PDSFIXED        (1)
#define PDSVARIABLE     (2)
#define PDSTRASH        (-1)
#define VICAR           (3)
#define VIKINGFIXED     (4) /* Viking discs have a unique variant */
#define VIKINGVARIABLE  (5)


#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

#define MAX_SIZE	20480   /* a guess -- even magellan images aren't
				    bigger than 2k x 2k */
#define RTBUFFSIZE	62	/* small buffer for magic */
#define FNAMESIZE	1024	/* too hard to generalize really getting it */
#define PDSUNCOMP	"vdcomp" /* filter to un-Huffmanize */

/* This is arbitrary.  Everything I've seen so far fits in 50 chars */
#define COMMENTSIZE	50


static char	scanbuff      [MAX_SIZE], 
                rtbuff        [RTBUFFSIZE], 
                infobuff      [COMMENTSIZE],
		spacecraft    [COMMENTSIZE],
		target        [COMMENTSIZE],
		filtname      [COMMENTSIZE],
		mphase        [COMMENTSIZE],
		iname         [COMMENTSIZE],
		itime         [COMMENTSIZE],
                garbage       [1020],
		pdsuncompfname[FNAMESIZE];
byte *image;


/* 
 * pds_strstr.c --
 *
 *	Source code for the "strstr" library routine.
 *
 * Copyright 1988 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

/*
 *----------------------------------------------------------------------
 *
 * pds_strstr --
 *
 *	Locate the first instance of a substring in a string.
 *
 * Results:
 *	If string contains substring, the return value is the
 *	location of the first matching instance of substring
 *	in string.  If string doesn't contain substring, the
 *	return value is 0.  Matching is done on an exact
 *	character-for-character basis with no wildcards or special
 *	characters.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *pds_strstr(string, substring)
     char *string;	/* String to search. */
     char *substring;		/* Substring to try to find in string. */
{
  register char *a, *b;

  /* First scan quickly through the two strings looking for a
   * single-character match.  When it's found, then compare the
   * rest of the substring.
   */

  b = substring;
  if (*b == 0) return string;

  for ( ; *string != 0; string += 1) {
    if (*string != *b) continue;

    a = string;
    while (1) {
      if (*b == 0) return string;

      if (*a++ != *b++) break;

    }
    b = substring;
  }
  return (char *) 0;
}





/* Get a NULL-, CR-, or CRLF-terminated record from a PDS file  */
/* The method of termination is unpredictable.  A pox on VMS.   */
/* Sometimes we even get mixtures of CR's and LF's.  Sigh.      */

int getpdsrec(f,buff)
     FILE *f;
     char *buff;
{
  static char *bp;
  static int count;
  int c;

  /* read any leading CR's or LF's (sigh) */
  *buff='\0';
  bp=buff;
  count = 0;
  while (count==0) {
    c=fgetc(f);
    switch (c) {
    case EOF: return(0);
    case '\r':
    case '\n': continue;
    default:  ungetc(c,f);
      count = -1;
    }
  }

  count=0;
  bp=buff;
  while (1) {
    c=fgetc(f);
    switch (c) {

    case '\r':  *bp='\0';
                switch (c=fgetc(f)) {
		  case EOF:
		  case '\n':  break;
                  default:    ungetc(c,f);
		  }
                return(count);

    case EOF:	*bp='\0';  return(count);		    
    
    case '\0':  return(count);
    
    default:	count++;  *bp++ = c;
    }
  }
}


/* A VICAR image seems to begin with a one-line header describing all sorts
 * of things, almost all of which don't mean much to Joe User, who just
 * wants to see what Venus looks like.  The first thing in this label is a
 * string defining the size of the label, like "LBLSIZE=2048".  Then there's
 * other crud of variable usefulness.  Important to us are the NL and NS
 * fields, which seem to tell us the number of columns and rows, respectively.
 * Among these other fields seem to be the local map coordinates of the region.
 * These files are easy -- you can even convert one to usefulness by grabbing
 * the dimensions and lblsize and feeding to rawtopgm.  Example, if lblsize,
 * nl, and ns are 2048, 1536, and 2048, respectively, use "rawtopgm -headerskip
 * 2048 2048 1536 <foo.img >foo.pgm.  All of the samples I've seen are 8 bits
 * deep, so we assume that.  Yeah, yeah, that's nasty and evil, and I'm
 * terribly ashamed of it, but I'm not going to bother with bit fiddling
 * until I see an image that needs it.  I once had a 16-bit Galileo image of
 * Venus, but it got lost (sigh).
 *
 * These images are typically fairly big, so it makes sense to verify the
 * format before we try to read it all in.
 *
 * PDS images are prefixed like this:
 *
 *  od -a c2061139.imq | head -a
 *  0000000    - nul   N   J   P   L   1   I   0   0   P   D   S   1   0   0
 *
 * so we'll read off the first two chars, which seem to be a length field
 * of some kind, then look for NJPL..  Images on the Sampler
 * disc seem to leave off the first two bytes.  Sigh.  This may sometimes be
 * a distinction between the fixed and variable-record files.
 */
            
/*******************************************/
int LoadPDS(fname, pinfo)
     char    *fname;
     PICINFO *pinfo;
{
  /* returns '1' on success, '0' on failure */

  byte *pic8;
  int tempnum;
  FILE	*zf;
  static int isfixed,teco,i,j,itype,
             recsize,hrecsize,irecsize,isimage,labelrecs,labelsofar,
             x,y,lpsize,lssize,samplesize,returnp,labelsize,yy;
  char	*tmp;
  char  *ftypstr;
  unsigned long filesize;

  pinfo->type = PIC8;
  pic8 = (byte *) NULL;
  isfixed = TRUE;
  returnp = isimage = FALSE;
  itype   = PDSTRASH;

  teco = i = j = recsize = hrecsize = irecsize = labelrecs = x = y = 0;
  lpsize = lssize = samplesize = labelsize = labelsofar = 0;

  (*pdsuncompfname) = (*iname) = (*target) = (*filtname) = (*garbage) = '\0';
  (*itime) = (*spacecraft) = (*scanbuff) = (*mphase) = (*rtbuff) = '\0';

  /* there may be some wisdom in statically mallocing space for an 800x800
     image, since that's what the Voyager cd's have, or even a 2kx2k, since
     some of the Magellan images from Ames are that size */

  zf=fopen(fname,"r");
  if (!zf) {
    SetISTR(ISTR_WARNING,"LoadPDS: can't open %s\n",fname);
    return 0;
  }

  /* figure out the file size (for Informational Purposes Only) */
  fseek(zf, 0L, 2);
  filesize = ftell(zf);
  fseek(zf, 0L, 0);

  /* read the magic.  If it's got two bytes of crud in front of it, it's
     a screwy "variable-length-record" file. We have to jump through
     bloody VAX hoops to find out how long the record is.  Sigh.  Have
     people never heard of newlines? */

  if ((teco=fread(rtbuff,13,1,zf)) != 1) {
    SetISTR(ISTR_WARNING,"LoadPDS: couldn't read magic #: %d,%s",teco,fname);
    fclose(zf);
    return 0;
  }

  /* this makes sscanf so much happier */
  rtbuff[RTBUFFSIZE-1] = '\0';

  if (strncmp(rtbuff, "NJPL1I00PDS",11) == 0) {
    itype=PDSFIXED;
  } else if (strncmp(rtbuff+2,"NJPL1I00PDS",11) == 0) {
    itype=PDSVARIABLE;
  } else if (strncmp(rtbuff,"CCSD3Z",6) == 0) {
    itype=VIKINGFIXED; /* unique variant of PDS on Viking discs (browse) */
  } else if (strncmp(rtbuff+2,"CCSD3Z",6) == 0) {
    itype=VIKINGVARIABLE;
  } else if (sscanf(rtbuff,"LBLSIZE = %d%n",&labelsize,&labelsofar) == 1) {
    itype=VICAR;
  } else {
    SetISTR(ISTR_WARNING,"LoadPDS: can't handle this file, for some reason\n");
    fclose(zf);
    return 0;
  }

  switch (itype) {
  case PDSFIXED:
  case VICAR:
  case VIKINGFIXED:     isfixed=TRUE;
                        sprintf(pinfo->fullInfo,
				"PDS/VICAR, 8 bits per pixel. (%ld bytes)",
				filesize);
                        break;
  case PDSVARIABLE:
  case VIKINGVARIABLE:  isfixed=FALSE;
                        sprintf(pinfo->fullInfo,
			"Huffman-encoded PDS, 8 bits per pixel. (%ld bytes)",
				filesize);
  }

  if ((itype == PDSFIXED) || (itype == PDSVARIABLE) ||
      (itype == VIKINGFIXED) || (itype == VIKINGVARIABLE)) {

    if (isfixed == FALSE) {
      teco = ((*rtbuff) + ((*(rtbuff+1)) << 8));
      fread(rtbuff,(teco % 2 ? teco + 1 : teco ) - 11,1,zf);
    }

    /* we know that we've got a PDS file of some sort.	We have to
       search through the labels looking for things because usage doesn't seem
       to be consistent, or even predictable.  When I added this format to
       xloadimage, I had the code spew out various peices of information, like
       the name of the target, filter used, etc.  XV, however, doesn't really
       provide a mechanism for this kind of thing, and I can't really stick it
       into the info box without overhauling it.  Maybe later I'll add a
       -verbose switch which this module will interpret as license to scribble
       on stdout or stderr */

    for (;;) {
      if (isfixed) {
	if (getpdsrec(zf,scanbuff) == 0) {
	  SetISTR(ISTR_WARNING,"corrupt or incomplete PDS labels (low fread)");
	  break;
	}
      } else {
	/* AAAARGGGGH!  We've got a bloody variable-length file where
	 * the headers are preceded by a byte count, as a VAX 16-bit
	 * integer of all things.  These guys seem to have had VMS on the
	 * brain.  ASCII?  Wot's that?
	 *
	 * "A compressed image file is composed of variable-length records
	 * defined according to the ISO standard.  Each variable length
	 * record starts with a record length indicator, stored as a 16-bit
	 * integer, followed by the number of bytes indicated in the record
	 * length indicator. If the length indicator is odd, then a pad byte
	 * is appended to the end of the record so that all records contain
	 * an even number of bytes." */
	                                                                    
	i=getc(zf);
	j=getc(zf);
	if (j == EOF) {
	  SetISTR(ISTR_WARNING,"LoadPDS: j is EOF\n");
	  fclose(zf);
	  return 0;
	}
	
	teco = i + (j << 8);
	if (teco % 2) teco++;

	if (fread(scanbuff,teco,1,zf) != 1) {
	  SetISTR(ISTR_WARNING,"LoadPDS: bad fread reading labels\n");
	  fclose(zf);
	  return 0;
	}
	
	scanbuff[teco]='\0';
      }

      /* otay, we've managed to wrestle a header of some sort from the
	 file -- now we grep through until we hit the END, at which point
	 we break out of the loop and see what we've got.  In the future,
	 we'll check for informational headers and write them out if the
	 fool user wants them.  There seems to be disagreement about what
	 the headers are called, since we might find, for example, either
	 TARGET_BODY or TARGET_NAME.  Bloody 'ell.  I think the problem
	 is that there isn't an actual *standard* for these formats, so
	 people kinda make it up as they go along. */

      if (strcmp(scanbuff,"END") == 0) {
	break;
      } else if (sscanf(scanbuff,"RECORD_TYPE = %s",rtbuff) == 1) {
	if (strncmp(rtbuff,"VARIABLE_LENGTH",15) == 0) {
	  /*		itype=PDSVARIABLE; */
	} else if (strncmp(rtbuff,"FIXED_LENGTH",12) == 0) {
	  /*		itype=PDSFIXED;*/
	} else {
	  rtbuff[RTBUFFSIZE-1]='\0'; /* juuuust in case */
	  SetISTR(ISTR_WARNING,
		  "LoadPDS: unsupported record type \"%s\"\n",rtbuff);
	  fclose(zf);
	  return 0;
	}
      } else if (sscanf(scanbuff,"RECORD_BYTES = %d",&recsize) == 1) {
	/* these default to RECORD_BYTES unless explicitly set */
	if (hrecsize == 0) hrecsize=recsize;
	    if (irecsize == 0) irecsize=recsize;
	continue;
      } else if (sscanf(scanbuff,"FILE_TYPE = %s", rtbuff) != 0) {
	if (strncmp(rtbuff,"IMAGE",5) == 0) {
	  isimage=TRUE;
	  continue;
	} else {
	  isimage=FALSE;
	  break;
	}
      } else if ((sscanf(scanbuff," HEADER_RECORDS = %d",&labelrecs) == 1) ||
		 (sscanf(scanbuff," LABEL_RECORDS = %d", &labelrecs) == 1)) {
	continue;
      } else if (sscanf(scanbuff," IMAGE_LINES = %d",&y) == 1) {
	isimage=TRUE; continue;
      } else if (sscanf(scanbuff," LINE_SAMPLES = %d",&x) == 1) {
	continue;
      } else if (sscanf(scanbuff," LINES = %d",&y) == 1) {
	isimage=TRUE; continue;
      } else if (sscanf(scanbuff," HEADER_RECORD_BYTES = %d",&hrecsize)==1) {
	continue;
      } else if (sscanf(scanbuff," IMAGE_RECORD_BYTES = %d",&irecsize)==1) {
	continue;
      } else if (sscanf(scanbuff," LINE_PREFIX_BYTES = %d",&lpsize)==1) {
	continue;
      } else if (sscanf(scanbuff," LINE_SUFFIX_BYTES = %d",&lssize)==1) {
	continue;
      } else if (sscanf(scanbuff," SAMPLE_BITS = %d", &samplesize) == 1) {
	continue;
      } else if (sscanf(scanbuff," SPACECRAFT_NAME = %s %s",
			spacecraft,garbage) == 2 ) {
	strcat(spacecraft,pds_strstr(scanbuff, spacecraft)+strlen(spacecraft));
	continue;
      } else if (sscanf(scanbuff," SPACECRAFT_NAME = %s", spacecraft) == 1) {
	continue;

      } else if (sscanf(scanbuff," TARGET_NAME = %s", target) == 1) {
	continue;
      } else if (sscanf(scanbuff," TARGET_BODY = %s", target) == 1) {
	continue;

      } else if (sscanf(scanbuff," SPACECRAFT_EVENT_TIME = %s", itime) == 1) {
	continue;
      } else if (sscanf(scanbuff," IMAGE_TIME = %s", itime) == 1) {
	continue;
      } else if (sscanf(scanbuff," FILTER_NAME = %s", filtname) == 1) {
	continue;
      } else if (sscanf(scanbuff," INSTRUMENT_FILTER_NAME = %s", filtname) 
		 == 1) {
	continue;
      }
    }

    if ((isimage == TRUE) && (labelsize=(labelrecs * hrecsize))) {
      if (samplesize!= 8) {
	SetISTR(ISTR_WARNING,"This image is not an 8-bit PDS file.  Sorry.\n");
	fclose(zf);
	return 0;
      }
    } else {
      SetISTR(ISTR_WARNING,
	      "This looks like a PDS/VICAR image, but not enough.\n");
      fclose(zf);
      return 0;
    }

    /* Some day this should be generalized to read <>8-bit files.  If I 
       ever find one that's interesting, I'll do the work.  (That's a
       hint that you should let me know if *you've* one some.) */

  } else if (itype == VICAR) {
    /* we've got a VICAR file.  Let's find out how big the puppy is */

    if (fread(scanbuff,labelsize-labelsofar,1,zf) == 1) {
      if ((tmp = (char *) pds_strstr(scanbuff,"NL=")) == NULL) {
	SetISTR(ISTR_WARNING,"LoadPDS: bad NL in VICAR\n");
	returnp=TRUE;
      }
      
      if (sscanf(tmp,"NL = %d",&y) != 1) {
	SetISTR(ISTR_WARNING,"LoadPDS: bad scan NL in VICAR\n");
	returnp=TRUE;
      }

      if ((tmp = (char *) pds_strstr(scanbuff, "NS=")) == NULL) {
	SetISTR(ISTR_WARNING,"LoadPDS: bad NS in VICAR\n");
	returnp=TRUE;
      }

      if (sscanf(tmp, "NS = %d",&x) != 1) {
	SetISTR(ISTR_WARNING,"LoadPDS: bad scan NS in VICAR\n");
	returnp=TRUE;
      }
    }

  } else {
    SetISTR(ISTR_WARNING,"LoadPDS: Unable to parse data.\n");
    returnp=TRUE;
  }

  if (returnp) {
    fclose(zf);
    return 0;
  }

  /* PDS files tend to have lots of information like this in them.  The
   * following are a few of the more interesting headers.  We'd do more, but
   * there's only so much space in the info box.  I tried to pick a
   * reasonable subset of these to display; if you want others, it's easy
   * to change the set. */

  *infobuff='\0';
  if (*spacecraft) {
    strcat(infobuff,spacecraft);
  }

  if (*target) {
    strcat(infobuff,", ");
    strcat(infobuff,target);
  }

  if (*filtname) {
    strcat(infobuff,", ");
    strcat(infobuff,filtname);
  }

  if (*itime) {
    strcat(infobuff,", ");
    strcat(infobuff,itime);
  }

  SetISTR(ISTR_WARNING,infobuff);

  strcpy(pdsuncompfname,fname);
  ftypstr = "";

  switch (itype) {
  case VICAR:
    sprintf(pinfo->fullInfo,"VICAR, 8 bits per pixel. (%ld bytes)", filesize);
    ftypstr = "VICAR";
    rewind(zf);
    break;

  case PDSFIXED:
  case VIKINGFIXED:
    sprintf(pinfo->fullInfo,"PDS, 8 bits per pixel. (%ld bytes)", filesize);
    ftypstr = "PDS";
    rewind(zf);
    break;

  case PDSVARIABLE:
  case VIKINGVARIABLE:
    sprintf(pinfo->fullInfo,
	    "PDS, 8 bits per pixel, Huffman-encoded. (%ld bytes)", filesize);
    ftypstr = "PDS (Huffman)";
    fclose(zf);

#ifndef VMS
    sprintf(pdsuncompfname,"%s/xvhuffXXXXXX", tmpdir);
    mktemp(pdsuncompfname);
    sprintf(scanbuff,"%s %s - 4 >%s",PDSUNCOMP,fname,pdsuncompfname);
#else
    strcpy(pdsuncompfname,"sys$disk:[]xvhuffXXXXXX");
    mktemp(pdsuncompfname);
    sprintf(scanbuff,"%s %s %s 4",PDSUNCOMP,fname,pdsuncompfname);
#endif

    SetISTR(ISTR_INFO,"De-Huffmanizing '%s'...",fname);

    /* pdsuncomp filters to a raw file */
#ifndef VMS
    if (tempnum=system(scanbuff)) {
      SetISTR(ISTR_WARNING,"Unable to de-Huffmanize '%s'.",fname);
      return 0;
    }
#else
    if (tempnum = !system(scanbuff)) {
      SetISTR(ISTR_WARNING,"Unable to de-Huffmanize '%s'.",fname);
      return 0;
    }
#endif


    zf=fopen(pdsuncompfname,"r");
    if (!zf) {
      SetISTR(ISTR_WARNING,"LoadPDS: can't open uncompressed file %s\n",
	      pdsuncompfname);
      return 0;
    }
  }

  /* skip headers */

  if ( isfixed == TRUE ) {
    fread(scanbuff,labelsize,1,zf);
  }

  image=(byte *)malloc(x*y);
  if (image == NULL) {
    SetISTR(ISTR_WARNING,"LoadPDS: couldn't malloc %d",x*y);
    fclose(zf);
    if (isfixed == FALSE)
      unlink(pdsuncompfname);
    exit(1);
  }

  if ((lssize || lpsize) && ((itype == PDSFIXED) || (itype == VIKINGFIXED)) ) {
    /* ARrrrgh.  Some of these images have crud intermixed with the image, */
    /* preventing us from freading in one fell swoop */
    /* (whatever a fell swoop is */

    for (yy=0; yy<y; yy++) {
      if (lpsize && ((teco=(fread(scanbuff,lpsize,1,zf))) != 1)) {
	SetISTR(ISTR_WARNING, "LoadPDS: unexpected EOF reading prefix");
	fclose(zf);
	return 0;
      }

      if ((teco=(fread(image+(yy*x),x,1,zf))) != 1) {
	SetISTR(ISTR_WARNING, "LoadPDS: unexpected EOF reading line %d",yy);
	fclose(zf);
	return 0;
      }

      if (lssize && ((teco=(fread(scanbuff,lssize,1,zf))) != 1)) {
	SetISTR(ISTR_WARNING, "LoadPDS: unexpected EOF reading suffix");
	fclose(zf);
	return 0;
      }
    }

  } else if ((yy=fread(image,x*y,1,zf)) != 1) {
    SetISTR(ISTR_WARNING,"LoadPDS: error reading image data");
    fclose(zf);
    if (itype==PDSVARIABLE || itype==VIKINGVARIABLE)
      unlink(pdsuncompfname);

    return 0;
  }

    fclose(zf);


  if (isfixed == FALSE)
    unlink(pdsuncompfname);

  pinfo->pic = image;
  pinfo->w   = x;
  pinfo->h    = y;

  /* these are grayscale, so cobble up a ramped colormap */
  /* Viking images on the CD's seem to be inverted.  Sigh */

  switch (itype) {
  case PDSVARIABLE:
  case PDSFIXED:
  case VICAR:
    for (yy=0; yy<=255; yy++) {
      pinfo->r[yy] = pinfo->g[yy] = pinfo->b[yy] = yy;
    }
    break;

  case VIKINGFIXED:
  case VIKINGVARIABLE:
    for (yy=0; yy<=255; yy++) {
      pinfo->r[yy] = pinfo->g[yy] = pinfo->b[yy] = (255-yy);
    }
  }

  pinfo->frmType = -1;   /* can't save as PDS */
  pinfo->colType = F_GREYSCALE;
  sprintf(pinfo->shrtInfo, "%dx%d %s. ", pinfo->w, pinfo->h, ftypstr);
  pinfo->comment = (char *) NULL;

  if (pic8) return 1;
  return 0;
}


#endif /* HAVE_PDS */
