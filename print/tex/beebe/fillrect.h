/* -*-C-*- fillrect.h */
/*-->fillrect*/
/**********************************************************************/
/****************************** fillrect ******************************/
/**********************************************************************/

void
fillrect(x,y,width,height)
COORDINATE x,y,width,height;		/* lower left corner, size */

/***********************************************************************
With the page origin (0,0) at the lower-left corner of the bitmap,  draw
a filled rectangle at (x,y).
***********************************************************************/

{
    register UNSIGN32 *p;
    register COORDINATE i,j,xloffset,xroffset,xlpart,xrpart;

    if ( ((x+width) <= 0) || (XSIZE <= x) || ((y+height) <= 0) ||
	(YSIZE <= y) )
	return;		/* Trivial reject -- rectangle outside page */
    else
    {
	xlpart = x/HOST_WORD_SIZE;
	xrpart = (x + width)/HOST_WORD_SIZE;
	xloffset = x % HOST_WORD_SIZE;
	xroffset = (x + width) % HOST_WORD_SIZE;
	for (j = 0; j < height; ++j)	    /* loop over raster lines */
	{
	    if (IN(0,y+j,YBIT-1))
	    {
		p = BITMAP(y+j,xlpart);
		if (IN(0,xlpart,XBIT-1))
		    if (xlpart < xrpart) /* set bits in left partial word */
			*p |= rightones[xloffset];
		    else    /* set bits in middle of left partial word */
			*p |= (rightones[xloffset] & ~rightones[xroffset]);

		++p;
		for (i = xlpart+1; i < xrpart; (++p,++i))
		    if (IN(0,i,XBIT-1))
			*p = (UNSIGN32)ONES;  /* set complete words */

		/* finally, set bits in right partial word */
		if ((xlpart < xrpart) && IN(0,xrpart,XBIT-1))
		    *BITMAP(y+j,xrpart) |= ~rightones[xroffset];
	    }
	}
    }
}

