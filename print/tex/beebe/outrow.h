/* -*-C-*- outrow.h */
/*-->outrow*/
/**********************************************************************/
/******************************* outrow *******************************/
/**********************************************************************/

void
outrow(c,yoff)	/* copy img_row[] into rasters[] if allocated, else no-op */
BYTE c;		/* current character value */
UNSIGN16 yoff;	/* offset from top row (0,1,...,hp-1) */
{
    register UNSIGN16 k;	/* loop index */
    register UNSIGN32 *p;	/* pointer into img_row[] */
    register UNSIGN32 *q;	/* pointer into rasters[] */
    register struct char_entry *tcharptr; /* temporary char_entry pointer */
    UNSIGN16 words_per_row;	/* number of raster words to copy */

    tcharptr = &(fontptr->ch[c]);
    if (tcharptr->rasters != (UNSIGN32*)NULL)
    {
	words_per_row = (UNSIGN16)(tcharptr->wp + 31) >> 5;
	p = tcharptr->rasters + yoff*words_per_row;
	q = img_row;
	for (k = words_per_row; k; --k)	/* copy img_row[] into rasters[] */
	    *p++ = *q++;
    }
}
