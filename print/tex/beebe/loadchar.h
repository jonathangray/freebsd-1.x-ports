/* -*-C-*- loadchar.h */
/*-->loadchar*/
/**********************************************************************/
/****************************** loadchar ******************************/
/**********************************************************************/

void
loadchar(c)
BYTE c;		/* character number in current font */

/***********************************************************************
    This simple version will do for now.  Even in a book DVI file, there
    are unlikely to be more than about 25 fonts used and with an average
    of 50 characters  each, this  is about  1250 character  definitions.
    With typical character  sizes of 10  point type on  a 300  dots/inch
    device, each would take a block of (10/72)*300 = 42 dots square,  or
    about 84 words each,  for a total storage  requirement of 84*1250  =
    105000 words.  This is  quite reasonable for  the DEC-20/60 and  VAX
    Unix  implementations,  and  may  be  acceptable  on  large   memory
    microprocessors like the MC68000 and NS16000.

    However, for future expansion, a  reference count of each  character
    is  maintained,  and  the  total  storage  allocated  for  character
    descriptions is recorded.  Then, when  malloc fails, or MAXCACHE  is
    reached, the font  entries can  be scanned  to find  the least  used
    characters whose  raster  storage  can  then  be  freed  and  malloc
    retried.
***********************************************************************/

{
    void (*charyy)();		/* subterfuge to get around PCC-20 bug */
    register UNSIGN32 nwords;	/* how many 32-bit words we need */
    register struct char_entry *tcharptr;

    if ((c < FIRSTPXLCHAR) || (LASTPXLCHAR < c)) /* check character range */
	return;

    tcharptr = &(fontptr->ch[c]);

    if (!VISIBLE(tcharptr))	/* check for empty character rasters */
	return;

    nwords = (UNSIGN32)(((tcharptr->wp+31) >> 5) * (tcharptr->hp));

    tcharptr->rasters = (UNSIGN32*)MALLOC((unsigned)(nwords*sizeof(UNSIGN32)));
    if (tcharptr->rasters == (UNSIGN32*)NULL)
    {
	(void)sprintf(message,"loadchar():  Could not allocate %ld words of \
raster space--used %ld words so far",
	    (long)nwords,(long)cache_size);
	(void)fatal(message);
    }
    tcharptr->refcount = 0;		/* clear reference count */
    cache_size += (INT32)nwords;	/* update cache size record */

    if (fontptr != pfontptr)
	openfont(fontptr->n);

    if (fontfp == (FILE *)NULL)		/* do nothing if no font file */
	return;

    /* Bug workaround: PCC-20 otherwise jumps to charxx instead of *charxx */
    charyy = fontptr->charxx;
    (void)(*charyy)(c,outrow);		/* load character from font file */
}

