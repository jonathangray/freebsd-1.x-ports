/* -*-C-*- dispchar.h */
/*-->dispchar*/
/**********************************************************************/
/****************************** dispchar ******************************/
/**********************************************************************/

void
dispchar(c)
BYTE c;		/* character number in current font */

/***********************************************************************

   This procedure has the delicate  job of OR'ing the current  character
   raster description into the bitmap,  where the character box  extends
   horizontally from  (xcorner  ..  xcorner+wp-1)  and  vertically  from
   (ycorner  ..   ycorner+hp-1).   The  lower  left  corner  coordinates
   (xcorner, ycorner) are  related to  the current point  (xcp, ycp)  as
   follows:

	<------wp------>
    ^	................
    |	................
    |	................
    |	................
    |	................
    |	................
    |	................
    hp  ................
    |	................
    |	................
    |	................
    |	.....o..........       <-- (xcp,ycp) at "o"
    |	................ ^
    |	................ |
    |	................ |--- (hp - yoffp - 1)
    |	................ |
    v	+............... v     <-- (xcorner,ycorner) at "+"
	<--->
	  |
	  |
	 xoffp

   The current PXL file format stores character rasters in 32-bit words,
   with the rightmost portion of the  last word in each line beyond  the
   edge of  the character  being all  0 bits.	For efficiency,  the  OR
   operation is done a word  at a time, and  in general, each such  word
   contributes to two words in the bitmap line.

	     line		    line		   line
   |.........word.........|.........word.........|.........word.........|

  32-bit chunks--> |.....chrast.....|.....chrast.....|.....chrast.....|

		   |<---->|<------->|
		       |       |
     bits_to_current---^       ^--- bits_to_next

   Thus, each  32-bit  chunk  will  be  right-shifted  (filling  vacated
   positions at the left with 0 bits) leaving "bits_to_current" bits  at
   the low end and then OR'd into the current word of the bitmap line.

   Since the C language  right-shift operator may  or may not  propagate
   the sign bit  (which is usually  at the left),  a compile-time  flag,
   ARITHRSHIFT, is necessary to include an extra AND operation to remove
   them  when	the  right-shift   is  implemented   by  an   arithmetic
   (sign-propagating), rather than a logical, shift.

   The 32-bit  chunk will  then  be left-shifted  (with 0  bits  filling
   vacated positions on  the right) leaving  "bits_to_next" bits at  the
   high end and OR'd into the next word of the bitmap line.

   When the host word  size exceeds 32 bits  (e.g. DEC-10 or -20  36-bit
   word), the  first step  may in  fact require  a left  shift, and  the
   second step is then not needed.   This is detected in the code  below
   by "bits_to_next" being negative.

***********************************************************************/
{
    register struct char_entry *tcharptr;
    COORDINATE x,xcorner,ycorner;
    UNSIGN16 ilimit;
    register INT16 bits_to_next;
    register UNSIGN16 i;
    UNSIGN32 word32;
    register UNSIGN32 *p;
    register UNSIGN32 *raster_word;
    register COORDINATE j;

    if ((c < FIRSTPXLCHAR) || (LASTPXLCHAR < c)) /* check character range */
	return;

    tcharptr = &(fontptr->ch[c]);

    if (tcharptr->rasters == (UNSIGN32*)NULL)/* if rasters still on file */
	loadchar(c);			/* go get them */

    if (tcharptr->rasters == (UNSIGN32*)NULL)
	return;	/* character image must be empty */

    tcharptr->refcount++;		/* update reference count */
    raster_word = tcharptr->rasters;	/* pointer to first raster word */

    xcorner = xcp - tcharptr->xoffp;
    ycorner = ycp - (tcharptr->hp - tcharptr->yoffp - 1);

    if (DBGOPT(DBG_CHAR_DUMP))
    {
	NEWLINE(stdout);
        (void)printf(
	    "dispchar(): (xcp,ycp) = (%d,%d) (xcorner,ycorner) = (%d,%d)",
	    xcp,ycp,xcorner,ycorner);
	NEWLINE(stdout);
	(void)printf("            (wp,hp) = (%d,%d)  (xoffp,yoffp) = (%d,%d)",
	    tcharptr->wp,tcharptr->hp,tcharptr->xoffp,tcharptr->yoffp);
	NEWLINE(stdout);
	ilimit = (UNSIGN16)((tcharptr->wp + 31) >> 5);
	for (j = tcharptr->hp; j > 0; --j)
	{
	    for (i = 0; i < ilimit; ++i)
	        (void)printf(" %08lx",*raster_word++);
	    NEWLINE(stdout);
	}
	raster_word = tcharptr->rasters;
    }

    for (j = tcharptr->hp; j > 0; --j)	/* loop over hp rasters from */
    {					/* top to bottom */
	x = xcorner;			/* select horizontal position */
	p = BITMAP(ycorner+j-1,x/HOST_WORD_SIZE); /* and find word on line */
	ilimit = (UNSIGN16)((tcharptr->wp + 31) >> 5);
	for (i = 0; i < ilimit; ++i)
	{		    /* loop over current line */
	    word32 = *raster_word++; /* get 32-bit portion of raster */
	    bits_to_next = (INT16)((x % HOST_WORD_SIZE) - HOST_WORD_SIZE + 32);

#if    (HOST_WORD_SIZE > 32)
	    if (bits_to_next < 0)   /* then must left shift character raster */
		*p |= (word32 << (-bits_to_next));  /* and OR into line */
	    else
#endif

	    {
		*p |=
#if    (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD)
/* these compilers correctly use a logical right shift for */
/* unsigned values */
#else
#if    ARITHRSHIFT	    /* arithmetic right shift propagates a sign */
			    /* bit which must be cleared by this AND  */
		      rightones[bits_to_next] &
#endif /* ARITHRSHIFT */
#endif /* (IBM_PC_LATTICE | IBM_PC_MICROSOFT | IBM_PC_WIZARD) */

		      (word32 >> bits_to_next);     /* OR into line */

		if (bits_to_next > 0)
		    *++p |= (word32 << (HOST_WORD_SIZE - bits_to_next));
			    /* OR in any spill into next word */
		else if (bits_to_next == 0)
		    ++p;    /* ended at word boundary, so start new one */
	    }
	    x += 32;	    /* and update horizontal position */
	}
    }
}

