/***********************************************************************
With non-segmented memory, we  just allocate the bitmap  as a large  2-D
array.  Access to elements is  concealed in the macro BITMAP(y,x)  which
returns the address  of the word  at the (y,x)  position.  On  segmented
memory machines (Intel iAPX) this can  still be done if the bitmap  fits
in one  segment,  and  SEGMEM  will have  been  defined  accordingly  in
dvixxx.c.

For segmented memories, not all compilers support pointer addressing  of
objects larger than a segment, so we implement the bitmap as a vector of
pointers to  raster  lines,  each of  which  is  allocated  dynamically.
Element access is again  concealed in BITMAP(y,x), but  the code has  to
ensure that no pointer is ever advanced outside of a raster line.   This
affects  functions   makechar()  and   prtbmap().   Bitmap   access   in
dispchar(), fillrect(), and  prxbmap() is already  restricted to  single
raster lines, so no changes are necessary there.

getbmap() handles the  necessary allocation  for either  of these,  so
devinit() in each dvixxx remains ignorant of the details.
***********************************************************************/

#if    SEGMEM

UNSIGN32* bitmap[YBIT] =
{
	(UNSIGN32*)NULL	/* only first entry need be initialized */
};
#if    IBM_PC_MICROSOFT
#define BITMAP(y,x) ((UNSIGN32*)normaddr(bitmap[y],(x)<<2))
#define FP_SEG(fp) (*((unsigned *)&(fp) + 1))
#define FP_OFF(fp) (*((unsigned *)&(fp)))

char*
normaddr(p,byte_offset) /* return address p+byte_offset */
char *p;
int byte_offset;	/* byte_offset may be positive or negative */
{
    long address;
    char *q;

    /* Reconstruct 32-bit address from SEGMENT*16 + OFFSET + byte_offset */

    address = (long)FP_SEG(p);	/* long <- unsigned by zero extend */
    address <<= 4;		/* segment real memory address */
    address += (long)FP_OFF(p);	/* *p real memory address */
    address += (long)byte_offset;/* *(p+byte_offset) real memory address */

    /* Renormalize address to OFFSET in 0..15 */

    FP_OFF(q) = (unsigned)(address & 0x0f);
    FP_SEG(q) = (unsigned)(address >> 4);

    return (q);
}
#else /* NOT IBM_PC_MICROSOFT */
#define BITMAP(y,x) (bitmap[y] + (UNSIGN16)(x))
#endif /* IBM_PC_MICROSOFT */

#else /* NOT SEGMEM */

UNSIGN32* bitmap = (UNSIGN32*)NULL;
#define BITMAP(y,x) (bitmap + ((UNSIGN32)XBIT*(UNSIGN32)(y)) + (UNSIGN32)(x))

#endif /* SEGMEM */
