/* -*-C-*- getbmap.h */
/*-->getbmap*/
/**********************************************************************/
/****************************** getbmap *******************************/
/**********************************************************************/

void
getbmap()				/* allocate bitmap array */
{
#if    SEGMEM
    register UNSIGN16 y;

    if (bitmap[0] == (UNSIGN32*)NULL)
    {
	for (y = 0; y < (UNSIGN16)YBIT; ++y)
	{
	    bitmap[y] = (UNSIGN32*)MALLOC( (unsigned)((XBIT) * \
	        sizeof(UNSIGN32)) );
	    if (bitmap[y] == (UNSIGN32*)NULL)
		(void)fatal(
		    "getbmap():  Cannot allocate space for page image bitmap");
	}
    }
#else /* NOT SEGMEM */
    if (bitmap == (UNSIGN32*)NULL)
    {
	bitmap =
	(UNSIGN32*)MALLOC(((UNSIGN32)(XBIT))*((UNSIGN32)(YBIT))*sizeof(UNSIGN32));
	if (bitmap == (UNSIGN32*)NULL)
	    (void)fatal(
		"getbmap():  Cannot allocate space for page image bitmap");
    }
#endif /* SEGMEM */
}

