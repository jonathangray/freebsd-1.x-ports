/* -*-C-*- findpost.h */
/*-->findpost*/
/**********************************************************************/
/****************************** findpost ******************************/
/**********************************************************************/

void
findpost()

{
    register long	postambleptr;
    register BYTE	i;

    (void) FSEEK (dvifp, 0L, 2); /* goto end of file */
    postambleptr = FTELL(dvifp) - 4;
    (void) FSEEK (dvifp, postambleptr, 0);

    while (TRUE)
    {
	(void) FSEEK (dvifp, --(postambleptr), 0);
	if (((i = (BYTE)nosignex(dvifp,(BYTE)1)) != 223) &&
	    (i != DVIFORMAT))
	    (void)fatal("findpost():  Bad end of DVI file");
	if (i == DVIFORMAT)
	    break;
    }
    (void) FSEEK (dvifp, postambleptr - 4, 0);
    (void) FSEEK (dvifp, (long)nosignex(dvifp,(BYTE)4), 0);
}


