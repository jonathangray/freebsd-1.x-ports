/*
 * $Id: dysize.c,v 1.1 1993/08/17 09:42:29 alm Exp $
 */
#ifdef NEED_DYSIZE
int
dysize(y)
int y;
{
    
	if (y % 4 == 0 &&
	    y % 100 != 0 || y % 400 == 0)
		return(366);		/* ----------> */
	else
		return(365);		/* ----------> */
    
}
#endif /* NEED_DYSIZE */
