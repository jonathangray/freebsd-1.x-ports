/**********************************************************************
* %W% %G%
*
* ss 	:	A SpreadSheet Program
*
* Art's Spreadsheet program.          Art Mulder ( art@cs.ualberta.ca )
* University of Alberta, Department of Computing Science.
***********************************************************************
* disprange.c header file
**********************************************************************/

#ifndef disprange_h
#   define disprange_h

/* 
 * Function Prototypes
 */

    void RangeToggle();
    char * RangeGet();
    void RangeGetNum();
    char * RangeForceInput();

/* OLD */
/**     void GetRangeInput();			**/
/**     int GetRange( minr,minc,maxr,maxc  );	**/

    void startshow();
    void showdr();

#endif /* disprange_h undefined */

/**********************************************************************
*	End
**********************************************************************/
