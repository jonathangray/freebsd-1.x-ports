/******************************************************************************/
/*                                                                            */
/*  Title       : monthname.c                                                 */
/*  Author      : Manfred Bester, DL5KR                                       */
/*  Date        : 01Nov84                                                     */
/*  Last change : 05May92                                                     */
/*                                                                            */
/*  Synopsis    : This function returns the name of the nth month.            */
/*                                                                            */
/******************************************************************************/

char *monthname(month)

int  month;

{
    static char *name[] = { "xxx", 
                            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    return((month < 1 || month > 12) ? name[0] : name[month]);
}

/******************************************************************************/
/*                                                                            */
/*  end of function monthname.c                                               */
/*                                                                            */
/******************************************************************************/
