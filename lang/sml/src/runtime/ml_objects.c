/* ml_objects.c
 *
 * COPYRIGHT 1990 by AT&T Bell Laboratories.
 */

#include "ml_state.h"
#include "ml_types.h"

/* the null string */
#ifdef THINK_C
       int string0[2] = {MAKE_DESC(0, TAG_string), 0};
#else
static int string0[2] = {MAKE_DESC(0, TAG_string), 0};
#endif


/* ML_alloc_string:
 * Allocate and initialize an ML string.
 */
ML_val_t ML_alloc_string (msp, s)
    MLState_ptr msp;
    char	*s;
{
    register int len, n;
    ML_val_t	res;

    len = strlen(s);
    if (len == 0)
	return PTR_CtoML(&string0[1]);
    else if (len == 1)
	return ((ML_val_t)INT_CtoML(*s));
    else {
	n = (len + 3) >> 2;
	ML_alloc_write (msp, 0, MAKE_DESC(len, TAG_string));
	res = ML_alloc (msp, n);
	strncpy ((char *)PTR_MLtoC(res), s, len);
	return res;
    }

} /* end of ML_alloc_string. */

/* make_str_list:
 * Make a ML list of ML strings from a NULL terminated (char *) vector.
 */
ML_val_t make_str_list (msp, vec)
    MLState_ptr msp;
    char	**vec;
{
    register int i;
    ML_val_t	l;

    for (i = 0;  vec[i] != 0;  i++)
	continue;
    for (l = ML_nil;  --i >= 0; ) {
	ML_val_t s = ML_alloc_string (msp, vec[i]);
	l = ML_cons (msp, s, l);
    }

    return l;
}


/* ML_eqstr:
 * ML string equality.
 */
int ML_eqstr (s1, s2)
    ML_val_t	    s1, s2;
{
    register int l;

    if (s1 == s2)
	return 1;
    else if ((l = OBJ_LEN(s1)) != OBJ_LEN(s2))
	return 0;
    else
	return (strncmp((char *)PTR_MLtoC(s1), (char *)PTR_MLtoC(s2), l) == 0);
}
