/* mp.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */
#ifndef _MP_
#define _MP_

#ifndef MAX_PROCS
#define MAX_PROCS 1
#endif /* !MAX_PROCS */

#define MLPROC_RUNNING     0  /* processor is running   */
#define MLPROC_SUSPENDED   1  /* processor is suspended */
#define MLPROC_NO_PROC     2  /* no processor allocated */

#endif /* !_MP_ */

