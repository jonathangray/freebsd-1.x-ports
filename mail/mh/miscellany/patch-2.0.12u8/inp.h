/* /usr/home/jtc/CVSROOT/mh/miscellany/patch-2.0.12u8/inp.h,v 1.1.1.1 1993/01/30 04:40:53 jtc Exp
 *
 * inp.h,v
 * Revision 1.1.1.1  1993/01/30  04:40:53  jtc
 * mh-6.8
 *
 * Revision 2.0  86/09/17  15:37:25  lwall
 * Baseline for netwide release.
 * 
 */

EXT LINENUM input_lines INIT(0);	/* how long is input file in lines */
EXT LINENUM last_frozen_line INIT(0);	/* how many input lines have been */
					/* irretractibly output */

bool rev_in_string();
void scan_input();
bool plan_a();			/* returns false if insufficient memory */
void plan_b();
char *ifetch();

