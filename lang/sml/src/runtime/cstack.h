/* 
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * cstack.h
 *
 * when we are in ML code there is a C stack frame allocated.  this file
 * defines the contents of that frame.  this is different for each
 * architecture.  right now i only put HPPA in here because i don't want to
 * mess with other files, but they should prob be moved in too.
 */

#ifdef HPPA
/* space for
   C-callee save regs r3-r18    16
   locals: mlstate...            5
   C-caller save regs r8-r11     4
   other mul/div things to save  
   but must be 0 mod 64, so make it 192 */

#define ml_framesize     192

/* some magic HP stuff at beginning of frame */
#define mlstate_offset   -36
#define startgc_offset   -40
#define mul_offset       -44
#define div_offset       -48
#define cvti2d_offset    -52
#define callersave_offset(i) -68+(i-8)*4
#define othersave_offset(i)  -92+i*4
/* some space left in stack frame here */
#define c_reg_offset(i)  -ml_framesize+(i-3)*4
/* end of frame */
#define rp_offset        -20
#endif
