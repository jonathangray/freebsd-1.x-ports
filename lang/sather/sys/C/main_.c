/* * Last edited: May 22 14:11 1991 (om) */
/* File: sather/sys/C/main_.c
   Author: Chu-Cheow Lim
   Created: Wed May 22 14:11:08 1991
   Copyright (C) International Computer Science Institute, 1991

   COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
   and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
   LICENSE contained in the file: "sather/doc/license.txt" of the Sather
   distribution. The license is also available from ICSI, 1947 Center
   St., Suite 600, Berkeley CA 94704, USA.
*/

#include "all_.h"

extern rt_init_();
extern int main_();

main(argc, argv)
int argc;
ptr argv;
{
  rt_init_();
  return main_(argc, argv);
}
