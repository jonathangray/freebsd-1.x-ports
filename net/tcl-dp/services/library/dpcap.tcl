# dpcap.tcl --
#
# This file contains utilities to access the /etc/dpcap file.
#
# Copyright 1992 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#

proc dpcapLoad {} {
  global dpcap;
  if [catch {array size dpcap}] {
    catch {unset dpcap};
    source "/etc/dpcap";
  }
  return;
}

