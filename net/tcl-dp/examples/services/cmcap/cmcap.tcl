# cmcap.tcl --
#
# This file contains utilities to access the /etc/cmcap file.
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

proc cmcapLoad {} {
  global cmcap;
  if [catch {array size cmcap}] {
    catch {unset cmcap};
    source "/etc/cmcap";
  }
  return;
}

