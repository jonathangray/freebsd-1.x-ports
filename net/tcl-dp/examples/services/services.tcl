# services.tcl --
#
# This file contains autoloading Tcl/Tk procedures 
# that provide easy (no explicit RPC) access to the 
# services server.
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

###########################################################################

cmcapLoad;

set servicesHost [lindex $cmcap(services) 0];
set servicesPort [lindex $cmcap(services) 1];

###########################################################################

proc ServiceSock {} {
  global servicesSock;
  global servicesHost;
  global servicesPort;
  
  if [catch {set servicesSock} sock] {
    if [catch {MakeRPCClient $servicesHost $servicesPort} sock] {
      error "Error: service server not on $servicesHost:$servicesPort";
    }
  }
  set servicesSock $sock;
  return $servicesSock;
}

proc ServiceFind {service} {
  return [RPC [ServiceSock] FindService $service];
}

proc ServiceAdvertise {service host port} {
  return [RPC [ServiceSock] AdvertiseService $service $host $port];
}

proc ServiceUnadvertise {service} {
  return [RPC [ServiceSock] UnadvertiseService $service];
}

