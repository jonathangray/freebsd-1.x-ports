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

dpcapLoad;

set servicesHost [lindex $dpcap(services) 0];
set servicesPort [lindex $dpcap(services) 1];

###########################################################################

proc ServiceSock {} {
  global servicesSock;
  global servicesHost;
  global servicesPort;
  
  if [catch {set servicesSock} sock] {
    if [catch {dp_MakeRPCClient $servicesHost $servicesPort} sock] {
      error "Error: service server not on $servicesHost:$servicesPort";
    }
  }
  set servicesSock $sock;
  return $servicesSock;
}

proc ServiceFind {service} {
  return [dp_RPC [ServiceSock] FindService $service];
}

proc ServiceAdvertise {service host port} {
  return [dp_RPC [ServiceSock] AdvertiseService $service $host $port];
}

proc ServiceUnadvertise {service} {
  return [dp_RPC [ServiceSock] UnadvertiseService $service];
}

