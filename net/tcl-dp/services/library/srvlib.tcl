# servlib.tcl
#
# functions for setting up "well behaved" tcl services
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
#   Simplifies writing well behaved Tcl-DP services by helping to
#   keep information listed at the serviced consistent.
#
#   Features:
#      - One step server setup (srvlib_Init)
#      - Will relist service with serviced if it "forgets" about this
#        serviced (because serviced went down).
#      - Will shutdown this instance of the service if the serviced is
#        found down or another instance of this service has registered
#        itself.
#      - Unregisters service when process exits
#

###########################################################################
# Client Calls:
#
# srvlib_Init name
#   - Should be called at startup to register service with the service
#     daemon. 



######################################################################
# srvlib_Init
#   Should be called once by server at startup to register with the
#   serviced.
#
#   Creates a service port and advertises it with the services server.
#   Returns the port number of the RPC listening port
proc srvlib_Init {name} {
    msg "ServiceUtil: $name"
    set host [exec hostname]
    set port [dp_MakeRPCServer 0]

    msg "ServiceUtil:  registering at $host $port"
    if { [catch "srvlib_Advertise $name $host $port"] } {
        error "Unable to register with service daemon"
    }

    # Set up a checkup function that will check whether we're
    # still advertised with the services server (in case it went down)
    # every 2 minutes
    msg "ServiceUtil:  setting up every cmd"
    every 120000 "srvlib_Check $name $host $port"
    
    # when we go down, tell the serviced
    dp_atexit prepend "srvlib_Unadvertise $name"
    
    return $port
}


proc srvlib_Advertise {service host port} {
  return [RPCByName serviced AdvertiseService $service $host $port];
}

proc srvlib_Unadvertise {service} {
  return [RPCByName serviced UnadvertiseService $service];
}

proc srvlib_Check {name host port} {
    # contact the services server to make sure that we're still listed.
    # If someone else is listed under our name, exit.
    # If the service is "OnDemand" then relist ourselves.
    if {[catch "srvc_Find $name" hp] } {
        # Services server is down, kill ourselves
        errmsg "Service server is down... Shutting down."
        srvlib_Shutdown
    } else {
        if { $hp != [list $host $port] } {
            if { ($hp == "ONDEMAND") || ($hp=="") } {
                # readvertise ourselves
                srvlib_Advertise $name $host $port
            } else {
                # another instance of us is up.  Die.
                errmsg "Another instance registered with serviced. Bailing."
                srvlib_Shutdown
            }
        }
    }
}

proc srvlib_Shutdown {} {
  # We've been replaced at the nameserver and need to shut down.
  # ?? Is there anyway to tell whether we have any clients that
  # we shouldn't shutdown on?
  exit
}
