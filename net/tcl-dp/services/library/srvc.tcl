# srvc.tcl --  Services Client library
#
# This file contains autoloading Tcl/Tk procedures 
# that provide easy access to the services server
# and the servers listed there.
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
#   Provides a slightly more robust and easy to use interface to RPC
#   services than that provided by the basic Tcl-DP RPC primitives.
#
#   Features:
#      - Access servers by name rather than file handle
#        (no need to set up and tear down connections manually)
#      - Connection caching to limit tear up and tear down costs.
#      - Transparent reconnection and retry for stale server connections
#        or servers that have been restarted (and have changed addresses)
#      - Cooperation with serviced to support auto-start services
#

###########################################################################
# Basic Client Calls:
#
# srvc_ListServices pat 
#   - gives list of servers registered with the services server
# RPCByName name args
#   - issues the given RPC command at the given host.
#     the host is identified by symbolic name.
# MultiRPCJoin namelist args
#   - runs the given RPC command at all of the servers listed 
#     in namelist and joins all of the results in a list of the
#     form: {{servername results} ... {servername result}}
#
# srvc_GetConn name
#   - returns a file handle for a connection to server. 
#     Client need not call this directly if the above RPC procs
#     are used.
# srvc_RelConn filehandle
#   - release a file handle returned by GetServerConn.  This may or
#     may not cause the connection to be closed

###########################################################################

dpcapLoad;

set srvc_Host [lindex $dpcap(services) 0];
set srvc_Port [lindex $dpcap(services) 1];

###########################################################################


######################################################################
# srvc_ListServices {pat}
#   Get a list of services matching the given pattern.
proc srvc_ListServices {{pat "*"}} {
  return [RPCByName serviced ListAllMatches $pat];
}

######################################################################
# RPCByName {serverName args}
#   severName is a name known to the nameserver for the entities
#   to be called.
# This handles communication to the name server in case of error
proc RPCByName {serverName args} {
  set tries 0
  set done 0
  while { $tries<2 && !$done } {
      #May toss error
      set c [srvc_GetConn $serverName]

      if { [catch {dp_RPC $c eval $args} result] } {
          puts "RPC error: $result"

          #error in performing RPC.   Errors from bad connections
          #look like: file "file3" isn't open.
          if { [regexp {file \"[a-z0-9]*\" isn't open} $result] } {
              errmsg "RPC to $serverName failed.  Will retry"

              #tell the server about the bad connection
              srvc_BadConnection $serverName
              set result ""
              incr tries
          } else {
            # This is the clients error - pass it on
            srvc_RelConn $c
            error $result
          }         
      } else {
        set done 1
      }
  
      srvc_RelConn $c
  }
  
  return $result
}


######################################################################
# MultiRPCJoin {serverlist args}
#   serverlist is a list of names known to the nameserver for the entities
#   to be called.
#
# !!! The current version of this command is extremely inefficient.
# A better version would use multiple RDO's with callbacks so that
# all of the remote servers could compute their results in parallel.
# As it is, the MultiRPC takes time in proportion to the number of
# servers called.
proc MultiRPCJoin {serverlist args} {

  set result {}
  
  foreach s $serverlist {
      if { [catch "RPCByName $s eval $args" res] } {
        errmsg "MulitRPC: $s, $args: $res"
      } else {
        lappend result [list $s $res]
      }
  }
  
  return $result
}


######################################################################
#    srvc_GetConn serverName
#       Establishes connection with remote TclDP server.  Returns
#       file descriptor for communication
proc srvc_GetConn { serverName }  {
    #could check cache here
    if { [set conn [srvc_CacheFind $serverName]] == "" } {       
        # Look up service and make and RPC connection to it
        set conn [srvc_EstablishConnection $serverName]
             
        srvc_CacheAdd $serverName $conn
    }
    
    srvc_Lock $serverName
    return $conn
}

######################################################################
#    srvc_RelConn conn
#       Releases connection to remote server.  Namemgr may then chose
#       to close the connection, or to cache it for future use.
proc srvc_RelConn {conn}  {
    # free it for now
    srvc_UnLock [srvc_FindServiceFor $conn]
}

######################################################################
#    srvc_GetServerHostID serverName
#       Returns the internet address of serverName
proc srvc_GetServerHostID { serverName } {
    return [lindex [srvc_FindSeviceOrLaunch $serverName] 0]
}

######################################################################
#    srvc_GetServerInfo serverName
#      Returns the internet address and port for the given service
proc srvc_GetServerInfo { serverName }  {
    #If we have it cached, look up socket info
    if { ([set conn [srvc_CacheFind $serverName]] != "") &&
          ![catch "GetSocketInfo $conn" res] } {
        #GetSocketConn is an added C Tcl proc.  If user doesn't
        #have it, we'll call the serviced to get the addr
        return $res
    }
    
    #otherwise, query name server for info
    return [srvc_FindSeviceOrLaunch $serverName]
}


proc srvc_CacheAdd { key val } {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks

  if { $srvc_CacheSize >= $srvc_MaxCacheSize } {
      #Purge connection from the cache
      # !!! we should do this randomly
      foreach c [array names srvc_ConnCache] {
        if { !$srvc_ConnLocks($c)>0 } {
          dp_CloseRPC $c  # the dp_atclose callback will remove the cache entry
          break;
        }
      }
  }

  #set up new connection
  set srvc_ConnCache($key) $val
  set srvc_ConnLocks($key) 0
  dp_atclose $val append "srvc_CacheRemove $val"
  incr srvc_CacheSize
}

proc srvc_CacheFind { key } {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks

    if { [catch {set srvc_ConnCache($key)} val] } {
        return ""
    } else {
      return $val
    }
}

proc srvc_CacheRemove { key } {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks

    if {![catch "unset srvc_ConnCache($key)"] } {
        catch "unset srvc_ConnLocks($key)"
        set srvc_CacheSize [expr $srvc_CacheSize-1]
    }
}

proc srvc_FindServiceFor {conn} {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache

  # find the key for the given value
  foreach c [array names srvc_ConnCache] {
      if { $srvc_ConnCache($c)==$conn } {
        return $c
      }
  }
  errmsg "Failed to find service for connection: $conn"
  return ""
}


######################################################################
# srvc_Lock
#    Inc the ref count for the given service
proc srvc_Lock {serviceName} {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks

  incr srvc_ConnLocks($serviceName)
}


######################################################################
# srvc_UnLock
#    Decrement the ref count for the given service
proc srvc_UnLock {serviceName} {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks

  if {$serviceName!=""} {
      catch "set srvc_ConnLocks($serviceName) \
          [expr $srvc_ConnLocks($serviceName)-1]"
  }
}


######################################################################
# srvc_EstablishConnection
#    Open a connection to the given host.
#    The given service should not already have a connection
#    open in the cache. 
proc srvc_EstablishConnection {serverName} {
    for {set tries 0} {$tries<2} {incr tries} {
        set remHP [srvc_FindSeviceOrLaunch $serverName]
        
        if { [catch "dp_MakeRPCClient [lindex $remHP 0] [lindex $remHP 1]" \
                conn] } {
            msg "Unable to establish connection with $serverName"

            # The address we were given doesn't work
            # eliminate connection from our cache and tell sserviced
            # to forget about it
            srvc_BadService $serverName
             msg "retrying"
        } else {
          return $conn
        }
    }

    error "Couldn't establish connect to $serverName"
}



######################################################################
# srvc_FindSeviceOrLaunch
#    Look up service and return host and port.
#    abort with error if not found
#
# This will instruct name server to launch onDemand services 
# as necessary
proc srvc_FindSeviceOrLaunch { service } {
    #special case for the serviced
    if { $service == "serviced" } {
        global srvc_Host srvc_Port

        return [list $srvc_Host $srvc_Port]
    }

    set remHP [srvc_Find $service]
    
    if { $remHP=="ONDEMAND" } {
        msg "Service $service is registered as ONDEMAND"
      
        # Instruct serviced to launch it
        if { [RPCByName serviced LaunchOnDemand $service] != "LAUNCHING" } {
          errmsg "Failed to launch $service"
        }
        return [srvc_CheckLauchingService $service]
    }

    if { $remHP=="LAUCHING" } {
        msg "Service $service is lauching.  Checking..."
        return [CheckLaunchingService $service]
    }

    if { [llength $remHP]!=2 } {
        error "Couldn't find service $service"
    }
    return $remHP
}

proc srvc_Find {service} {
  return [RPCByName serviced FindExactService $service];
}


######################################################################
# srvc_CheckLauchingService
#    Call back name server several times to see if a lauching server
#    is up yet.
proc srvc_CheckLauchingService {name} {  
  # Wait a while and see if server has shown up
  # Try three times
  set tries 0
  while { $tries < 3 } {
      # wait 2, 4, 8 secs
      after [expr {2000<<$tries}]

      msg "Checking for $name"
      set ret [srvc_Find $name]
      if { [llength $ret]==2 } {
          msg "$name is up: $ret"
          return $ret
      }

      msg "$name not up yet: $ret"
      incr tries
  }

  error "Time out lauching $name"
}

######################################################################
#    srvc_BadConnection serverName
#       Used by client to report a server connection that has failed.
#       We will purge this item from our cache and the client
#       can try again (thereby establishing a new connection)
proc srvc_BadConnection {serverName} {
    catch "close $serverName"
    srvc_CacheRemove $serverName
}

######################################################################
#    srvc_BadService serverName
#       Report a service that cannot be connected to.
#       Purge the cache of the entry and report the bad
#       service to the serviced
proc srvc_BadService {serverName} {
    srvc_CacheRemove $serverName
    if {serverName != "serviced"} {
      RPCByName serviced UnadvertiseService $serverName
    }
}


# How many open server connections do we want to allow?
set srvc_MaxCacheSize 6

proc srvc_Init {} {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks
  set srvc_CacheSize 0
  set srvc_ConnCache(null) "";
  set srvc_ConnLocks(null) "";
}


######################################################################
#    srvc_FlushCache
#       Flush all server cache (so that we'll open new connections
#       next time we try to RPC.  Used when we fork
proc srvc_FlushCache {} {
  global srvc_MaxCacheSize srvc_CacheSize srvc_ConnCache srvc_ConnLocks
  
  unset srvc_ConnCache
  unset srvc_ConnLocks
  
  srvc_Init
}

######################################################################

srvc_Init
