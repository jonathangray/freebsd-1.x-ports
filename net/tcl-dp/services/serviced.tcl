#
# serviced.tcl -- script for the services server daemon
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
#   A simple network name server for distributed Tcl-DP applications.
#
#   Features:
#      - Maintains list of host/portnum pairs keyed by service name
#        Clients can do text match search on registered services.
#      - Allows config file to register "auto-start" services that
#        will be lauched when client asks for them and an instance is
#        not already running.  By using "rsh" as launch command,
#        services can be lauched on remote machines.
#

###########################################################################
# Client Calls:
#
#  Clients and servers normally access the serviced transparently
#  the the srvc.tcl and srvlib.tcl auto-load libraries.
#  These libraries call:
#     dp_AdvertiseService
#     dp_UnadvertiseService
#     dp_ListAllMatches
#     dp_FindService
#     dp_LaunchOnDemand

###########################################################################

#
# The argument dp_configFile should contain the name of a file from
# which to read configuration commands (e.g.  dp_RegisterOnDemandService)
#

set dp_configFile [lindex $argv 0];

# XXX: presently unused:
# XXX: see dp_servicesLoad and dp_servicesSave below.
#   The variable dp_servicesPath should be set to the absolute path 
#   of file where we can load/save the server state.
#
# set dp_servicesPath [lindex $argv 1];

###########################################################################

set dp_services(null) "";
set dp_aliases(null)  "";
set dp_onDemand(null) "";

###########################################################################

# Services get listed in one of two ways:  
#   - dp_AdvertiseService 
#   - dp_RegisterOnDemandService
# 
# dp_AdvertiseService will advertise a running daemon.
# dp_RegisterOnDemandService will register a command that can be used
# to launch a daemon that is not already running.  Once it starts
# running it should connect to the services server and register
# itself with advertise service.
# 
# Clients who call dp_FindService for an "OnDemand" service will be
# returned the special value "ONDEMAND".  If the client wants to
# run it, he should call dp_LaunchOnDemand, wait awhile, a call back
# to see if the service is up.

proc dp_servicesLoad {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global dp_servicesPath;

  catch {uplevel #0 "source $dp_servicesPath"};
}

proc dp_servicesSave {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global dp_aliases;
  global dp_services;
  global dp_servicesPath;

  if [catch {set file [open $dp_servicesPath w]}] {
    puts stdout "Error : Could not access $dp_servicesPath";
    puts stdout "";
    return;
  }

  puts $file "";
  foreach alias   [lsort [array names dp_aliases]] {
    puts $file "set dp_aliases($alias) \"[set dp_aliases($alias)]\";";
  }

  puts $file "";
  foreach service [lsort [array names dp_services]] {
    puts $file "set dp_services($service) \"[set dp_services($service)]\";";
  }

  close $file;
}

###########################################################################

proc dp_ListAliases {} {
  global dp_aliases;
  return [lsort [array names dp_aliases]];
}

proc dp_ListServices {} {
  global dp_services;
  return [lsort [array names dp_services]];
}

#   returns a list of all dp_services matching the pattern
proc dp_ListAllMatches {pat} {
  global dp_aliases;
  global dp_services;

  return [concat [dp_lfilter [array names dp_aliases] $pat] \
                       [dp_lfilter [array names dp_services] $pat]]
}


###########################################################################

proc dp_AliasService {service alias} {
  global dp_aliases;

  if {[catch {set dp_aliases($alias) $service}]} {
    return 0;
  }
  dp_servicesSave;
  return 1;
}

proc dp_AdvertiseService {service host port} {
  global dp_services;

  if {[catch {set dp_services($service) "$host $port"}]} {
    return 0;
  }
  dp_servicesSave;
  return 1;
}

proc dp_RegisterOnDemandService { service command } {
  global dp_onDemand dp_services

  if {[catch {set dp_onDemand($service) $command}]} {
    return 0;
  }
  
  if {[catch {set dp_services($service) "ONDEMAND"}]} {
    return 0;
  }

  dp_servicesSave;
  return 1;
}


###########################################################################

proc dp_UnaliasService {service} {
  global dp_aliases;

  foreach alias [lsort [array names dp_aliases]] {
    if {[match $service $dp_aliases($alias)]} {
      catch {unset dp_aliases($alias)};
      dp_UnaliasService $alias;
    }
  }

  dp_servicesSave;
  return 1;
}

proc dp_UnadvertiseService {service} {
  global dp_services dp_onDemand

  if { [catch "set dp_onDemand($service)"] } {
    catch {unset dp_services($service)};
    dp_UnaliasService $service;
  } else {
    set dp_services($service) "ONDEMAND"
  }
  
  dp_servicesSave;
  return 1;
}


###########################################################################

proc dp_FindExactService {name} {
  global dp_aliases;
  global dp_services;
  
  if {[catch {set dp_aliases($name)} result] == 0} {
    return [dp_FindExactService $result];
  }     
  if {[catch {set dp_services($name)} result] == 0} {
    return $result;
  }
}

proc dp_FindExactServiceNoCase {name} {
  global dp_aliases;
  global dp_services;

  foreach alias [lsort [array names dp_aliases]] {
    if [matchNoCaseExact $name $alias] {
      return [dp_FindExactServiceNoCase $dp_aliases($alias)];
    }
  }

  foreach service [lsort [array names dp_services]] {
    if [matchNoCaseExact $name $service] {
      if {[catch {set dp_services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc dp_FindMatchedService {name} {
  global dp_aliases;
  global dp_services;

  foreach alias [lsort [array names dp_aliases]] {
    if [matchPattern $name $alias] {
      return [dp_FindMatchedService $dp_aliases($alias)];
    }
  }

  foreach service [lsort [array names dp_services]] {
    if [matchPattern $name $service] {
      if {[catch {set dp_services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc dp_FindMatchedServiceNoCase {name} {
  global dp_aliases;
  global dp_services;

  foreach alias [lsort [array names dp_aliases]] {
    if [matchNoCasePattern $name $alias] {
      return [dp_FindMatchedServiceNoCase $dp_aliases($alias)];
    }
  }

  foreach service [lsort [array names dp_services]] {
    if [matchNoCasePattern $name $service] {
      if {[catch {set dp_services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc dp_FindService {name} {

    set result [dp_FindExactService $name];
    if {$result!=""} {
        return $result;
    }

    set result [dp_FindExactServiceNoCase $name];
    if {$result!=""} {
        return $result;
    }

    set result [dp_FindMatchedService $name];
    if {$result!=""} {
        return $result;
    }

    set result [dp_FindMatchedServiceNoCase $name];
    return $result;
}


# This will attempt to launch it
# and return "LAUNCHING".  Otherwise it returns "".
proc dp_LaunchOnDemand {name} {
  global dp_onDemand dp_services errorCode
  
  catch {set dp_services($name)} addr
  
  if {$addr!="ONDEMAND" || [catch {set dp_onDemand($name)} cmd]} {
        return "Failed get command, addr == $addr"
  }
  
  #uplevel
  # if {[catch "uplevel #0 $cmd"] } {
  #  return "error running command: $cmd: $errorCode"
  #}
  uplevel #0 $cmd
  set dp_services($name) "LAUNCHING"
  
  # if service hasn't come up and advertised after 60 sec, then remark it
  # as on demand
  after 60000 dp_CleanupOnDemand $name
  return "LAUNCHING"
}

# if a service is still marked as "LAUNCHING" then mark it as
# on demand
proc dp_CleanupOnDemand { $name } {
  global dp_onDemand dp_services
  
  catch {
    if { $dp_services($name)=="LAUNCHING" } {
      set $dp_services($name) "ONDEMAND"
    }
  }
}


######################################################################
# dp_lfilter list pattern
#   returns a list of all elements of the list matching the pattern
proc dp_lfilter {list pattern} {
        set res {}
        foreach elm $list {
                        if { [string match $pattern $elm] } {
                                        lappend res $elm
                        }
        }
        return $res
}

######################################################################
# dp_LookupList servicelist
#   returns a list of {host port} for each service named in the list
proc dp_LookupList {servicelist} {
        set res {}
        
        foreach s $servicelist {
                lappend res [dp_FindExactService $s]
        }
        return $res
}


###########################################################################

dpcapLoad

# read in the config file
if { $dp_configFile!="" } {
  source $dp_configFile
}

set dp_servicesHost [lindex $dpcap(services) 0];
set dp_servicesPort [lindex $dpcap(services) 1];

###########################################################################

dp_servicesLoad;

###########################################################################

if [catch {dp_MakeRPCServer $dp_servicesPort}] {
  puts stdout "Error : Could not start services server.";
  puts stdout "Error : Port $dp_servicesPort on $dp_servicesHost already in use.";
  puts stdout "";
  exit;
}

puts "Started up services server on [exec hostname] port $dp_servicesPort"
puts "Config file: $dp_configFile"
puts "Autostart servers: [array names dp_onDemand]"

###########################################################################

#
# Make services server RPC safe.
#

#auto_load_all;

set    auto_noexec 1;
rename auto_reset {};

rename dp_MakeRPCServer {};
rename dp_MakeRPCClient {};

# should be uncommented to make server (somewhat) secure
# if'ed out for debugging.
if {0} {
  rename dp_connect {};
  
  rename open {};
  rename exec {};
  rename proc {};
  rename rename {};
  
  close stderr;
  close stdout;
}

unset dp_configFile
