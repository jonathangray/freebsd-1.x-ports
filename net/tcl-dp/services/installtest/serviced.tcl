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
#     AdvertiseService
#     UnadvertiseService
#     ListAllMatches
#     FindService
#     LaunchOnDemand

###########################################################################

#
# The argument configFile should contain the name of a file from
# which to read configuration commands (e.g.  RegisterOnDemandService)
#

set configFile [lindex $argv 0];

# XXX: presently unused:
# XXX: see servicesLoad and servicesSave below.
#   The variable servicesPath should be set to the absolute path 
#   of file where we can load/save the server state.
#
# set servicesPath [lindex $argv 1];

###########################################################################

set services(null) "";
set aliases(null)  "";
set onDemand(null) "";

###########################################################################

# Services get listed in one of two ways:  
#   - AdvertiseService 
#   - RegisterOnDemandService
# 
# AdvertiseService will advertise a running daemon.
# RegisterOnDemandService will register a command that can be used
# to launch a daemon that is not already running.  Once it starts
# running it should connect to the services server and register
# itself with advertise service.
# 
# Clients who call FindService for an "OnDemand" service will be
# returned the special value "ONDEMAND".  If the client wants to
# run it, he should call LaunchOnDemand, wait awhile, a call back
# to see if the service is up.

proc servicesLoad {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global servicesPath;

  catch {uplevel #0 "source $servicesPath"};
}

proc servicesSave {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global aliases;
  global services;
  global servicesPath;

  if [catch {set file [open $servicesPath w]}] {
    puts stdout "Error : Could not access $servicesPath";
    puts stdout "";
    return;
  }

  puts $file "";
  foreach alias   [lsort [array names aliases]] {
    puts $file "set aliases($alias) \"[set aliases($alias)]\";";
  }

  puts $file "";
  foreach service [lsort [array names services]] {
    puts $file "set services($service) \"[set services($service)]\";";
  }

  close $file;
}

###########################################################################

proc ListAliases {} {
  global aliases;
  return [lsort [array names aliases]];
}

proc ListServices {} {
  global services;
  return [lsort [array names services]];
}

#   returns a list of all services matching the pattern
proc ListAllMatches {pat} {
  global aliases;
  global services;

  return [concat [lfilter [array names aliases] $pat] \
                       [lfilter [array names services] $pat]]
}


###########################################################################

proc AliasService {service alias} {
  global aliases;

  if {[catch {set aliases($alias) $service}]} {
    return 0;
  }
  servicesSave;
  return 1;
}

proc AdvertiseService {service host port} {
  global services;

  if {[catch {set services($service) "$host $port"}]} {
    return 0;
  }
  servicesSave;
  return 1;
}

proc RegisterOnDemandService { service command } {
  global onDemand services

  if {[catch {set onDemand($service) $command}]} {
    return 0;
  }
  
  if {[catch {set services($service) "ONDEMAND"}]} {
    return 0;
  }

  servicesSave;
  return 1;
}


###########################################################################

proc UnaliasService {service} {
  global aliases;

  foreach alias [lsort [array names aliases]] {
    if {[match $service $aliases($alias)]} {
      catch {unset aliases($alias)};
      UnaliasService $alias;
    }
  }

  servicesSave;
  return 1;
}

proc UnadvertiseService {service} {
  global services onDemand

  if { [catch "set onDemand($service)"] } {
    catch {unset services($service)};
    UnaliasService $service;
  } else {
    set services($service) "ONDEMAND"
  }
  
  servicesSave;
  return 1;
}


###########################################################################

proc FindExactService {name} {
  global aliases;
  global services;
  
  if {[catch {set aliases($name)} result] == 0} {
    return [FindExactService $result];
  }     
  if {[catch {set services($name)} result] == 0} {
    return $result;
  }
}

proc FindExactServiceNoCase {name} {
  global aliases;
  global services;

  foreach alias [lsort [array names aliases]] {
    if [matchNoCaseExact $name $alias] {
      return [FindExactServiceNoCase $aliases($alias)];
    }
  }

  foreach service [lsort [array names services]] {
    if [matchNoCaseExact $name $service] {
      if {[catch {set services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc FindMatchedService {name} {
  global aliases;
  global services;

  foreach alias [lsort [array names aliases]] {
    if [matchPattern $name $alias] {
      return [FindMatchedService $aliases($alias)];
    }
  }

  foreach service [lsort [array names services]] {
    if [matchPattern $name $service] {
      if {[catch {set services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc FindMatchedServiceNoCase {name} {
  global aliases;
  global services;

  foreach alias [lsort [array names aliases]] {
    if [matchNoCasePattern $name $alias] {
      return [FindMatchedServiceNoCase $aliases($alias)];
    }
  }

  foreach service [lsort [array names services]] {
    if [matchNoCasePattern $name $service] {
      if {[catch {set services($service)} result] == 0} {
        return $result;
      }
    }
  }
}

proc FindService {name} {

    set result [FindExactService $name];
    if {$result!=""} {
        return $result;
    }

    set result [FindExactServiceNoCase $name];
    if {$result!=""} {
        return $result;
    }

    set result [FindMatchedService $name];
    if {$result!=""} {
        return $result;
    }

    set result [FindMatchedServiceNoCase $name];
    return $result;
}


# This will attempt to launch it
# and return "LAUNCHING".  Otherwise it returns "".
proc LaunchOnDemand {name} {
  global onDemand services errorCode
  
  catch {set services($name)} addr
  
  if {$addr!="ONDEMAND" || [catch {set onDemand($name)} cmd]} {
        return "Failed get command, addr == $addr"
  }
  
  #uplevel
  # if {[catch "uplevel #0 $cmd"] } {
  #  return "error running command: $cmd: $errorCode"
  #}
  uplevel #0 $cmd
  set services($name) "LAUNCHING"
  
  # if service hasn't come up and advertised after 60 sec, then remark it
  # as on demand
  after 60000 CleanupOnDemand $name
  return "LAUNCHING"
}

# if a service is still marked as "LAUNCHING" then mark it as
# on demand
proc CleanupOnDemand { $name } {
  global onDemand services
  
  catch {
    if { $services($name)=="LAUNCHING" } {
      set $services($name) "ONDEMAND"
    }
  }
}


######################################################################
# lfilter list pattern
#   returns a list of all elements of the list matching the pattern
proc lfilter {list pattern} {
        set res {}
        foreach elm $list {
                        if { [string match $pattern $elm] } {
                                        lappend res $elm
                        }
        }
        return $res
}

######################################################################
# LookupList servicelist
#   returns a list of {host port} for each service named in the list
proc LookupList {servicelist} {
        set res {}
        
        foreach s $servicelist {
                lappend res [FindExactService $s]
        }
        return $res
}


###########################################################################

dpcapLoad

# read in the config file
if { $configFile!="" } {
  source $configFile
}

set servicesHost [lindex $dpcap(services) 0];
set servicesPort [lindex $dpcap(services) 1];

###########################################################################

servicesLoad;

###########################################################################

if [catch {dp_MakeRPCServer $servicesPort}] {
  puts stdout "Error : Could not start services server.";
  puts stdout "Error : Port $servicesPort on $servicesHost already in use.";
  puts stdout "";
  exit;
}

puts "Started up services server on [exec hostname] port $servicesPort"
puts "Config file: $configFile"
puts "Autostart servers: [array names onDemand]"

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
