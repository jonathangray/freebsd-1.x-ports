
#
# serviced.tcl -- script for the services server daemon
#

###########################################################################

#
# The variable servicesPath should be set to the absolute path 
# of the directory where we can load/save the file serviced.data.
#
# XXX: presently unused!
# XXX: see servicesLoad and servicesSave below.
#

set servicesPath [lindex $argv 0];

###########################################################################

cmcapLoad;

set servicesHost [lindex $cmcap(services) 0];
set servicesPort [lindex $cmcap(services) 1];

###########################################################################

set services(null) "";
set aliases(null)  "";

###########################################################################

proc servicesLoad {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global servicesPath;

  catch {uplevel #0 "source $servicesPath/serviced.data"};
}

proc servicesSave {} {

  # XXX: services server keeps no persistent state.
  #
  return;

  global aliases;
  global services;
  global servicesPath;

  if [catch {set file [open $servicesPath/serviced.data w]}] {
    puts stdout "Error : Could not access $servicesPath/serviced.data";
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
  global services;

  catch {unset services($service)};
  UnaliasService $service;

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
    if {[string length $result] > 0} {
	return $result;
    }

    set result [FindExactServiceNoCase $name];
    if {[string length $result] > 0} {
	return $result;
    }

    set result [FindMatchedService $name];
    if {[string length $result] > 0} {
	return $result;
    }

    set result [FindMatchedServiceNoCase $name];
    if {[string length $result] > 0} {
	return $result;
    }
}

###########################################################################

servicesLoad;

###########################################################################

if [catch {MakeRPCServer $servicesPort}] {
  puts stdout "Error : Could not start services server.";
  puts stdout "Error : Port $servicesPort on $servicesHost already in use.";
  puts stdout "";
  exit;
}

###########################################################################

#
# Make services server RPC safe.
#

auto_load_all;

set    auto_noexec 1;
rename auto_reset {};

rename MakeRPCServer {};
rename MakeRPCClient {};

rename connect {};

rename open {};
rename exec {};
rename proc {};
rename rename {};

close stderr;
close stdout;

