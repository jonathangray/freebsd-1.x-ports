
#
# killservice -- script for killing a currently advertised service
#

###########################################################################

cmcapLoad;

set servicesHost [lindex $cmcap(services) 0];
set servicesPort [lindex $cmcap(services) 1];

###########################################################################

if { $argc != 1} {
    puts stderr "usage: killservice service"
    exit
}

set service [lindex $argv 0]

if {[catch {MakeRPCClient $servicesHost $servicesPort} ns] == 0} {
    set info [RPC $ns FindService $service]
    CloseRPC $ns
    if { [llength $info] == 0 } {
	puts stderr "killservice: no such service $service"
	exit
    }
    set host [lindex $info 0]
    set port [lindex $info 1]
    if {[catch {MakeRPCClient $host $port} serv] == 0} {
	RDO $serv exit
    } else {
	puts stderr "killservice: error connecting to $service"
    }
} else {
    puts stderr "killservice: error connecting to services server"
}
exit
