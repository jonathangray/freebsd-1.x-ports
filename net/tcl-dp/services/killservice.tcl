
#
# killservice -- script for killing a currently advertised service
#

###########################################################################

dpcapLoad;

set dp_servicesHost [lindex $dpcap(services) 0];
set dp_servicesPort [lindex $dpcap(services) 1];

###########################################################################

if { $argc != 1} {
    puts stderr "usage: killservice service"
    exit
}

set service [lindex $argv 0]

if {[catch {dp_MakeRPCClient $dp_servicesHost $dp_servicesPort} ns] == 0} {
    set info [dp_RPC $ns dp_FindService $service]
    dp_CloseRPC $ns
    if { [llength $info] == 0 } {
	puts stderr "killservice: no such service $service"
	exit
    }
    set host [lindex $info 0]
    set port [lindex $info 1]
    if {[catch {dp_MakeRPCClient $host $port} serv] == 0} {
	dp_RDO $serv exit
    } else {
	puts stderr "killservice: error connecting to $service"
    }
} else {
    puts stderr "killservice: error connecting to services server"
}
exit
