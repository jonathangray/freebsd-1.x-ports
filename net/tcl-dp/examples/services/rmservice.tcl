
#
# rmservice -- script for removing (unadvertising) a currently 
#           -- advertised service
#

###########################################################################

cmcapLoad;

set servicesHost [lindex $cmcap(services) 0];
set servicesPort [lindex $cmcap(services) 1];

###########################################################################

if {$argc != 1} {
  puts stdout "usage : rmservice service";
  exit;
}

if {[catch {MakeRPCClient $servicesHost $servicesPort} ns] == 0} {
    catch {RPC $ns UnadvertiseService [lindex $argv 0]}
    CloseRPC $ns
}
exit
