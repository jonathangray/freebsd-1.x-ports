
#
# rmservice -- script for removing (unadvertising) a currently 
#           -- advertised service
#

###########################################################################

dpcapLoad;

set dp_servicesHost [lindex $dpcap(services) 0];
set dp_servicesPort [lindex $dpcap(services) 1];

###########################################################################

if {$argc != 1} {
  puts stdout "usage : rmservice service";
  exit;
}

if {[catch {dp_MakeRPCClient $dp_servicesHost $dp_servicesPort} ns] == 0} {
    catch {dp_RPC $ns dp_UnadvertiseService [lindex $argv 0]}
    dp_CloseRPC $ns
}
exit
