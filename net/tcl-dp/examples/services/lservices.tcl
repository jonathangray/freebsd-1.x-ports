
#
# lservices -- script for listing currently advertised services
#

###########################################################################

cmcapLoad;

set servicesHost [lindex $cmcap(services) 0];
set servicesPort [lindex $cmcap(services) 1];

###########################################################################

if {[catch {MakeRPCClient $servicesHost $servicesPort} ns] == 0} {
    catch {puts stdout [RPC $ns ListServices]}
    CloseRPC $ns
}
exit

