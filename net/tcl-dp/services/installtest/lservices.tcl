
#
# lservices -- script for listing currently advertised services
#

###########################################################################

dpcapLoad;

set servicesHost [lindex $dpcap(services) 0];
set servicesPort [lindex $dpcap(services) 1];

###########################################################################

if {[catch {dp_MakeRPCClient $servicesHost $servicesPort} ns] == 0} {
    catch {puts stdout [dp_RPC $ns ListServices]}
    dp_CloseRPC $ns
}
exit

