
#
# lservices -- script for listing currently advertised services
#

###########################################################################

dpcapLoad;

set dp_servicesHost [lindex $dpcap(services) 0];
set dp_servicesPort [lindex $dpcap(services) 1];

###########################################################################

if {[catch {dp_MakeRPCClient $dp_servicesHost $dp_servicesPort} ns] == 0} {
    catch {puts stdout [dp_RPC $ns dp_ListServices]}
    dp_CloseRPC $ns
}
exit

