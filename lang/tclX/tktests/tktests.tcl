# tktests.tcl
#
# Script to run Tk tests with wishx  Should be from this directory
# with an argument containing the Tk source path.
#

if {[llength $argv] != 1} {
    puts stderr "Path to the Tk source directory should be the only arg"
    exit 1
}

cd [lindex $argv 0]/tests

source all

exit 0


