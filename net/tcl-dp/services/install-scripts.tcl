
#
# This Tcl/Tk script is used by the Makefile to help install
# services server related scripts.
#

if {$argc <= 1} {
  puts stderr "Install error: imust pass LIBDIR and BINDIR.";
  return;
}

set libdir [lindex $argv 0];
set bindir [lindex $argv 1];

####################################################################

puts stdout "Installing services server scripts in $bindir";
puts stdout "Installing services server library files in $libdir";

####################################################################

proc emit {message} {
  global file;
  puts $file $message;
}

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f killservice
set file [open $bindir/killservice w]

emit "#!/bin/sh -f";
emit "dpwish -notk -f $libdir/killservice.tcl \$1";
emit "";

close $file;
exec chmod 755 $bindir/killservice;
exec cp killservice.tcl $libdir/killservice.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f rmservice
set file [open $bindir/rmservice w]

emit "#!/bin/sh -f";
emit "dpwish -notk -f $libdir/rmservice.tcl \$1";
emit "";

close $file;
exec chmod 755 $bindir/rmservice;
exec cp rmservice.tcl $libdir/rmservice.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f lservices
set file [open $bindir/lservices w]

emit "#!/bin/sh -f";
emit "dpwish -notk -f $libdir/lservices.tcl";
emit "";

close $file;
exec chmod 755 $bindir/lservices;
exec cp lservices.tcl $libdir/lservices.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f serviced
set file [open $bindir/serviced w]

emit "#!/bin/sh -f"
emit "phoenix dpwish -notk -bg -f $libdir/serviced.tcl";
emit "";

close $file;
exec chmod 755 $bindir/serviced;
exec cp serviced.tcl $libdir/serviced.tcl;

####################################################################
puts stdout "done";

exit;
