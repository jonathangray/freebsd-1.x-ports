
#
# This Tcl/Tk script is used by the Makefile to help install
# services server related scripts.
#

if {$argc <= 0} {
  puts stderr "Error: no install directory.";
  return;
}

set directoryInstall [lindex $argv 0];

####################################################################

puts stdout "Installing services server scripts in $directoryInstall";

####################################################################

proc emit {message} {
  global file;
  puts $file $message;
}

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f killservice
set file [open killservice w]

emit "#!/bin/sh -f";
emit "$directoryInstall/swish -f $directoryInstall/killservice.tcl \$1";
emit "";

close $file;
exec chmod 755 killservice;
exec cp killservice $directoryInstall/killservice;
exec cp killservice.tcl $directoryInstall/killservice.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f rmservice
set file [open rmservice w]

emit "#!/bin/sh -f";
emit "$directoryInstall/swish -f $directoryInstall/rmservice.tcl \$1";
emit "";

close $file;
exec chmod 755 rmservice;
exec cp rmservice $directoryInstall/rmservice;
exec cp rmservice.tcl $directoryInstall/rmservice.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f lservices
set file [open lservices w]

emit "#!/bin/sh -f";
emit "$directoryInstall/swish -f $directoryInstall/lservices.tcl";
emit "";

close $file;
exec chmod 755 lservices;
exec cp lservices $directoryInstall/lservices;
exec cp lservices.tcl $directoryInstall/lservices.tcl;

####################################################################
puts stdout "." nonewline;
flush stdout;

exec rm -f serviced
set file [open serviced w]

emit "#!/bin/sh -f"
emit "$directoryInstall/swish -d -f $directoryInstall/serviced.tcl";
emit "";

close $file;
exec chmod 755 serviced;
exec cp serviced $directoryInstall/serviced;
exec cp serviced.tcl $directoryInstall/serviced.tcl;

####################################################################
puts stdout "done";

exit;
