: see end of file for revision information
case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsnews (with variable substitutions)"
cat >newsnews <<!GROK!THIS!
			*** NEWS NEWS ***

Welcome to trn 3.4.  This is mainly a bugfix release of version 3.3
and a few improvements.  Trn is "threaded read news", based on rn.

You can type 'h' at any prompt to display a summary of the commands that
are available.

This message will not be displayed again unless it is updated with new
information.

Wayne Davison  <davison@borland.com>
!GROK!THIS!
$eunicefix newsnews
#
# $Id: newsnews.SH,v 1.5 1994/02/22 01:47:52 nate Exp $
#
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 

