: see end of file for revision information
case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsnews (with variable substitutions)"
cat >newsnews <<!GROK!THIS!
			*** NEWS NEWS ***

Welcome to trn.  There are more options to trn than you want to think about,
so we won't list them here.  If you want to find out about them, read the
manpage.  There are some important things to remember, though:

  * Trn is an extension of rn.  Where possible, the command syntax is the same.
  * To access all the new features, specify the options -x and -X.  These
    options MAY be on by default, but it won't hurt to be redundant.
  * Single-character commands don't require a carriage return -- only
    commands that let you type in an argument.
  * At ANY prompt, you may type 'h' for help.  There are many different help
    menus, depending on the context.  Also, typing <esc>h in the middle of a
    multi-character command will list escape substitutions.
  * Typing a space to any prompt means to do the normal thing.  You could
    spend all day reading news and never hit anything but the space bar.

This message will not be displayed again unless it is updated with new
information.

Wayne Davison  <davison@borland.com>
!GROK!THIS!
$eunicefix newsnews
#
# $Id: newsnews.SH,v 1.2 1993/07/26 19:12:46 nate Exp $
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

