# acl.tcl --
#
# Access control list (acl) implementation for Tcl
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

###########################################################################
#
# Add a "plused" item to an acl.  Returns new acl
#

proc Acl+ {acl str} {
    lappend acl [list 1 $str]
}

###########################################################################
#
# Add a "minused" item to an acl.  Returns new ac
#

proc Acl- {acl str} {
    lappend acl [list 0 $str]
}

###########################################################################
#
# Check if a string is "allowed" in an acl
#

proc AclCheck {acl str} {
    set result 1
    foreach elt $acl {
	if {[string match [lindex $elt 1] $str] == 1} {
	    set result [lindex $elt 0]
	}
    }
    return $result
}
