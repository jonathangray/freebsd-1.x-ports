# -------------------------------------------------------------------------
# 
# Edit this file to reflect to locations of the shared libraries for
# the extensions.  The contents of the variable "tcl_extloadpath"
# tell the extension command where to look for the libraries.
#
set tcl_extloadpath { 
    /usr/local/blt/lib
    /usr/local/lib
    /opt/cellar/tcl/tcl7.3/lib
    /opt/cellar/tk/tk3.6/lib
    /opt/cellar/deli/lib
    /opt/cellar/deli/blt/lib
    /opt/cellar/tcl/tcl7.3/lib
    /opt/cellar/tk/tk3.6/lib
}

# -------------------------------------------------------------------------
# 
# The format of the an entry in the tcl_extensions array is 
# 
#   initRoutine shlib ?shlib...?
#

#
# [incr tcl] library
#

set tcl_extensions(itcl) { Itcl_Init libitcl.so.1.3 }

if ![info exists tk_library] {
    return
}

# -------------------------------------------------------------------------
#
# 	Put extensions that require Tk after this point
#
# -------------------------------------------------------------------------

#
# BLT library 
#

set tcl_extensions(blt) { Blt_Init libBLT.so.1.7 }

#
# VUW widget set (bargraph, piechart, etc.)
#

#set tcl_extensions(vuw) { Vuw_Init libVUW.so.1.0 }
