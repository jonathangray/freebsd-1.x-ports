# -------------------------------------------------------------------------
# 
# Demo version of "extensions.tcl" file. Assumes shared library
# for blt existing in ../../src/shared/libBLT.so.1.7
#

global tcl_extensions tcl_extloadpath

set file [glob ../../../../src/shared/libBLT.*]
set file [lindex $file 0]
set lib [file tail $file]
set tcl_extloadpath { ../../../../src/shared }

# -------------------------------------------------------------------------
# 
# The format of the an entry in the tcl_extensions array is 
# 
#   initRoutine shlib ?shlib...?
#

# -------------------------------------------------------------------------
#
# 	Put extensions that require Tk after this point
#
# -------------------------------------------------------------------------

#
# BLT library 
#

set tcl_extensions(blt) [list Blt_Init $lib ]

