#-----------------------------------------------------------------------------
# TclInit.tcl -- Extended Tcl initialization.
#-----------------------------------------------------------------------------
# $Id: TclInit.tcl,v 1.1 1994/02/09 01:54:07 jkh Exp $
#-----------------------------------------------------------------------------

#
# Unknown command trap handler.
#
proc unknown args {
    if [auto_load [lindex $args 0]] {
        return [uplevel 1 $args]
    }
    if {([info proc tclx_unknown2] == "") && ![auto_load tclx_unknown2]} {
        error "can't find tclx_unknown2 on auto_path"
    }
    return [tclx_unknown2 $args]
}

set auto_index(buildpackageindex) {source [info library]/buildidx.tcl}

# == Put any code you want all Tcl programs to include here. ==

if !$tcl_interactive return

# == Interactive Tcl session initialization ==

if ![info exists tcl_prompt1] {
    set tcl_prompt1 {global argv0; return [file tail $argv0]>}
}
if ![info exists tcl_prompt2] {
    set tcl_prompt2 {return =>}
}
