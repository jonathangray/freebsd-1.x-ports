# cutbuffer.tcl
#	Stub for missing cutbuffer command
#	The best thing to do is add the tkCutbuffer.c code to your wish.
#	However, if that is not done, then these stub routines mask
#	the missing functionality

proc cutbuffer {args} {
    global cut_priv
    if ![info exists cut_priv(init)] {
	set cut_priv(init) 1
	Exmh_Debug "cutbuffer command missing (add tkCutbuffer.c to wish)"
    }
    return {}
}
