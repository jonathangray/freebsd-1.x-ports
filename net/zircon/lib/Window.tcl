#
proc Window {name args} {
    global WTO Wname
    set ln [string tolower $name]
    set this [newName Window]
    set WTO($ln) $this
    set Wname($this) $ln
    proc $this {args} " eval window_call $this \$args "
    toplevel $ln -class Zircon
    return $this
}
#
proc window_call {this op args} {
    switch $op {
    name { global Wname ; return $Wname($this) }
    default { eval window_$op $this $args }
    }
}
#
proc window_setIcon {this chan title} {
    global Icon
    set win [$this name]
    wm iconname $win $title
    set Icon($win) $title
    if ![string match {} [set icn [$chan icon]]] {
	global IconBM
	set IconBM($win) $icn
	wm iconbitmap $win [lindex $icn 0]
    }
}
#
proc window_popup {this} {
   set w [$this name]
   wm $w deiconify
   raise $w
}
#
proc window_delete {this} {
    global WTO Icon IconBM
    set w [$this name]
    bind $w.cFrm <Destroy> {}
    catch {unset WTO($w) Icon($w) IconBM($w)}
    foreach v {OType Wname} {
	global $v ; catch {unset ${v}($this)}
    }
    rename $this {}
    destroy $w
}
#
proc window_iconify {this} { wm iconify [$this name] }
#
proc window_configure {this args} {
    while {![string match {} $args]} {
	set val [lindex $args 1]
	set name [lindex $args 0]
	set opt [string range $name 1 end]
	switch -glob -- $name {
	-title -
	-iconname -
	-iconbitmap { wm $opt [$this name] $val }
	-grid -
	-minsize { eval wm $opt [$this name] $val }
	-*  { setGlob "C$opt" $this $val }
	}
	set args [lrange $args 2 end]
    }
}
#
proc window_build {args} {
}
#
proc Window_find {name} {
    global WTO
    set ln [string tolower name]
    return [expr {[info exists WTO($ln)] ? $WTO($ln) : {nil}}]
}
#
proc Window_make {name} {
    global WTO
    set ln [string tolower name]
    return [expr {[info exists WTO($ln)] ? $WTO($ln) : [Window $name]}]
}
