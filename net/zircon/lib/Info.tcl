#
# Build a Zircon Information Window.
#
proc Info {name args} {
    if [string match {::} $name] {
	set op [lindex $args 0]
	return [eval Info_$op [lrange $args 1 end] ]
    }
    global Icon BF Fg Bg Ft OType popInfo Name defChan zircon
    set OType(info0) Info
    set Name(.@info) info0
    info0 configure -hpos 0 -hbuff {} -open $popInfo \
      -close [$defChan close] -jump 1 -quiet 0 -draw 0 \
      -msg [$defChan msg] \
      -actions 0 -patterns {} -logfile {} -history [$defChan history] \
      -closetime [$defChan closetime] -log {} -menu 0 -join 0 \
      -ops {} -keep 1 -monitor 0
    toplevel $name -relief raised -borderwidth 2 -class Zircon
    wm title $name {Zircon Information Window}
    wm iconname $name [set Icon(info0) {Zircon Info}]
    wm minsize $name 10 1

    frame $name.cmd
    scrollbar $name.cmd.cscroller -orient horizontal \
      -command "$name.cmd.line view"
    emacsEntry $name.cmd.line -scrollcommand "$name.cmd.cscroller set"
    pack $name.cmd.cscroller -side bottom -expand 1 -fill x
    pack $name.cmd.line -side top -expand 1 -fill x
    if $zircon(command) { pack $name.cmd -fill x -side bottom}
    bind $name.cmd.line <Return> { doMisc2 info0 %W }
    set fr [frame $name.cFrm]
    frame $fr.cFrm -relief raised
    set oft $fr.info
    scrollbar $fr.vscroller -command "doScroll $oft"
    text $oft -height 10 -width 80 \
      -yscrollcommand "setScroll $oft $fr.vscroller" 
    rebind $oft
    bind $oft <Configure> {%W yview -pickplace end ; notIdle %W}
    bind $fr <Visibility> {notIdle %W}
    pack $fr.vscroller -side right -fill y -in $fr.cFrm
    pack $oft -side left -expand 1 -fill both -in $fr.cFrm
    pack $fr.cFrm -expand 1 -fill both
    pack $name.cFrm -expand 1 -fill both
    tkwait visibility $name
    set BF(info0) [getOValue $oft font boldFont Font]
    set Fg(info0) [getOValue $oft foreground foreground Foreground]
    set Bg(info0) [getOValue $oft background background Background]
    set Ft(info0) [getOValue $oft font font Font]
    info0 configure -window .@info
    return info0
}
#
proc info0 {op args} {
    switch $op {
    lname -
    name { return @info0 }
    text { return .@info.cFrm.info }
    command {
	    if [lindex $args 0] {
		pack .@info0.cmd
	    } {
		pack .@info0.cmd -forget
	    }
	}
    window { return .@info }
    default { eval channel_call info0 $op $args }
    }
}
