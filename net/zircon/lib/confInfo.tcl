#
set zircon(idata1) {
    { {Verbose CTCP} verboseCTCP }
    { {Pop Up Info} popInfo }
    { {Flag Pop Up} noPopup }
    { {No Channel List} noRefresh }
    { {Kill Path} killPath }
}
#
proc confInfo {} {
    set win .@confInfo
    if [winfo exists $win] { popup $win ; return }
    global zircon confData new
    toplevel $win -class Zircon
    wm title $win {General Configuration}
    confInit Info
    frame $win.misc0 -relief raised
    set i 0
    foreach d $zircon(idata1) {
	checkbutton $win.misc0.$i -text [lindex $d 0] \
	  -variable "new([lindex $d 1])"
	pack $win.misc0.$i -side left
	incr i
    }
    frame $win.misc1
    labelEntry 0 $win.misc1.help {-text {Help Service}} $new(helpService) \
      { set new(helpService) [%W get]}
    labelEntry 0 $win.misc1.soff {-text {Sign Off}} $new(signoff) \
      { set new(signoff) [%W get]}
    pack $win.misc1.help $win.misc1.soff -side left -fill x
    frame $win.misc2 -relief raised
    global confData
    global toInfo
    label $win.misc2.label -text "Send to Info :"
    pack $win.misc2.label -side left
    set i 0
    foreach ci $confData(info) {
	set uci [string toupper $ci]
	checkbutton $win.misc2.inf${i} -text $ci \
	  -variable confI${i} -command "doCInfo ${i} ${uci}"
	global confI${i}
	set confI${i} [expr {[lsearch $toInfo $uci] >= 0}]
	pack $win.misc2.inf${i} -side left
	incr i
    }
    frame $win.misc3 -relief raised
    label $win.misc3.label -text "No Confirm :"
    pack $win.misc3.label -side left
    global confData
    global noConfirm
    set i 0
    foreach ci $confData(nconf) {
	set uci [string toupper $ci]
	checkbutton $win.misc3.inf${i} -text $ci \
	  -variable confNC${i} -command "doCNConf ${i} ${uci}"
	global confNC${i}
	set confNC${i} [expr {[lsearch $noConfirm $uci] >= 0}]
	pack $win.misc3.inf${i} -side left
	incr i
    }
    frame $win.filter -relief raised

    checkbutton $win.filter.public -variable new(showPublic) -text "Public"
    checkbutton $win.filter.local -variable new(showLocal) -text "Local"
    checkbutton $win.filter.private -variable new(showPrivate) -text "Private"
    checkbutton $win.filter.topic -variable new(topicOnly) -text "With Topic"

    global minMembers ; set tmp $minMembers
    scale $win.filter.members \
      -from 1 -to 25 -label "Minimum Number of Members" \
      -showvalue 1 -orient horizontal \
      -command {global new ; set new(minMembers) }

    $win.filter.members set $tmp
    pack $win.filter.members -side left -expand 1 -fill x
    pack $win.filter.public $win.filter.local $win.filter.private \
      $win.filter.topic -side left

    labelEntry 0 $win.filter2 {-text {Channel Pattern} -width 16} \
      $new(listPattern) {set new(listPattern) [%W get]}
    labelEntry 0 $win.filter3 {-text {Topic Pattern} -width 16} \
      $new(topicPattern) {set new(topicPattern) [%W get]}
    confMkBtn $win Info
    pack $win.misc0  $win.misc1  $win.misc2 $win.misc3 $win.filter \
      $win.filter $win.filter2 $win.filter3 $win.btn -fill x
}
