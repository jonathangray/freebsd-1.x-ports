#
#
# Create a browser window
#
#
proc BrowserWindow {geometry} {

	global wcounter location1 location2 location3
	global screen_width screen_height

	set wcounter [expr $wcounter+1]
	set name [format ".bw%05d" $wcounter]

	toplevel $name -class PtBrowser -relief raised
	wm geometry $name [FixGeometry $geometry]
	wm positionfrom $name user
	wm minsize $name 0 0
	bind $name <Enter>	"EnterBrowser $name"
	bind $name <Configure>	"Configure $name"

	# create the list of open windows
	# first a frame to contain the scrollbar and the list
	frame $name.openList

	# then a scrollbar
	if [Option get tkScrolling] {
		scrollbar $name.openList.scroll -relief raised \
			-command "$name.openList.list yview"
	} else {
		scrollbar $name.openList.scroll -relief raised
		bind $name.openList.scroll <ButtonPress-1> \
					"PtScroll 1 $name.openList.list %y"
		bind $name.openList.scroll <ButtonRelease-1> "PtStopScroll"
		bind $name.openList.scroll <Leave> "PtStopScroll"
		bind $name.openList.scroll <ButtonPress-2> \
					"PtScroll 2 $name.openList.list %y"
		bind $name.openList.scroll <ButtonPress-3> \
					"PtScroll 3 $name.openList.list %y"
		bind $name.openList.scroll <ButtonRelease-3> "PtStopScroll"
		bind $name.openList.scroll <B2-Motion> \
					"PtScroll 2 $name.openList.list %y"
	}

	# then a list
	listbox $name.openList.list -relief raised \
		-geometry 10x3 -yscrollcommand "$name.openList.scroll set"
	tk_listboxSingleSelect $name.openList.list
	bind $name.openList.list <Button-1> \
		"RaiseListWindow \[%W nearest %y\] $location1
			RaiseWindow"
	bind $name.openList.list <Button-2> \
		"RaiseListWindow \[%W nearest %y\] $location2
			RaiseWindow"
	bind $name.openList.list <Button-3> \
		"RaiseListWindow \[%W nearest %y\] $location3
			RaiseWindow"
	$name.openList.list insert 0 "No open files"

	# then pack then in
	pack append $name.openList \
		$name.openList.scroll {left fill} \
		$name.openList.list {right fill expand}

	# create the resizer
	frame $name.resizer
	global PointTclLibrary
	button $name.resizer.up -bitmap "@$PointTclLibrary/up.bm" \
		-padx 0 -pady 0 \
		-command "ChangeGeometry $name.openList.list -1"
	button $name.resizer.down -bitmap "@$PointTclLibrary/down.bm" \
		-padx 0 -pady 0 \
		-command "ChangeGeometry $name.openList.list  1"
	pack append $name.resizer \
	$name.resizer.up {left fill expand} \
	$name.resizer.down {right fill expand}

	# create the menu bar
	global BrowserMenuSpec
	MakeMenubar $name.menu $BrowserMenuSpec
	browserMenuBindings $name.menu

	# create the list of files in the current directory
	# first a frame to contain the scrollbar and the list
	frame $name.fileList

	# then a scrollbar
	if [Option get tkScrolling] {
		scrollbar $name.fileList.scroll -relief raised \
			-command "$name.fileList.list yview"
	} else {
		scrollbar $name.fileList.scroll -relief raised
		bind $name.fileList.scroll <ButtonPress-1> \
					"PtScroll 1 $name.fileList.list %y"
		bind $name.fileList.scroll <ButtonPress-2> \
					"PtScroll 2 $name.fileList.list %y"
		bind $name.fileList.scroll <ButtonPress-3> \
					"PtScroll 3 $name.fileList.list %y"
		bind $name.fileList.scroll <B2-Motion> \
					"PtScroll 2 $name.fileList.list %y"
	}

	# then a list
	listbox $name.fileList.list -relief raised -geometry 20x10 \
		-yscrollcommand "$name.fileList.scroll set"
	bind $name.fileList.list <Double-Button-1> "OpenFileOrCD 1"
	bind $name.fileList.list <2> "[bind Listbox <1>]"
	bind $name.fileList.list <B2-Motion> "[bind Listbox <B1-Motion>]"
	bind $name.fileList.list <Double-Button-2> "OpenFileOrCD 2"
	bind $name.fileList.list <3> "[bind Listbox <1>]"
	bind $name.fileList.list <B3-Motion> "[bind Listbox <B1-Motion>]"
	bind $name.fileList.list <Double-Button-3> "OpenFileOrCD 3"
	tk_listboxSingleSelect $name.fileList.list
	pt_listboxSingleSelect 2 $name.fileList.list
	pt_listboxSingleSelect 3 $name.fileList.list
	# then pack then in
	pack append $name.fileList \
		$name.fileList.scroll {left fill} \
		$name.fileList.list {right fill expand}

	# pack everything in the frame
	pack append $name \
		$name.resizer {top fill} \
		$name.openList {top fill} \
		$name.menu {top fill} \
		$name.fileList {top fill expand}
	set screen_width [winfo screenwidth $name]
	set screen_height [winfo screenheight $name]
	return $name
}

proc ChangeGeometry {w by} {
	set geom [lindex [$w configure -geometry] 4]
	set i [string first "x" $geom]
	set oldWide [string range $geom 0 [expr $i-1]]
	set oldHigh [string range $geom [expr $i+1] end]
	set newHigh [expr $oldHigh+$by]
	if {$newHigh=="0"} { return }
	$w configure -geometry [format "%sx%s" $oldWide $newHigh]
}

proc PtScroll {how list y} {
	global PtContinuousScrolling
	if [Option get button1ScrollsDown] {
		if {$how==1} {
			set how 3
		} else {
			if {$how==3} {
				set how 1
			}
		}
	}
	if {$how==1} {
		set nearest [$list nearest $y]
		set top [$list nearest 0]
		set up [expr $top-$nearest]
		if {$up==0} { set up -1 }
		set new_top [expr $top+$up]
		if {$new_top<0} {set new_top 0}
		$list yview $new_top
###		set PtContinuousScrolling 1
###		after 350 PtScrollAgain $list $up
	} else {
		if {$how==2} {
			set high [winfo height $list]
			set size [$list size]
			set new_top [expr ($y*$size)/$high]
			$list yview $new_top
		} else {
			set top [$list nearest 0]
			set new_top [$list nearest $y]
			if {$top==$new_top} { incr new_top }
			$list yview $new_top
###			set PtContinuousScrolling 1
###			set down [expr $new_top-$top]
###			after 350 PtScrollAgain $list $down
		}
	}
}

proc PtScrollAgain {list incr} {
	global PtContinuousScrolling
	if !$PtContinuousScrolling { return }
	set top [$list nearest 0]
	set new_top [expr $top+$incr]
	if {$new_top<0} {set new_top 0}
	$list yview $new_top
	after 50 PtScrollAgain $list $incr
}

proc PtStopScroll {} {
	global PtContinuousScrolling
	set PtContinuousScrolling 0
}

# do it once to make sure the varialbe PtContinuousScrolling gets created.
PtStopScroll

proc TextWindow {geometry} {
	set ret [TextWindow2 $geometry left]
#
# This code is intended to show a busy cursor while the window is being
# mapped.  It doesn't quite work so skip it.
#
#	ChangeCursor busy
#	MappedYet $ret
#
	return $ret
}

proc MappedYet {name} {
	if [winfo ismapped $name] {
		ChangeCursor current
	}
}

#
#
# Create a text window
#
#
proc TextWindow2 {geometry {scrollSide left}} {

	global wcounter

	set wcounter [expr $wcounter+1]
	set name [format ".tw%05d" $wcounter]

	toplevel $name -class PtText -relief raised
	wm positionfrom $name program
	wm geometry $name [FixGeometry $geometry]
	wm positionfrom $name program
	wm minsize $name 0 0
	bind $name <Enter> "EnterText $name"

	# the menu bar at the top (the window manager provides the title)
	global TextMenuSpec
	MakeMenubar $name.menu $TextMenuSpec
	TextMenuBindings $name.menu

	# create a message line
	entry $name.msg -relief raised

	# a frame to hold the vertical scroll bar and the text window
	frame $name.vScrollAndText -relief raised
	scrollbar $name.vScrollAndText.vScroll -relief raised \
		-width 12 -command "VScroll tk"
	bind $name.vScrollAndText.vScroll <ButtonPress> {VScroll press %y %b}
	bind $name.vScrollAndText.vScroll <B1-Motion> {VScroll press %y 1}
	bind $name.vScrollAndText.vScroll <B2-Motion> {VScroll motion %y 2}
	bind $name.vScrollAndText.vScroll <B3-Motion> {VScroll press %y 3}
	bind $name.vScrollAndText.vScroll <ButtonRelease-1> \
						{VScroll release %y 1}
	bind $name.vScrollAndText.vScroll <ButtonRelease-3> \
						{VScroll release %y 3}
	frame $name.vScrollAndText.lineNumbers -bg antiquewhite \
		-geometry 30x30
	frame $name.vScrollAndText.text
	TextBindings $name.vScrollAndText.text $name
	if [string compare $scrollSide left] {
		pack append $name.vScrollAndText \
			$name.vScrollAndText.text {left expand fill} \
			$name.vScrollAndText.vScroll {right fill}
	} else {
		pack append $name.vScrollAndText \
			$name.vScrollAndText.vScroll {left fill} \
			$name.vScrollAndText.text {right expand fill}
	}

	# a frame to hold the filler (splitter) and the horizontal scroll bar
	frame $name.splitterAndHScroll
	frame $name.splitterAndHScroll.splitter -geometry "12x12"
	scrollbar $name.splitterAndHScroll.hScroll -orient horizontal \
		-relief raised -width 10 -command "HScroll tk"
	bind $name.splitterAndHScroll.hScroll <ButtonPress> \
							{HScroll press %x %b}
	bind $name.splitterAndHScroll.hScroll <B1-Motion> \
							{HScroll press %x 1}
	bind $name.splitterAndHScroll.hScroll <B2-Motion> \
							{HScroll motion %x 2}
	bind $name.splitterAndHScroll.hScroll <B3-Motion> \
							{HScroll press %x 3}
	bind $name.splitterAndHScroll.hScroll <ButtonRelease-1> \
							{HScroll release %x 1}
	bind $name.splitterAndHScroll.hScroll <ButtonRelease-3> \
							{HScroll release %x 3}
	pack append $name.splitterAndHScroll \
		$name.splitterAndHScroll.splitter {left fill} \
		$name.splitterAndHScroll.hScroll {right expand fill}

	# now pack them all up in a column
	pack append $name \
		$name.menu {top fill} \
		$name.msg {top fill} \
		$name.vScrollAndText {top expand fill} \
		$name.splitterAndHScroll {bottom fill}
	return $name
}

