puts stdout "Point setup file: [info script]"
wm withdraw .
#
# Keep this in so incremental search works
#
# create a hidden frame that gets the focus during incremental search
frame .incrementalsearch
bind .incrementalsearch <Escape> {
	global oldFocus
	focus $oldFocus
	MessageLine ""
}
bind .incrementalsearch <Return> {
	global oldFocus
	focus $oldFocus
	MessageLine ""
}
bind .incrementalsearch <Any-Key> {SearchCharacter %N %s}
bind .incrementalsearch <Control-s> {RepeatSearch forward}
#
# Motif flag
#
###set tk_strictMotif 1
#
# Setup some variables used in the tcl code 
#
# CHANGE THIS:
set HelpDirectory $PointTclLibrary/help
#
# these work well for an 1280x1024 screen
#
set location1  "502x460"
###set location1  "502x460+0+0"
set location2  "502x460-200+465"
set location3  "502x460-0+0"
set location4  "502x460+0+485"
set browser1   "135x445+510+0"
set browser2   "135x445+651+0"
set browser3   "135x445+787+0"
Option set textGeometry		$location1
Option set browserGeometry	$browser1
#
# Read the default options file
#
source $PointTclLibrary/options.tcl
#
# Set Point options different from the defaults
#
Option set wrapAroundSearches		True
#
# Setup some variables used in the tcl code
#
set wcounter 0
set debugMode 0
#
# set up autoloading
#
global PointTclLibrary
set auto_path "$PointTclLibrary $auto_path"
#
# Class bindings
#
bind Entry <Button-2> {%W insert insert [selection get]; tk_entrySeeCaret %W}
bind Button <2> {tk_butDown %W}
bind Button <ButtonRelease-2> {tk_butUp %W}
#
# read in the other tcl files
#
source $PointTclLibrary/sourcing.tcl

