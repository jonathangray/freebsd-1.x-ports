# pref.tcl
#
# User pref.  This uses a table-driven scheme to set a bunch
# of variables in the rest of the application.  The results are
# written out to a Xresources-style file that is read by Preferences_Init
# at startup.
#
# Copyright (c) 1993 Xerox Corporation.
# Use and copying of this software and preparation of derivative works based
# upon this software are permitted. Any distribution of this software or
# derivative works must comply with all applicable United States export
# control laws. This software is made available AS IS, and Xerox Corporation
# makes no warranty about the software, its performance or its conformity to
# any specification.

# A two-level scheme is used
# pref(panes) => list of preference windows
# pref($p,text) => explainatory text
# pref($p,prefs) => list of lists, each sublist looks like
#	{ varname xresname defaultValue Comment HelpMsg }
# The varname can be a simple variable or an array element
# The xresname is an Xresource specification
# The defaultValue can be a list, which turns into a set of radio buttons
# or it can be "ON" or "OFF", which turns into a check box
# or if it is a single string, it turns into an entry containing that string


proc PrefVar { item } { lindex $item 0 }
proc PrefXres { item } { lindex $item 1 }
proc PrefDefault { item } { lindex $item 2 }
proc PrefComment { item } { lindex $item 3 }
proc PrefHelp { item } { lindex $item 4 }

proc Preferences_Init { userDefaults appDefaults } {
    global pref

    set pref(uid) 0
    set pref(panes) {}
    set pref(userDefaults) $userDefaults
    set pref(appDefaults) $appDefaults

    if [catch {option readfile $appDefaults startup} err] {
	Exmh_Status "Error in app-defaults $appDefaults: $err"
    }
    if [file exists $userDefaults] {
	if [catch {option readfile $userDefaults user} err] {
	    Exmh_Status "Error in user-defaults $userDefaults: $err"
	}
    }
}
proc Preferences_Add { id text prefs } {
    global pref

    # Set up the table that drives the UI layout
    set ix [lsearch pref(panes) $id]
    if {$ix < 0} {
	lappend pref(panes) $id
	set pref($id,prefs) $prefs
	set pref($id,text) $text
    } else {
	lappend pref($id,prefs) $prefs
	append pref($id,text) \n$text
    }

    # Initialize the global variable from the option database,
    # else the default value supplied.

    foreach item $prefs {
	set varName [PrefVar $item]
	set xresName [PrefXres $item]
	set value [PrefValue $varName $xresName]
	set default [PrefDefault $item]
	Exmh_Debug Pref_Add $varName $value
	if {$value == {}} {
	    # Set variables that are still not set
	    if {[llength $default] > 1} {
		if {[lindex $default 0] == "CHOICE"} {
		    PrefValueSet $varName [lindex $default 1]
		} else {
		    PrefValueSet $varName $default
		}
	    } else {
		if {$default == "OFF" || $default == "ON"} {
		    # This is a boolean
		    if {$default == "OFF"} {
			PrefValueSet $varName 0
		    } else {
			PrefValueSet $varName 1
		    }
		} else {
		    # This is a string or numeric
		    PrefValueSet $varName $default
		}
	    }
	} else {
	    # Warp booleans to 0 or 1
	    if {$default == "OFF" || $default == "ON"} {
		Exmh_Debug Pref Check $varName $value
		case $value {
		    {0 1} { # ok as is }
		    {true True TRUE} { PrefValueSet $varName 1}
		    {false False FALSE} {PrefValueSet $varName 0}
		    default {
			catch {puts stderr "Bogus boolean value $value for Xresource $xresName"}
			PrefValueSet $varName 0
		    }
		}
	    }
	}
    }
}
# Return the value of the given variable,
# or the value from the xresource database,
# or {} if neither exist
proc PrefValue { _var _xres } {
    set _xresval [option get . $_xres {}]
    if [string match *(* $_var] {
	set _arrayName [lindex [split $_var (] 0]
	global $_arrayName
    } else {
	global $_var
    }
    if [catch {
	set $_var
    } _val ] {
	if {$_xresval != {}} {
	    set $_var $_xresval
	    return $_xresval
	} else {
	    return {}
	}
    } else {
	return $_val
    }
}
# set the value of the variable
proc PrefValueSet { _var _value } {
    if [catch {
	if [string match *(* $_var] {
	    set _arrayName [lindex [split $_var (] 0]
	    global $_arrayName
	} else {
	    global $_var
	}
    }] {
	return ""
    } else {
	return [set $_var $_value]
    }
}
proc PrefEntrySet { entry varName } {
    PrefValueSet $varName [$entry get]
}
proc PreferencesSaveEntries {} {
    global pref PrefEntry
    foreach id $pref(panes) {
	foreach item $pref($id,prefs) {
	    set varName [PrefVar $item]
	    set xresName [PrefXres $item]
	    set value [PrefValue $varName $xresName]
	    if [info exist PrefEntry($varName)] {
		set default [PrefDefault $item]
		if {[llength $default] > 1} {
		    if {[lindex $default 0] != "CHOICE"} {
			PrefEntrySet $PrefEntry($varName) $varName
		    }
		} else {
		    if {$default == "OFF" || $default == "ON"} {
			# This is a boolean
		    } else {
			# This is a string or numeric
			PrefEntrySet $PrefEntry($varName) $varName
		    }
		}
	    }
	}
    }
}
proc PreferencesDismiss {{ix {}}} {
    global exwin pref
    Exwin_Dismiss .pref$ix
    catch {PreferencesNukeItemHelp .prefitemhelp}
    if {$ix == {}} {
	catch {Exwin_Dismiss .prefhelp}
	set ix 0
	foreach id $pref(panes) {
	    catch {Exwin_Dismiss .pref$ix}
	    incr ix
	}
    }
}

proc PreferencesHelp {} {
    if [Exwin_Toplevel .prefhelp "Preferences Help" Help] {
	wm group .prefhelp .pref
	Widget_Label .prefhelp.but label {left fill} \
	    -text "Help for Preferences"
	set helptext "
The Preference settings are divided into a number of
subsections.  These correspond to different modules of
the Exmh implementation.  Use the buttons in the main
Preferences window to select various sections.

Information about each setting will appear if you click on
the short description of the preference item.  You can make
the info window go away by clicking on another label or
by clicking inside the info window.

There are three types of options you can set through the Preferences dialog.
Choices are represented by radio-style buttons where only one button
in the set can be enabled at once.  These take effect immediately.
Booleans are represented by check-sytle buttons.  If the checkbox is
dark, then the option is turned on.  This takes effect immediately.

Numeric and filename settings have entry widgets in which you can
type in a new value.  The new value takes effect when you type
<Return>, or when you click \"Save\".

The Save button will save your settings in a .exmh-defaults file
in your home directory.  This is an Xresources-style file.

The Reset button will restore the settings from defaults and your
last saved pref.

The Dismiss button leaves the settings as they are displayed
and removes the pref window.  Thus, those settings
that \"take effect immediatly\" are still in effect but they
are not saved to your pref file.
"
	set numLines [llength [split $helptext \n]]
	if {$numLines > 25} {set numLines 25}
	set t [Widget_Text .prefhelp $numLines -setgrid true]
	$t insert 1.0 $helptext
    }
}

proc Preferences_Dialog {} {
    global pref
    if [Exwin_Toplevel .pref "Exmh Preferences" Pref] {
	set buttons .pref.but
	$buttons.quit configure -command {PreferencesDismiss}
	Widget_AddBut $buttons save Save {PreferencesSave}
	Widget_AddBut $buttons reset "Reset All" {PreferencesReset}
	Widget_AddBut $buttons help Help {PreferencesHelp}

	set body [Widget_Frame .pref b Rim]
	$body configure -borderwidth 2 -relief raised
	set body [Widget_Frame $body b Pad]
	$body configure -borderwidth 10
	set body [Widget_Frame $body body Body]

	set maxWidth 0
	foreach id $pref(panes) {
	    set len [string length $id]
	    if {$len > $maxWidth} {
		set maxWidth $len
	    }
	}
	set i 0
	foreach id $pref(panes) {
	    Widget_AddBut $body but$i $id [list PreferencesSectionDialog $id] \
		{top}
	    $body.but$i configure -width $maxWidth
	    incr i
	}
    }
}

proc PreferencesSectionDialog { id } {
    global pref env
    set ix [lsearch $pref(panes) $id]
    if {$ix < 0} {
	return
    }
    set buttons .pref$ix.but
    if [Exwin_Toplevel .pref$ix "$id Preferences" Pref] {
	$buttons.quit configure -command [list PreferencesDismiss $ix]
	Widget_AddBut $buttons reset Reset [list PreferencesReset $id]
	Widget_AddBut $buttons next Next [list PreferencesNext $ix] {left}

	Widget_Label $buttons label {left fill} -text "Click labels for more details"

	set body [Widget_Frame .pref$ix b Rim]
	$body configure -borderwidth 2 -relief raised
	set body [Widget_Frame $body b Pad]
	$body configure -borderwidth 10
	set body [Widget_Frame $body body Body]

	set txt [Widget_Text [Widget_Frame $body text] 4]
	$txt insert 1.0 $pref($id,text)
	$txt configure -state disabled
#	Widget_Message $body message -text $pref($id,text)
	set maxWidth 0
	foreach item $pref($id,prefs) {
	    set len [string length [PrefComment $item]]
	    if {$len > $maxWidth} {
		set maxWidth $len
	    }
	}
	foreach item $pref($id,prefs) {
	    PreferencesDialogItem $body $item $maxWidth
	}
    }
    set pref(label) $buttons.label
}
proc PreferencesNext { ix } {
    global pref
    global exwin
    set geo [string trimleft [wm geometry .pref$ix] -x0123456789]
    Exwin_Dismiss .pref$ix
    catch {PreferencesNukeItemHelp .prefitemhelp}
    incr ix
    set id [lindex $pref(panes) $ix]
    if {$id != {}} {
	PreferencesSectionDialog $id
	wm geometry .pref$ix $geo
    }
}

proc PreferencesDialogItem { frame item width } {
    global pref
    incr pref(uid)
    set f [Widget_Frame $frame p$pref(uid) Preference]
    $f configure -borderwidth 2
    Widget_Label $f label {left fill} \
	-text [PrefComment $item] -width $width -relief flat
    bind $f.label <1> [list PreferencesItemHelp  %X %Y [PrefHelp $item]]

    set default [PrefDefault $item]
    if {([llength $default] > 1) && ([lindex $default 0] == "CHOICE")} {
	# >1 This is a list of choices
	foreach choice [lreplace $default 0 0] {
	    incr pref(uid)
	    Widget_RadioBut $f c$pref(uid) $choice [PrefVar $item] {left}
	}
    } else {
	if {$default == "OFF" || $default == "ON"} {
	    # This is a boolean
	    set varName [PrefVar $item]
	    set xresName [PrefXres $item]
	    if {[PrefValue $varName $xresName] == {}} {
		if {$default == "OFF"} {
		    PrefValueSet $varName 0
		} else {
		    PrefValueSet $varName 1
		}
	    }
	    Widget_CheckBut $f check "On" $varName {left}
	} else {
	    # This is a string or numeric
	    global PrefEntry
	    Widget_Entry $f entry {left fill expand} -width 10 -background white -relief sunken -bd 2
	    set PrefEntry([PrefVar $item]) $f.entry

	    set varName [PrefVar $item]
	    set xresName [PrefXres $item]
	    set curValue [PrefValue $varName $xresName]
	    if {$curValue != ""} {
		set default $curValue
	    }
	    $f.entry insert 0 $default
	    bind $f.entry <Return> [list PrefEntrySet %W $varName]
	}
    }
}
proc PreferencesItemHelp { x y text } {
    global pref
    catch {destroy .prefitemhelp}
    if {$text == {}} {
	return
    }
    set self [Widget_Toplevel .prefitemhelp "Item help" Itemhelp [expr $x+10] [expr $y+10]]
    wm transient .prefitemhelp .pref
    Widget_Message $self msg -text $text -aspect 1500
    bind $self.msg <1> {PreferencesNukeItemHelp .prefitemhelp}
    $pref(label) configure -text "Click on popup or another label"
}
proc PreferencesNukeItemHelp { t } {
    global pref
    $pref(label) configure -text ""
    destroy $t
}

proc PreferencesSave {} {
    global pref
    PreferencesSaveEntries
    set newstuff {}
    foreach id $pref(panes) {
	foreach item $pref($id,prefs) {
	    set varName [PrefVar $item]
	    set xresName [PrefXres $item]
	    set value [PrefValue $varName $xresName]
if {0} {
	    set default [PrefDefault $item]
	    if {[llength $default] == 1} {
		if {$default != "OFF" && $default != "ON"} {
		    global PrefEntry
		    set entry $PrefEntry($varName)
		    set value [$entry get]
		}
	    }
}
	    lappend newstuff [format "%s\t%s" *${xresName}: $value]
	}
    }
    Preferences_RewriteSection "Lines below here automatically added" "End Preferences State" $newstuff
    PreferencesReset
    PreferencesDismiss
    Background_Preferences
}
proc Preferences_RewriteSection { boundary1 boundary2 newstuff } {
    global pref
    if [catch {
	set old [open $pref(userDefaults) r]
	set oldValues [split [string trimright [read $old] \n] \n]
	close $old
    }] {
	set oldValues {}
    }
    if [catch {open $pref(userDefaults).new w} out] {
	.pref.but.label configure -text "Cannot save in $pref(userDefaults).new: $out"
	return
    }
    set state "before"
    foreach line $oldValues {
	case $state {
	    "before" {
		if {[string compare $line "!!! $boundary1"] == 0} {
		    set state "inside"
		    puts $out "!!! $boundary1"
		    puts $out "!!! [exec date]"
		    puts $out "!!! Do not edit below here"
		    foreach item $newstuff {
			puts $out $item
		    }
		    puts $out "!!! $boundary2"
		} else {
		    puts $out $line
		}
	    }
	    "inside" {
		if {[string compare $line "!!! $boundary2"] == 0} {
		    set state "after"
		}
	    }
	    "after" {
		puts $out $line
	    }
	}
    }
    if {$state == "before"} {
	puts $out "!!! $boundary1"
	puts $out "!!! [exec date]"
	puts $out "!!! Do not edit below here"
	foreach item $newstuff {
	    puts $out $item
	}
	puts $out "!!! $boundary2"
    }
    close $out
    set new [glob $pref(userDefaults).new]
    set old [file root $new]
    if [catch {exec mv $new $old} err] {
	Exmh_Status "Cannot install $new: $err"
	return
    }
}
proc Preferences_ReadSection { boundary1 boundary2 } {
    global pref
    if [catch {
	set old [open $pref(userDefaults) r]
	set oldValues [split [string trimright [read $old] \n] \n]
	close $old
    }] {
	set oldValues {}
    }
    set state "before"
    set results {}
    foreach line $oldValues {
	case $state {
	    "before" {
		if {[string compare $line "!!! $boundary1"] == 0} {
		    set state "inside"
		}
	    }
	    "inside" {
		if {![regexp {^!!!} $line]} {
		    lappend results $line
		}
		if {[string compare $line "!!! $boundary2"] == 0} {
		    break
		}
	    }
	}
    }
    return $results
}
proc PreferencesReset { {id_in {}} } {
    global pref
    # Re-read user defaults
    option clear
    catch {option readfile $pref(appDefaults) startup}
    catch {option readfile $pref(userDefaults) user}
    # Now set variables
    if {$id_in == {}} {
	set id_in $pref(panes)
    } else {
	set id_in [list $id_in]
    }
    foreach id $id_in {
	foreach item $pref($id,prefs) {
	    set varName [PrefVar $item]
	    set xresName [PrefXres $item]
	    set xresval [option get . $xresName {}]
	    if {$xresval != {}} {
		set default $xresval
	    } else {
		set default [PrefDefault $item]
	    }
	    if {([llength $default] > 1) && ([lindex $default 0] == "CHOICE")} {
		PrefValueSet $varName [lindex $default 1]
	    } else {
		if {$default == "OFF"} {
		    PrefValueSet $varName 0
		} else {
		    if {$default == "ON"} {
			PrefValueSet $varName 1
		    } else {
			global PrefEntry
			if [info exists PrefEntry($varName)] {
			    set entry $PrefEntry($varName)
			    $entry delete 0 end
			    $entry insert 0 $default
			}
			PrefValueSet $varName $default
		    }
		}
	    }
	}
    }
}
proc Preferences_Resource { _varName _rname _default } {
    set _rval [option get . $_rname {}]
    if {$_rval != {}} {
	PrefValueSet $_varName $_rval
    } else {
	PrefValueSet $_varName $_default
    }
}
