puts stdout "Tk startup file: [info script]"
if [catch {source [info library]/init.tcl} error_msg] then {
	puts stderr "ERROR: could not find tcl initialization code (init.tcl),"
	puts stderr "       tcl may not be installed correctly."
	puts stderr $error_msg
	exit 1
}
if [catch {source $tk_library/tk.tcl} error_msg] then {
	puts stderr "ERROR: could not find tk initialization code (tk.tcl),"
	puts stderr "       tk may not be installed correctly."
	puts stderr $error_msg
	exit 1
}
set use_default 1
if [file exists ~/.ptrc] then {
	if [catch {source ~/.ptrc} error_msg] then {
		puts stderr \
	"ERROR: .ptrc contained an error, using system default ptsetup.tcl"
		puts stderr $error_msg
	} else {
		set use_default 0
	}
} else {
	if [file exists ./ptsetup.tcl] then {
		if [catch {source ./ptsetup.tcl} error_msg] then {
			puts stderr \
"ERROR: ./ptsetup.tcl contained an error, using system default ptsetup.tcl"
			puts stderr $error_msg
		} else {
			set use_default 0
		}
	}
}
if $use_default {
	global PointTclLibrary
	if [file exists $PointTclLibrary/ptsetup.tcl] {
		source $PointTclLibrary/ptsetup.tcl
	} else {
		puts stderr \
		"ERROR: cannot find a ptsetup.tcl file. POINT WILL FAIL."
		exit 1
	}
}
