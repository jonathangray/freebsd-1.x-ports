#
# Procs for the class User
#
proc User_list {} { global Unick ; return [array names Unick] }
#
proc User_save {desc} {
    foreach fr [User :: friends] {
	set ln "User [$fr name] -friend 1"
	if [$fr isNotify] { append ln " -notify 1" }
	puts $desc $ln
    }
}
#
proc User_friends {} {
    set l {}
    foreach fr [User :: list] {
	if [$fr isFriend] { lappend l $fr }
    }
    return $l
}
#
proc User_find {nk} {
    global UTO
    set name [string tolower $nk]
    if [info exists UTO($name)] { return $UTO($name) } { return nil }
}
#
proc User_make {nk} {
    global UTO
    set name [string tolower $nk]
    if [info exists UTO($name)] { return $UTO($name) } { return [User $nk] }
}
#
proc User_pack {where} {
    makeArray ${where}UTO
    foreach u [User :: friends] { $u pack $where }
}
#
proc User_unpack {where} {
    global ${where}UTO
    foreach u [array names ${where}UTO] { [set ${where}UTO($u)] unpack $where }
    foreach v {Unick Ulnick Unotify Ufriend UTO} {
	global ${where}${v}
	unset ${where}${v}
    }
}
