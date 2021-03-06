#
# Test support routines (adapted from Ousterhout's Tcl set)
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
#            AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
#
#      RCS:  testlib.tcl,v 1.1.1.1 1994/03/21 22:09:51 mmc Exp
# ----------------------------------------------------------------------
#               Copyright (c) 1993  AT&T Bell Laboratories
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of AT&T Bell Laboratories
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# AT&T disclaims all warranties with regard to this software, including
# all implied warranties of merchantability and fitness.  In no event
# shall AT&T be liable for any special, indirect or consequential
# damages or any damages whatsoever resulting from loss of use, data or
# profits, whether in an action of contract, negligence or other
# tortuous action, arising out of or in connection with the use or
# performance of this software.
# ======================================================================

# ----------------------------------------------------------------------
#  USAGE:  test <test-desc> <test-cmd> <check>
#
#  Executes the given test, the evaluates the <check> condition to
#  see if the test passed.  The result from the <test-cmd> is kept
#  in the variable $result.  If this condition evaluates non-zero,
#  the test has passed.  Otherwise, the test has failed.  A variety
#  if checking routines (test_cmp_*) are provided below to make
#  the check condition easier to write.
# ----------------------------------------------------------------------
proc test {desc cmd check} {
    set result [uplevel $cmd]

    if {![expr $check]} {
		puts stdout "-------------------------------------------------------"
		puts stdout ">>>> FAILED TEST <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
		puts stdout "-------------------------------------------------------"
		set lines [split $desc "\n"]
		foreach i $lines {
    		puts stdout $i
		}
		puts stdout "======================================================="
		set lines [split $cmd "\n"]
		set label TEST
		foreach i $lines {
    		puts stdout "   $label | $i"
			set label "    "
		}
		puts stdout "-------------------------------------------------------"
		set lines [split $check "\n"]
		set label CHECK
		foreach i $lines {
			if {$i != ""} {
    			puts stdout "  $label | $i"
				set label "     "
			}
		}
		puts stdout "-------------------------------------------------------"
		set lines [split $result "\n"]
		set label RESULT
		foreach i $lines {
			if {$i != ""} {
    			puts stdout " $label | \$result => $i"
				set label "      "
			}
		}
		puts stdout "======================================================="
		error "tests aborted"
    }
}

# ----------------------------------------------------------------------
#  USAGE:  test_cmp_nums <num1> <num2>
#
#  Compares two numbers to see if they are "equal."  Numbers are
#  "equal" if they have an absolute value greater than 1.0e-6 and they
#  have at least 5 significant figures.  Returns 1/0 for true/false.
# ----------------------------------------------------------------------
proc test_cmp_nums {num1 num2} {
	global TEST_ABS_TOL TEST_REL_TOL

	if {[expr abs($num1)] > $TEST_ABS_TOL &&
	    [expr abs($num2)] > $TEST_ABS_TOL} {
		set avg [expr 0.5*($num1+$num2)]
		set diff [expr abs(($num1-$num2)/$avg)]

		if {$diff > $TEST_REL_TOL} {
			return 0
		}
	}
	return 1
}

# ----------------------------------------------------------------------
#  USAGE:  test_cmp_vectors <list1> <list2>
#
#  Compares two lists of numbers to see if they are "equal."  Vectors
#  are "equal" if elements are "equal" in the numeric sense.
#  Returns 1/0 for true/false.
# ----------------------------------------------------------------------
proc test_cmp_vectors {list1 list2} {
	if {[llength $list1] != [llength $list2]} {
		return 0
	}
	for {set i 0} {$i < [llength $list1]} {incr i} {
		set n1 [lindex $list1 $i]
		set n2 [lindex $list2 $i]

		if {![test_cmp_nums $n1 $n2]} {
			return 0
		}
	}
	return 1
}

# ----------------------------------------------------------------------
#  USAGE:  test_cmp_lists <list1> <list2>
#
#  Compares two lists to see if they are "equal."  Lists are "equal"
#  if they contain exactly the same elements, but perhaps in a
#  different order.  Returns 1/0 for true/false.
# ----------------------------------------------------------------------
proc test_cmp_lists {list1 list2} {
	if {[llength $list1] != [llength $list2]} {
		return 0
	}
	foreach elem $list1 {
		set i [lsearch $list2 $elem]
		if {$i >= 0} {
			set list2 [lreplace $list2 $i $i]
		} else {
			return 0
		}
	}
	return 1
}
