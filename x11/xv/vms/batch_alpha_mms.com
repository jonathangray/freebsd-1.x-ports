$!========================================================================
$!
$!  Name      : BATCH_MMS.COM
$!
$!  Purpose   : Build XV (v3.00) under VMS via MMS
$!  Suggested usage: Submit /NoPrint /Log = Sys$Disk:[] /Notify BATCH_MMS.COM
$!
$!  Created : 27-OCT-1992   by Rick Dyson  for xv v2.21a
$!  Modified:  9-FEB-1993   by Rick Dyson  for xv v2.21b & ALPHA Support
$!            22-FEB-1993   by Rick Dyson  for xv v3.00
$!
$!========================================================================
$ THIS_PATH = F$Element (0, "]", F$Environment ("PROCEDURE")) + "]"
$ Set Default 'THIS_PATH'
$ MMS /Description = ALPHA.MMS
$ Exit
