$! This command file builds all of the Computer Modern font files,
$! using the parameter files described in Computer Modern Typesetting,
$! Volume E.
$!
$! First, mount the disk containing the MetaFont files.  You will have
$! to tailor these lines to mount the disk on which you stored the
$! MetaFont distribution.  Here it is done by invoking a command file.
$!
$@sys$login:mount_font_disk
$!
$! Define the locations of the MetaFont programs and set the default 
$! disk directory for this process to be the build area followed by 
$! the library area.  You will need to edit this line to refer to the 
$! correct file, and you will need to edit the file for your installation.
$!
$@disk$fonts:[sauter.tex.fonts.cm_build]setup
$!
$! Since this batch job runs for a long time, the next line can be used
$! if the job must be resumbitted because of a system failure to avoid
$! repeating work which has already been done.
$!
$ goto restart
$!
$! If the batch job is to be restarted after a lot of work has already
$! been done, move the next line to the first font that was not
$! completed successfully.
$!
$restart:
$!
$! Now invoke the BUILD_FONT.COM file for each font.  Each invocation
$! causes the .TFM and seven PXL files to be built.
$!
$@build_font CMB10
$@build_font CMBSY10
$@build_font CMBX10
$@build_font CMBX12
$@build_font CMBX5
$@build_font CMBX6
$@build_font CMBX7
$@build_font CMBX8
$@build_font CMBX9
$@build_font CMBXSL10
$@build_font CMBXTI10
$@build_font CMCSC10
$@build_font CMDUNH10
$@build_font CMEX10
$@build_font CMFF10
$@build_font CMFI10
$@build_font CMFIB8
$@build_font CMINCH
$@build_font CMITT10
$@build_font CMMI10
$@build_font CMMI12
$@build_font CMMI5
$@build_font CMMI6
$@build_font CMMI7
$@build_font CMMI8
$@build_font CMMI9
$@build_font CMMIB10
$@build_font CMR10
$@build_font CMR12
$@build_font CMR17
$@build_font CMR5
$@build_font CMR6
$@build_font CMR7
$@build_font CMR8
$@build_font CMR9
$@build_font CMSL10
$@build_font CMSL12
$@build_font CMSL8
$@build_font CMSL9
$@build_font CMSLTT10
$@build_font CMSS10
$@build_font CMSS12
$@build_font CMSS17
$@build_font CMSS8
$@build_font CMSS9
$@build_font CMSSBX10
$@build_font CMSSDC10
$@build_font CMSSI10
$@build_font CMSSI12
$@build_font CMSSI17
$@build_font CMSSI8
$@build_font CMSSI9
$@build_font CMSSQ8
$@build_font CMSSQI8
$@build_font CMSY10
$@build_font CMSY5
$@build_font CMSY6
$@build_font CMSY7
$@build_font CMSY8
$@build_font CMSY9
$@build_font CMTCSC10
$@build_font CMTEX10
$@build_font CMTEX8
$@build_font CMTEX9
$@build_font CMTI10
$@build_font CMTI12
$@build_font CMTI7
$@build_font CMTI8
$@build_font CMTI9
$@build_font CMTT10
$@build_font CMTT12
$@build_font CMTT8
$@build_font CMTT9
$@build_font CMU10
$@build_font CMVTT10

