$! This command file builds all of the Computer Modern font files,
$! using parameter files which take the point size as input.
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
$! Build each font by specifying the desired family and point size.
$! The lines which are not commented out are the 75 standard Computer
$! Modern fonts.  Using these paramter files any point size within
$! any of the standard families can be constructed, by interpolation
$! between standard sizes or extrapolation from standard sizes.
$! If point sizes matching the standard fonts are specified, the fonts
$! produced exactly match the fonts produced using the parameter files
$! described in Computers and Typesetting, volume E.
$!
$! In the lines below, point sizes of 5, 6, 7, 8, 9, 10, 11, 12, 17.28,
$! 20 and 100 are suggested for most font families.  Thus, if you want
$! one of these point sizes and it is not a standard font, just uncomment
$! the line.  Also, these lines serve as examples for other sizes.
$! Note that very small sizes will probably not be readable on printers
$! limited to 300 dots per inch, such as the LN03.  Also, very large
$! fonts may exceed the font storage capacity of some printers.
$!
$!@build_var_font cmbsy5 cmbsy 5
$!@build_var_font cmbsy6 cmbsy 6
$!@build_var_font cmbsy7 cmbsy 7
$!@build_var_font cmbsy8 cmbsy 8
$!@build_var_font cmbsy9 cmbsy 9
$@build_var_font cmbsy10 cmbsy 10
$!@build_var_font cmbsy11 cmbsy 11
$!@build_var_font cmbsy12 cmbsy 12
$!@build_var_font cmbsy17 cmbsy 17.28
$!@build_var_font cmbsy20 cmbsy 20
$!@build_var_font cmbsy100 cmbsy 100
$!@build_var_font cmbxsl5 cmbxsl 5
$!@build_var_font cmbxsl6 cmbxsl 6
$!@build_var_font cmbxsl7 cmbxsl 7
$!@build_var_font cmbxsl8 cmbxsl 8
$!@build_var_font cmbxsl9 cmbxsl 9
$@build_var_font cmbxsl10 cmbxsl 10
$!@build_var_font cmbxsl11 cmbxsl 11
$!@build_var_font cmbxsl12 cmbxsl 12
$!@build_var_font cmbxsl17 cmbxsl 17.28
$!@build_var_font cmbxsl20 cmbxsl 20
$!@build_var_font cmbxsl100 cmbxsl 100
$!@build_var_font cmbxti5 cmbxti 5
$!@build_var_font cmbxti6 cmbxti 6
$!@build_var_font cmbxti7 cmbxti 7
$!@build_var_font cmbxti8 cmbxti 8
$!@build_var_font cmbxti9 cmbxti 9
$@build_var_font cmbxti10 cmbxti 10
$!@build_var_font cmbxti11 cmbxti 11
$!@build_var_font cmbxti12 cmbxti 12
$!@build_var_font cmbxti17 cmbxti 17.28
$!@build_var_font cmbxti20 cmbxti 20
$!@build_var_font cmbxti100 cmbxti 100
$@build_var_font cmbx5 cmbx 5
$@build_var_font cmbx6 cmbx 6
$@build_var_font cmbx7 cmbx 7
$@build_var_font cmbx8 cmbx 8
$@build_var_font cmbx9 cmbx 9
$@build_var_font cmbx10 cmbx 10
$!@build_var_font cmbx11 cmbx 11
$@build_var_font cmbx12 cmbx 12
$!@build_var_font cmbx17 cmbx 17.28
$!@build_var_font cmbx20 cmbx 20
$!@build_var_font cmbx100 cmbx 100
$!@build_var_font cmb5 cmb 5
$!@build_var_font cmb6 cmb 6
$!@build_var_font cmb7 cmb 7
$!@build_var_font cmb8 cmb 8
$!@build_var_font cmb9 cmb 9
$@build_var_font cmb10 cmb 10
$!@build_var_font cmb11 cmb 11
$!@build_var_font cmb12 cmb 12
$!@build_var_font cmb17 cmb 17.28
$!@build_var_font cmb20 cmb 20
$!@build_var_font cmb100 cmb 100
$!@build_var_font cmcsc5 cmcsc 5
$!@build_var_font cmcsc6 cmcsc 6
$!@build_var_font cmcsc7 cmcsc 7
$!@build_var_font cmcsc8 cmcsc 8
$!@build_var_font cmcsc9 cmcsc 9
$@build_var_font cmcsc10 cmcsc 10
$!@build_var_font cmcsc11 cmcsc 11
$!@build_var_font cmcsc12 cmcsc 12
$!@build_var_font cmcsc17 cmcsc 17.28
$!@build_var_font cmcsc20 cmcsc 20
$!@build_var_font cmcsc100 cmcsc 100
$!@build_var_font cmdunh5 cmdunh 5
$!@build_var_font cmdunh6 cmdunh 6
$!@build_var_font cmdunh7 cmdunh 7
$!@build_var_font cmdunh8 cmdunh 8
$!@build_var_font cmdunh9 cmdunh 9
$@build_var_font cmdunh10 cmdunh 10
$!@build_var_font cmdunh11 cmdunh 11
$!@build_var_font cmdunh12 cmdunh 12
$!@build_var_font cmdunh17 cmdunh 17.28
$!@build_var_font cmdunh20 cmdunh 20
$!@build_var_font cmdunh100 cmdunh 100
$!@build_var_font cmex5 cmex 5
$!@build_var_font cmex6 cmex 6
$!@build_var_font cmex7 cmex 7
$!@build_var_font cmex8 cmex 8
$!@build_var_font cmex9 cmex 9
$@build_var_font cmex10 cmex 10
$!@build_var_font cmex11 cmex 11
$!@build_var_font cmex12 cmex 12
$!@build_var_font cmex17 cmex 17.28
$!@build_var_font cmex20 cmex 20
$!@build_var_font cmex100 cmex 100
$!@build_var_font cmff5 cmff 5
$!@build_var_font cmff6 cmff 6
$!@build_var_font cmff7 cmff 7
$!@build_var_font cmff8 cmff 8
$!@build_var_font cmff9 cmff 9
$@build_var_font cmff10 cmff 10
$!@build_var_font cmff11 cmff 11
$!@build_var_font cmff12 cmff 12
$!@build_var_font cmff17 cmff 17.28
$!@build_var_font cmff20 cmff 20
$!@build_var_font cmff100 cmff 100
$!@build_var_font cmfib5 cmfib 5
$!@build_var_font cmfib6 cmfib 6
$!@build_var_font cmfib7 cmfib 7
$@build_var_font cmfib8 cmfib 8
$!@build_var_font cmfib9 cmfib 9
$!@build_var_font cmfib10 cmfib 10
$!@build_var_font cmfib11 cmfib 11
$!@build_var_font cmfib12 cmfib 12
$!@build_var_font cmfib17 cmfib 17.28
$!@build_var_font cmfib20 cmfib 20
$!@build_var_font cmfib100 cmfib 100
$!@build_var_font cmfi5 cmfi 5
$!@build_var_font cmfi6 cmfi 6
$!@build_var_font cmfi7 cmfi 7
$!@build_var_font cmfi8 cmfi 8
$!@build_var_font cmfi9 cmfi 9
$@build_var_font cmfi10 cmfi 10
$!@build_var_font cmfi11 cmfi 11
$!@build_var_font cmfi12 cmfi 12
$!@build_var_font cmfi17 cmfi 17.28
$!@build_var_font cmfi20 cmfi 20
$!@build_var_font cmfi100 cmfi 100
$@build_var_font cminch cminch 104.0687561
$!@build_var_font cmitt5 cmitt 5
$!@build_var_font cmitt6 cmitt 6
$!@build_var_font cmitt7 cmitt 7
$!@build_var_font cmitt8 cmitt 8
$!@build_var_font cmitt9 cmitt 9
$@build_var_font cmitt10 cmitt 10
$!@build_var_font cmitt11 cmitt 11
$!@build_var_font cmitt12 cmitt 12
$!@build_var_font cmitt17 cmitt 17.28
$!@build_var_font cmitt20 cmitt 20
$!@build_var_font cmitt100 cmitt 100
$!@build_var_font cmmib5 cmmib 5
$!@build_var_font cmmib6 cmmib 6
$!@build_var_font cmmib7 cmmib 7
$!@build_var_font cmmib8 cmmib 8
$!@build_var_font cmmib9 cmmib 9
$@build_var_font cmmib10 cmmib 10
$!@build_var_font cmmib11 cmmib 11
$!@build_var_font cmmib12 cmmib 12
$!@build_var_font cmmib17 cmmib 17.28
$!@build_var_font cmmib20 cmmib 20
$!@build_var_font cmmib100 cmmib 100
$@build_var_font cmmi5 cmmi 5
$@build_var_font cmmi6 cmmi 6
$@build_var_font cmmi7 cmmi 7
$@build_var_font cmmi8 cmmi 8
$@build_var_font cmmi9 cmmi 9
$@build_var_font cmmi10 cmmi 10
$!@build_var_font cmmi11 cmmi 11
$@build_var_font cmmi12 cmmi 12
$!@build_var_font cmmi17 cmmi 17.28
$!@build_var_font cmmi20 cmmi 20
$!@build_var_font cmmi100 cmmi 100
$@build_var_font cmr5 cmr 5
$@build_var_font cmr6 cmr 6
$@build_var_font cmr7 cmr 7
$@build_var_font cmr8 cmr 8
$@build_var_font cmr9 cmr 9
$@build_var_font cmr10 cmr 10
$!@build_var_font cmr11 cmr 11
$@build_var_font cmr12 cmr 12
$@build_var_font cmr17 cmr 17.28
$!@build_var_font cmr20 cmr 20
$!@build_var_font cmr100 cmr 100
$!@build_var_font cmsltt5 cmsltt 5
$!@build_var_font cmsltt6 cmsltt 6
$!@build_var_font cmsltt7 cmsltt 7
$!@build_var_font cmsltt8 cmsltt 8
$!@build_var_font cmsltt9 cmsltt 9
$@build_var_font cmsltt10 cmsltt 10
$!@build_var_font cmsltt11 cmsltt 11
$!@build_var_font cmsltt12 cmsltt 12
$!@build_var_font cmsltt17 cmsltt 17.28
$!@build_var_font cmsltt20 cmsltt 20
$!@build_var_font cmsltt100 cmsltt 100
$!@build_var_font cmsl5 cmsl 5
$!@build_var_font cmsl6 cmsl 6
$!@build_var_font cmsl7 cmsl 7
$@build_var_font cmsl8 cmsl 8
$@build_var_font cmsl9 cmsl 9
$@build_var_font cmsl10 cmsl 10
$!@build_var_font cmsl11 cmsl 11
$@build_var_font cmsl12 cmsl 12
$!@build_var_font cmsl17 cmsl 17.28
$!@build_var_font cmsl20 cmsl 20
$!@build_var_font cmsl100 cmsl 100
$!@build_var_font cmssbx5 cmssbx 5
$!@build_var_font cmssbx6 cmssbx 6
$!@build_var_font cmssbx7 cmssbx 7
$!@build_var_font cmssbx8 cmssbx 8
$!@build_var_font cmssbx9 cmssbx 9
$@build_var_font cmssbx10 cmssbx 10
$!@build_var_font cmssbx11 cmssbx 11
$!@build_var_font cmssbx12 cmssbx 12
$!@build_var_font cmssbx17 cmssbx 17.28
$!@build_var_font cmssbx20 cmssbx 20
$!@build_var_font cmssbx100 cmssbx 100
$!@build_var_font cmssdc5 cmssdc 5
$!@build_var_font cmssdc6 cmssdc 6
$!@build_var_font cmssdc7 cmssdc 7
$!@build_var_font cmssdc8 cmssdc 8
$!@build_var_font cmssdc9 cmssdc 9
$@build_var_font cmssdc10 cmssdc 10
$!@build_var_font cmssdc11 cmssdc 11
$!@build_var_font cmssdc12 cmssdc 12
$!@build_var_font cmssdc17 cmssdc 17.28
$!@build_var_font cmssdc20 cmssdc 20
$!@build_var_font cmssdc100 cmssdc 100
$!@build_var_font cmssi5 cmssi 5
$!@build_var_font cmssi6 cmssi 6
$!@build_var_font cmssi7 cmssi 7
$@build_var_font cmssi8 cmssi 8
$@build_var_font cmssi9 cmssi 9
$@build_var_font cmssi10 cmssi 10
$!@build_var_font cmssi11 cmssi 11
$@build_var_font cmssi12 cmssi 12
$@build_var_font cmssi17 cmssi 17.28
$!@build_var_font cmssi20 cmssi 20
$!@build_var_font cmssi100 cmssi 100
$!@build_var_font cmssqi5 cmssqi 5
$!@build_var_font cmssqi6 cmssqi 6
$!@build_var_font cmssqi7 cmssqi 7
$@build_var_font cmssqi8 cmssqi 8
$!@build_var_font cmssqi9 cmssqi 9
$!@build_var_font cmssqi10 cmssqi 10
$!@build_var_font cmssqi11 cmssqi 11
$!@build_var_font cmssqi12 cmssqi 12
$!@build_var_font cmssqi17 cmssqi 17.28
$!@build_var_font cmssqi20 cmssqi 20
$!@build_var_font cmssqi100 cmssqi 100
$!@build_var_font cmssq5 cmssq 5
$!@build_var_font cmssq6 cmssq 6
$!@build_var_font cmssq7 cmssq 7
$@build_var_font cmssq8 cmssq 8
$!@build_var_font cmssq9 cmssq 9
$!@build_var_font cmssq10 cmssq 10
$!@build_var_font cmssq11 cmssq 11
$!@build_var_font cmssq12 cmssq 12
$!@build_var_font cmssq17 cmssq 17.28
$!@build_var_font cmssq20 cmssq 20
$!@build_var_font cmssq100 cmssq 100
$!@build_var_font cmss5 cmss 5
$!@build_var_font cmss6 cmss 6
$!@build_var_font cmss7 cmss 7
$@build_var_font cmss8 cmss 8
$@build_var_font cmss9 cmss 9
$@build_var_font cmss10 cmss 10
$!@build_var_font cmss11 cmss 11
$@build_var_font cmss12 cmss 12
$@build_var_font cmss17 cmss 17.28
$!@build_var_font cmss20 cmss 20
$!@build_var_font cmss100 cmss 100
$@build_var_font cmsy5 cmsy 5
$@build_var_font cmsy6 cmsy 6
$@build_var_font cmsy7 cmsy 7
$@build_var_font cmsy8 cmsy 8
$@build_var_font cmsy9 cmsy 9
$@build_var_font cmsy10 cmsy 10
$!@build_var_font cmsy11 cmsy 11
$!@build_var_font cmsy12 cmsy 12
$!@build_var_font cmsy17 cmsy 17.28
$!@build_var_font cmsy20 cmsy 20
$!@build_var_font cmsy100 cmsy 100
$!@build_var_font cmtcsc5 cmtcsc 5
$!@build_var_font cmtcsc6 cmtcsc 6
$!@build_var_font cmtcsc7 cmtcsc 7
$!@build_var_font cmtcsc8 cmtcsc 8
$!@build_var_font cmtcsc9 cmtcsc 9
$@build_var_font cmtcsc10 cmtcsc 10
$!@build_var_font cmtcsc11 cmtcsc 11
$!@build_var_font cmtcsc12 cmtcsc 12
$!@build_var_font cmtcsc17 cmtcsc 17.28
$!@build_var_font cmtcsc20 cmtcsc 20
$!@build_var_font cmtcsc100 cmtcsc 100
$!@build_var_font cmtex5 cmtex 5
$!@build_var_font cmtex6 cmtex 6
$!@build_var_font cmtex7 cmtex 7
$@build_var_font cmtex8 cmtex 8
$@build_var_font cmtex9 cmtex 9
$@build_var_font cmtex10 cmtex 10
$!@build_var_font cmtex11 cmtex 11
$!@build_var_font cmtex12 cmtex 12
$!@build_var_font cmtex17 cmtex 17.28
$!@build_var_font cmtex20 cmtex 20
$!@build_var_font cmtex100 cmtex 100
$!@build_var_font cmti5 cmti 5
$!@build_var_font cmti6 cmti 6
$@build_var_font cmti7 cmti 7
$@build_var_font cmti8 cmti 8
$@build_var_font cmti9 cmti 9
$@build_var_font cmti10 cmti 10
$!@build_var_font cmti11 cmti 11
$@build_var_font cmti12 cmti 12
$!@build_var_font cmti17 cmti 17.28
$!@build_var_font cmti20 cmti 20
$!@build_var_font cmti100 cmti 100
$!@build_var_font cmtt5 cmtt 5
$!@build_var_font cmtt6 cmtt 6
$!@build_var_font cmtt7 cmtt 7
$@build_var_font cmtt8 cmtt 8
$@build_var_font cmtt9 cmtt 9
$@build_var_font cmtt10 cmtt 10
$!@build_var_font cmtt11 cmtt 11
$@build_var_font cmtt12 cmtt 12
$!@build_var_font cmtt17 cmtt 17.28
$!@build_var_font cmtt20 cmtt 20
$!@build_var_font cmtt100 cmtt 100
$!@build_var_font cmu5 cmu 5
$!@build_var_font cmu6 cmu 6
$!@build_var_font cmu7 cmu 7
$!@build_var_font cmu8 cmu 8
$!@build_var_font cmu9 cmu 9
$@build_var_font cmu10 cmu 10
$!@build_var_font cmu11 cmu 11
$!@build_var_font cmu12 cmu 12
$!@build_var_font cmu17 cmu 17.28
$!@build_var_font cmu20 cmu 20
$!@build_var_font cmu100 cmu 100
$!@build_var_font cmvtt5 cmvtt 5
$!@build_var_font cmvtt6 cmvtt 6
$!@build_var_font cmvtt7 cmvtt 7
$!@build_var_font cmvtt8 cmvtt 8
$!@build_var_font cmvtt9 cmvtt 9
$@build_var_font cmvtt10 cmvtt 10
$!@build_var_font cmvtt11 cmvtt 11
$!@build_var_font cmvtt12 cmvtt 12
$!@build_var_font cmvtt17 cmvtt 17.28
$!@build_var_font cmvtt20 cmvtt 20
$!@build_var_font cmvtt100 cmvtt 100

