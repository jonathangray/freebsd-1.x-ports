There are a lot of files on this tape, and you may not have enough disk
space to keep all of them on-line all the time.  To help you choose
which files may be useful to you I have prepared this brief guide to
the tape's contents.

BUILD_ALL.COM, BUILD_FONT.COM and BUILD_FONT_MAG.COM are VMS command
files that build the Computer Modern font files in standard point sizes
using the standard parameter files.  BUILD_ALL calls BUILD_FONT which
calls BUILD_FONT_MAG.  Running BUILD_ALL takes about 2 1/2 days of
CPU time on a VAX-11/785.

BUILD_ALL_CM_FONTS.COM, BUILD_VAR_FONT.COM and BUILD_VAR_FONT_MAG.COM
are VMS command files that build the Computer Modern font files in
selectable point sizes using alternative parameter files.  If you
select standard point sizes you get exactly the same results as with
BUILD_ALL. BUILD_ALL_CM_FONTS contains the standard point sizes, with
some suggested additional sizes commented out. 

The 31 BUILD_*.MF files are the alternative parameter files for building
the Computer Modern fonts using selectable point sizes.

All but one of the remaining .MF files are used by the BUILD_*.MF
files as subroutines.  These are: BOLD_TO_MATH.MF, COMPUTE_CMBX.MF,
COMPUTE_CMFF.MF, COMPUTE_CMMI.MF, COMPUTE_CMR.MF, COMPUTE_CMSS.MF,
COMPUTE_CMSSBX.MF, COMPUTE_CMSSQ.MF, COMPUTE_CMSY.MF, COMPUTE_CMTI.MF,
COMPUTE_CMTT.MF and COMPUTE_SIGMA.MF. 

The remaining .MF file, SAUTER.MF, is used as a prefix file in all
Metafont runs.  You should put your devices in it as additional modes.
The file on the tape contains the DEC LN03 as an example. 

The *.*PXL files are pixel files produced using the above procedures.
The device-dependent Metafont parameters have been set for the DEC LN03.
These files are suitable for input to DVI2LN3.

The *.*LIS files are the log files from Metafont.

The *.TFM files are the TeX Font Metric files.  They are identical to
the files distributed with TeX version 2 and Metafont version 1.  These
files are device-independent.

README.tape contains this text.

SETUP.COM is a VMS command file that is executed at the beginning of all
jobs.  You should put in it any definitions or other commands that you
may need to run Metafont.

John Sauter
sauter%dssdev.dec@decwrl.dec.com
