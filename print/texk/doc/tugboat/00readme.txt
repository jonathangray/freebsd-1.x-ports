
Directory DISK$TEX:[TEX-ARCHIVE.DIGESTS.TUGBOAT]

(Files new or revised as of 30th August are marked with an asterisk in the
first column of each line.)

This directory was revised by BHK 30-AUG-1991 to incorporate the latest
versions of  the style files for use by authors submitting articles to TUGboat. 
The latest  versions of these style files now have a name incorporating TUGBOAT
rather  than the TUGBOT used previously.  The latter files are obsolete, but
retained  for use in formatting old articles.

This file contains descriptions of files in the TeX distribution which are
made available by or through the TeX Users Group, and comments should be
directed there rather than to the Stanford TeX group; see address below.

Please note that some of the files described here are also included in a
distribution of TeX fonts and macros from the American Mathematical Society;
see also the file -READ-.AMS.


General documentation

*00FILES.TXT      Latest state of play
*00README.TUG     Renamed copy of -read-.tug from Barbara Beeton
 TUGFIL.CHG	 Change log for TUGboat files
*00README.TXT     This file

APL font and user information

 APLDEF.TeX	Macros which implement the symbols in the APL font
 APLSTYLE.TeX	Macros needed to format APL text in the article
 APLVERB.TeX	Macros needed to represent input in the APL article
 CMAPL10.MF, .TFM, .300GF, .PK	The APL font files
 TB0APL.TeX	TUGboat article concerning the APL font

	The APL font, CMAPL10, was developed by Aarno Hohti and Okko Kanerva
	of the University of Helsinki, and reported in TUGboat 8#3, pp 275 ff.


Hyphenation patterns

*GERMANHYPH.TeX	German patterns, from Ruhr Universitaet Bochum
*ICEHYPH.TeX	Icelandic patterns, from Jorgen Pind; note that use requires
		special versions of TeX and Plain
 PORTHYPH.TeX	Portuguese patterns, from Pedro de Rezende


Macros based on LaTeX

 MERGE.STY	A form letter option to the letter style


Macros based on Plain TeX

*TREEDEF.TeX	tree definitions by David Eppstein
*TB0TREE.TeX	David Eppstein, "Trees in TeX", TUGboat 6#1, March 1985


TUGboat 	These files are also available in the distribution from AMS

 TUBGUIDE.TEX    Guide for authors

*LTUGBOAT.STY    Newest version of TUGboat macros, for authors using LaTeX
 LTUGBOT.STY	A (proto-) TUGboat option for use with the article style
                --- (Do not use) obsolete
*TUGBOAT.COM     Macros common to TUGBOAT.STY and LTUGBOAT.STY
*TUGBOAT.STY     Newest version of TUGboat macros, for authors using Plain
 TUGBOT.TeX	File which reads .STY file and data files for TUGboat articles
                --- (Do not use) obsolete
 TUGBOT.STY	Style file, containing all necessary definitions for TUGboat
                --- (Do not use) obsolete
 TB0HYF.TeX	Sample TUGboat article -- Hyphenation exceptions
 TB25HYF.TEX    Hyphenations exceptions list: TUGboat vol 10 # 4
 TB0CYR.TeX	Sample TUGboat article -- Math symbols and cyrillic fonts;
		note that use requires the fonts MCYR10, MSXM10 and MSYM10,
		and the file CYRACC.DEF (located with AMSTeX.TeX)


*GUIDEPRO.TEX		1990 TUG Proceedings guide

TUGboat tables of contents

*TBCVnn.TeX	File which reads .DEF file and data files, in 5-volume groups
		with nn = last volume of group
*TBCONT.DEF	Definitions to set tables of contents
*TBnnyy.CNT	TUGboat tables of contents, volume nn, year yy


The End.

	-Barbara Beeton, September 1988

	Computer Services Division
	American Mathematical Society
	P.O. Box 6248
	Providence, RI 02940

	Phone:  401-272-9500
	Net:	BNB@Math.AMS.Com

Files for generating indices of TUGboat (from Nelson Beebe)

*KWIC-BIB.AWK		AWK program to turn .bib file into .kwic file
*KWIC.DVI		175-page keyword-in-context listing for TUGboat
*KWIC.LTX		LaTeX base file to produce kwic.dvi
*LANDSCAPE.STY		Style file for landscape page sizes
*MAKEFILE		UNIX Makefile to build various targets
*MAKEFILE.D20		DEC-20 makefile
*MAKEFILE.UNX		UNIX makefile
*TUGBIB.BBL		BibTeX output for tugbib.ltx
*TUGBIB.DVI		LaTeX output for tugbib.ltx
*TUGBIB.LTX		LaTeX base file for alphabetic TUGboat bibliography
*TUGBIB2.BBL		BibTeX output for tugbib2.ltx
*TUGBIB2.DVI		LaTeX output for tugbib2.ltx
*TUGBIB2.LTX		LaTeX base file for time-ordered TUGboat bibliography
*TUGBOAT.AWK		AWK program for converting *.cnt files to tugboat.bib
*TUGBOAT.BIB		TUGboat BibTeX bibliography
*TUGBOAT.DEF		Macros used in tugboat.bib article entries.
*TUGBOAT.KWIC		TUGboat kwic index listings.
*TUGBOAT.PTX		TUGboat permuted index listings.
