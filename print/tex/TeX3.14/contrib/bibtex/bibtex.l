.TH BIBTEX 1 
.SH NAME
bibtex \- make a LaTeX bibliography
.SH SYNOPSIS
.B bibtex auxname
.SH DESCRIPTION
BibTeX reads the top-level auxiliary
.I (.aux)
file that was output during the running of
.BR latex (1)
and creates a bibliography
.I (.bbl)
file that will be incorporated into the document on subsequent runs of LaTeX.
The auxname on the command line must be given without the
.I .aux
extension.
.PP
BibTeX looks up, in the bibliographic database
.I (.bib)
files,
the entries specified by the \\cite and \\nocite commands
in the LaTeX source file.
It formats the information from those entries
according to instructions in the style
.I (.bst)
file,
and it outputs the results to the
.I .bbl
file.
.PP
The LaTeX manual (aka
.I LaTeX - A Document Preparation System,
by Leslie Lamport)
explains what a LaTeX source file must contain to work with BibTeX.
Appendix B of the LaTeX manual describes the format of the
.I .bib
files.
The "BibTeXing" document describes extensions and details of this format,
and it gives other useful hints for using BibTeX.

BibTeX on Unix incorporates the path-searching mechanism
described in the man page for
.BR tex (1).
As of the August 1988 web2c version,
BibTeX searches the TEXINPUTS path
(if it's defined, else a system-dependent default) for
.I .bst
files, BIBINPUTS for
.I .bib
files, and no path for
.I .aux
files.
.PP
.SH FILES
.TP 20
btxdoc.tex
"BibTeXing" \- LaTeXable documentation for general BibTeX users
.TP
btxhak.tex
"Designing BibTeX Styles" \- LaTeXable documentation for style designers
.TP
btxdoc.bib
database file for those two documents
.TP
xampl.bib
database file giving examples of all standard entry types
.TP
btxbst.doc
template file and documentation for the standard styles
.PP
All these files should be available
in the same directory somewhere on the system.
.SH "SEE ALSO"
.BR latex (1),
.BR tex (1).
