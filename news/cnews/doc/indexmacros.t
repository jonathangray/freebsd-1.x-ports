. \" Macros for the index
.de Ib	\" blank major entry
.br
.ne 2v
\\$1:
..
.de I>	\" major entry
.br
\\$1, \\$2
..
.de I<  \" minor entry
.br
   \\$2, \\$3
..
.de Lb	\" new letter starts here
.di Dt	\" start diverted text
.sp
.ps +2
.B \\$1
.ps -2
.sp
.di	\" end diverted text
.ne \\n(dnu+1v	\" get enough space for it
.Dt		\" output it
..
.\" set up various parameters for the right evironment.
.\" Your taste may be different.
.\" Set the page number for the index properly.
.LP
.DS C
.LG
.B Index
.DE
.LP
.2C
.DS L
.ta 5iR		\" right alignment tab
