% This is twist.ch
This is a changefile for WEAVE which produces TWIST, a version of WEAVE
which allows multiple changefiles (see TUGBoat Vol.7, No. 1, pg. 20-21).
@x We modify the text
@* Introduction.
This program converts a \.{WEB} file to a \TeX\ file. It was written
by D. E. Knuth in October, 1981; a somewhat similar {\mc SAIL} program had
been developed in March, 1979, although the earlier program used a top-down
parsing method that is quite different from the present scheme.
@y
\def\title{TWIST}
\def\contentspagenumber{1} % should be odd
\def\topofcontents{\null\vfill
  \titlefalse % include headline on the contents page
  \def\rheader{\mainfont Appendix E\hfil \contentspagenumber}
  \centerline{\titlefont The {\ttitlefont TWIST} processor}
  \vskip 15pt
  \centerline{(Version 2.7)}
  \vfill}
\pageno=\contentspagenumber \advance\pageno by 1
@* Introduction.
This program converts a \.{WEB} file to a \TeX\ file.  Several
changes have been made by GMD, to allow \.{WEAVE} to handle more
than one |change_file|. It is basically the original \.{WEAVE} written by
D.E. Knuth, but we'll name this \.{WEAVE}--Version \.{TWIST} to distinguish it
from the original one. The program was created by writing a |change_file|
modifying |weave.web|. To keep that |change_file| as small as possible we
didn't change the name "\.{WEAVE}" througout the documentation.
\bigskip
If you find a bug in the program you should send a message to:

{\obeylines\parskip=0pt
      W.Appelt, K.Horn
      GMD Gesellschaft f\"ur Mathematik und Datenverarbeitung
      Schlo\ss\ Birlinghoven
      Postfach 1240
      D-5205 Sankt Augustin

      West-Germany

}
\bigskip
The original version was written
by D. E. Knuth in October, 1981; a somewhat similar {\mc SAIL} program had
been developed in March, 1979, although the earlier program used a top-down
parsing method that is quite different from the present scheme.
@z

@x (1) we modify the banner line
The ``banner line'' defined here should be changed whenever \.{WEAVE}
is modified.

@d banner=='This is WEAVE, Version 2.8'
@y
The ``banner line'' defined here should be changed whenever \.{TWIST}
is modified.
@d banner=='This is TWIST based on WEAVE, Version 2.8 knitted by GMD'
@d term_in==input
@d term_out==output
@z

@x (2) Now we allow three change-files
@ The program begins with a fairly normal header, made up of pieces that
@^system dependencies@>
will mostly be filled in later. The \.{WEB} input comes from files |web_file|
and |change_file|, and the \TeX\ output goes to file |tex_file|.
@y
@ Changes have been made to allow more than one |change_file|.
To mark changed modules we won't use the asteriks as \.{WEAVE} does, but
we'll indicate the number or numbers of the corresponding
|change_files| influencing the actual module.
In this \.{TWIST}--version three |change_files| are used. The necessary new
definitions are inserted quite below this explanation, so don't be
astonished to find a macro-definition in this module.
Changing the number of |change-files| only causes updates in this
module.

The |change_files| have descending priorities. That means, if two
|change_files| want to modify the same line in a |web_file| , there will be a
warning and  the |change_file| with the lower priority will be advanced
to the next section without influencing the input.
If a |change_file| wants to modify lines within the |web_file| which
are currently already changed by another |change_file| then changes
are ignored.
A warning will be generated.

First you should examine this little example of \.{TWIST} with three
|change_files|:

\bigskip
Input files:
$$\vbox{{\tt
\halign{&
\quad#\hfill\quad&\quad#\hfill\quad&\quad#\hfill\quad&\quad#\hfill\quad\cr
|web_file|  &   |ch_file_1|  &  |ch_file_2|  &  |ch_file_3|     \cr
line1       &   @@x          &  @@x          &  @@x             \cr
line2       &   line1        &  line5        &  line1           \cr
line3       &   @@y          &  @@y          &  line2           \cr
line4       &   line1from1   &  line5from2   &  @@y             \cr
line5       &   line2from1   &  @@z          &  line1from3      \cr
line6       &   line3from1   &  @@x          &  @@z             \cr
line7       &   @@z          &  line10       &  @@x             \cr
line8       &   *EOF*        &  @@y          &  line4           \cr
line9       &                &  line10from2  &  line5           \cr
line10      &                &  line11from2  &  line6           \cr
line11      &                &  @@z          &  @@y             \cr
line12      &                &  *EOF*        &  line4from3      \cr
*EOF*       &                &               &  line5from3      \cr
	    &                &               &  @@z             \cr
	    &                &               &  *EOF*           \cr
}}
}
$$

The result will be that the program will input the following lines:

{\tt\obeylines
***WARNING...     (ch\_file\_3 will be advanced)
line1from1
line2from1
line3from1
line2
line3
***WARNING...     (ch\_file\_2 will be advanced)
line4from3
line5from3
line7
line8
line9
line10from2
line11from2
line11
line12
*EOF*
}

Now we want to explain the macros defined in this module. They serve to
avoid arrays of files since some PASCAL-compilers can't
handle arrays of files. Behind this we find the main-program's skeleton.

The program begins with a fairly normal header, made up of pieces that
@^system dependencies@>
will mostly be filled in later. The \.{WEB} input comes
from the file |web_file| and from the files |change_files|, and the
\TeX\ output goes to file |tex_file|.
@z

@x we have to split the change-file to allow system dependent changes
@d end_of_WEAVE = 9999 {go here to wrap it up}
@y
@d end_of_WEAVE = 9999 {go here to wrap it up}

@d ch_max==3  {indicates the maximum number of |change_files| with descending
priority, if this number is modified the following three definitions and
the program statement must be updated as well, further don't forget to
initialize the array prime[1..ch\_max] properly}

@d input_ch(#) == case # of
	      1: ch_not_eof:=input_ln(ch_file_1);
	      2: ch_not_eof:=input_ln(ch_file_2);
	      3: ch_not_eof:=input_ln(ch_file_3);end

@d reset_ch(#) == case # of
	      1: reset(ch_file_1);
	      2: reset(ch_file_2);
	      3: reset(ch_file_3);end

@d declaration_ch_files ==@/
		    @! ch_file_1:text_file;
		    @! ch_file_2:text_file;
		    @! ch_file_3:text_file

@d decl_prime == @! prime:array[1..ch_max] of integer
{augmenting the number of |change_files| don't forget to add the
corresponding prime number in the initialisation above}

@z

@x (2) program header,term_in and term_out (this shouldn't make worry)
program WEAVE(@!web_file,@!change_file,@!tex_file);
@y
program TWIST(@!term_in,@!term_out,@!web_file,@!ch_file_1,@!ch_file_2,
	       @!ch_file_3,@!tex_file);
@z

@x We define the prime numbers
procedure initialize;
  var @<Local variables for initialization@>@/
  begin @<Set initial values@>@/
  end;
@y
procedure initialize;
  var @<Local variables for initialization@>@/
  begin @<Set initial values@>
prime[1]:=2;
prime[2]:=3;
prime[3]:=5;   {augmenting the number of |change__ files| add a prime-number}
end;
@z

@x Default in case statement is otherwise. (7)
@d othercases == others: {default for cases not listed explicitly}
@y
@d othercases == otherwise {default for cases not listed explicitly}
@z
@x term_out (output) must not be redefined. (20)
@!term_out:text_file; {the terminal as an output file}
@y
@z

@x term_out (output) must not be opened. (21)
rewrite(term_out,'TTY:'); {send |term_out| output to the terminal}
@y
@z

@x break is replaced by writeln (22)
@d update_terminal == break(term_out) {empty the terminal output buffer}
@y
@d update_terminal == write_ln(term_out) {empty the terminal output buffer}
@z

@x (23) we declare the maximum number (ch_max) of change-files
@ The main input comes from |web_file|; this input may be overridden
by changes in |change_file|. (If |change_file| is empty, there are no changes.)

@<Globals...@>=
@!web_file:text_file; {primary input}
@!change_file:text_file; {updates}
@y
@ The main input comes from |web_file|; this input may be overridden
by changes in the |change_files|. The |change_files| have descending
priority. Only one change may be done to the same text in the |web_file|.
(If all |change_files| are empty, there are no changes.)
Whenever the number of |change_files| is increased the correspondent
file-declarations have to be modified. Since we have written a macro
for this in module~2 of \.{TWIST}, no more changes are necessary here.

@<Globals...@>=
@!web_file:text_file; {primary input}
declaration_ch_files; {look for the macro definitions at the beginning}
decl_prime; {look for the macro definition at the beginning}
@z

@x (24) we have to reset all change-files explicitely
@ The following code opens the input files.  Since these files were listed
in the program header, we assume that the \PASCAL\ runtime system has
already checked that suitable file names have been given; therefore no
additional error checking needs to be done. We will see below that
\.{WEAVE} reads through the entire input twice.
@^system dependencies@>

@p procedure open_input; {prepare to read |web_file| and |change_file|}
begin reset(web_file); reset(change_file);
end;
@y
@ The following code opens the input files.  Since these files were listed
in the program header, we assume that the \PASCAL\ runtime system has
already checked that suitable file names have been given; therefore no
additional error checking needs to be done. We will see below that
\.{WEAVE} reads through the entire input twice.
@^system dependencies@>
The macro |reset_ch(i)| resets the |i-th change_file|.
This is done by a macro,
so changing the number of |change_files| only makes necessary a modification
in the macro written in module~2 of the \.{TWIST}--program.

@p procedure open_input; {prepare to read |web_file| and |change_file|}
var i:integer; {loop variable for processing the |change_files|}
begin reset(web_file); for i:=1 to ch_max do reset_ch(i);end;
@z

@x (32) We want to specify the current change-file
@ The error locations can be indicated by using the global variables
|loc|, |line|, and |changing|, which tell respectively the first
unlooked-at position in |buffer|, the current line number, and whether or not
the current line is from |change_file| or |web_file|.
This routine should be modified on systems whose standard text editor
has special line-numbering conventions.
@^system dependencies@>

@<Print error location based on input buffer@>=
begin if changing then print('. (change file ')@+else print('. (');
print_ln('l.', line:1, ')');
if loc>=limit then l:=limit else l:=loc;
for k:=1 to l do
  if buffer[k-1]=tab_mark then print(' ')
  else print(xchr[buffer[k-1]]); {print the characters already read}
new_line;
for k:=1 to l do print(' '); {space out the next line}
for k:=l+1 to limit do print(xchr[buffer[k-1]]); {print the part not yet read}
if buffer[limit]="|" then print(xchr["|"]);
  {end of \PASCAL\ text in module names}
print(' '); {this space separates the message from future asterisks}
end
@y
@ The error locations can be indicated by using the global variables
|loc|, |line|, and |changing|, which tell respectively the first
unlooked-at position in |buffer|, the current line number, and whether or not
the current line is from a |change_file| (and if yes, from which one)
or from |web_file|. The variable |ch_act| gives the index of
the current |change_file|.
This routine should be modified on systems whose standard text editor
has special line-numbering conventions.
@^system dependencies@>

@<Print error location based on input buffer@>=
begin if changing then print('. (change file ',ch_act:1,' ')@+
else print('. (');
print_ln('l.', line:1, ')');
if loc>=limit then l:=limit else l:=loc;
for k:=1 to l do
  if buffer[k-1]=tab_mark then print(' ')
  else print(xchr[buffer[k-1]]); {print the characters already read}
new_line;
for k:=1 to l do print(' '); {space out the next line}
for k:=l+1 to limit do print(xchr[buffer[k-1]]); {print the part not yet read}
if buffer[limit]="|" then print(xchr["|"]);
  {end of \PASCAL\ text in module names}
print(' '); {this space separates the message from future asterisks}
end
@z

@x (45) we specify the actual change file
@ We keep track of the current module number in
|module_count|, which is the total number of modules that have started.
Modules which have been altered by a change file entry
have their |changed_module| flag turned on during the first phase.

@<Globals...@>=
@!module_count:0..max_modules; {the current module number}
@!changed_module: packed array [0..max_modules] of boolean; {is it changed?}
@!change_exists: boolean; {has any module changed?}
@y
@ We keep track of the current module number in
|module_count|, which is the total number of modules that have started.
Modules which have been altered by a |change_file|--entry
have their |changed_module| flag turned on during the first phase.
Since we have to work with more than one |change_file| we cannot
use a boolean flag. We have decided to associate a prime number
with each |change_file| and to multiply the flag's value with this
prime number if the module is changed by that |change_file|. The
initial value for the |changed_module|--flag is one.
A value different from one, indicates that a change has been made.

@<Globals...@>=
@!module_count:0..max_modules; {the current module number}
@!changed_module: packed array [0..max_modules] of integer;
{keeps a product of primes or 1}
@!change_exists: integer; {unequal 1 if a module has changed}
@z

@x (70) we define a type |ch_type| to characterise the local parameter
@* Lexical scanning.
Let us now consider the subroutines that read the \.{WEB} source file
and break it into meaningful units. There are four such procedures:
One simply skips to the next `\.{@@\ }' or `\.{@@*}' that begins a
module; another passes over the \TeX\ text at the beginning of a
module; the third passes over the \TeX\ text in a \PASCAL\ comment;
and the last, which is the most interesting, gets the next token of
a \PASCAL\ text.
@y
@* Lexical scanning.
Let us now consider the subroutines that read the \.{WEB} source file
and break it into meaningful units. There are four such procedures:
One simply skips to the next `\.{@@\ }' or `\.{@@*}' that begins a
module; another passes over the \TeX\ text at the beginning of a
module; the third passes over the \TeX\ text in a \PASCAL\ comment;
and the last, which is the most interesting, gets the next token of
a \PASCAL\ text.

In the \.{TWIST}--version of \.{WEAVE} there is more than one |change_file|.
So there are some modifications, which will be explained when they are made.
Altering the number of |change_files| only causes modifications in module~2
of \.{TWIST} since we decided to use macros whereever
the number of |change_files| is important.

We need a type |ch_type| to describe the local parameters. This will be done
here.

@<Types...@>=
@!ch_type=1..ch_max;
@z

@x (73) we expand |change_buffer|, |change_limit| and |ch_line| one dimension
@ When |changing| is |false|, the next line of |change_file| is kept in
|change_buffer[0..change_limit]|, for purposes of comparison with the next
line of |web_file|. After the change file has been completely input, we
set |change_limit:=0|, so that no further matches will be made.

@<Globals...@>=
@!change_buffer:array[0..buf_size] of ASCII_code;
@!change_limit:0..buf_size; {the last position occupied in |change_buffer|}
@y
@ Different to the original version we are able to handle more than
one |change_file|. So the variable |change_buffer| and
|change_limit| need one more dimension to indicate the current |change_file|.
The index of the current |change_file| will be kept in the variable |ch_act|.
It is set by the function |lines_dont_match|. The variable |ch_not_eof|
takes the value of the |input_ln| function when the input comes from a
|change_file|. To understand why we need this you should look up
the macro definition of |input_ch(#)| in module~2 of \.{TWIST}.
The current line numbers of the |web_file| and all the |change_files|
have to be managed. In the original version this could be done with the
macro |change_changing| defined in the previous module. \.{TWIST} has to handle
more than two files, so we use the array |ch_line| to keep the current line
numbers of all |change_files|. The variable |line| will be updated from
this array each time it is used.
When |changing| is |false|, the next lines of the
|change_files| are kept in their |change_buffers|.
The length of the |change_buffers| are kept in |change_limit[i]|.
After a |change_file| has been completely input, we
set its |change_limit:=0|, so that no further matches will be made.

@<Globals...@>=
@!change_buffer:array[1..ch_max,0..buf_size] of ASCII_code;
@!change_limit:array[1..ch_max] of 0..buf_size;
		      {the last position occupied in |change_buffer|}
@!ch_act:1..ch_max; {specifies the current |change_file|}
@!i:1..ch_max;{index used for loops}
@!ch_not_eof:boolean;{corresponds to the boolean |input_ln|}
@!ch_line:array[1..ch_max] of integer; {line refering to all |change_file|s}
@z

@x (74) we define an additional function ch_dont_match
@ Here's a simple function that checks if the two buffers are different.

@p function lines_dont_match:boolean;
label exit;
var k:0..buf_size; {index into the buffers}
begin lines_dont_match:=true;
if change_limit<>limit then return;
if limit>0 then
  for k:=0 to limit-1 do if change_buffer[k]<>buffer[k] then return;
lines_dont_match:=false;
exit: end;
@y
@ Differing from the original version the index of the current |change_buffer|
has to be specified as function parameter.
The function |lines_dont_match| checks if |change_buffer[i]| is equal to
the buffer filled by |web_file|. If so, the index of the |change_file|
is copied to the variable |ch_act|. The function |ch_dont_match| checks
if two |change_buffers| are equal.

@p function lines_dont_match(i:ch_type):boolean;
label exit;
var k:0..buf_size; {index into the buffers}
begin lines_dont_match:=true;
if change_limit[i]<>limit then return;
if limit>0 then
  for k:=0 to limit-1 do if change_buffer[i,k]<>buffer[k] then return;
ch_act:=i;
lines_dont_match:=false;
exit: end;
@t\hskip 2in@>

function ch_dont_match(i,j:ch_type):boolean;
label exit;
var k:0..buf_size; {index into the buffers}
begin ch_dont_match:=true;
if ((change_limit[i]=0) or (change_limit[j]=0)) then return;
if change_limit[i]<>change_limit[j] then return;
if change_limit[i]>0 then
   for k:=0 to change_limit[i]-1
       do if change_buffer[i,k]<>change_buffer[j,k] then return;
ch_dont_match:=false;
exit: end;
@z

@x (75) now we have to manage more than one change-file
@ Procedure |prime_the_change_buffer| sets |change_buffer| in preparation
for the next matching operation. Since blank lines in the change file are
not used for matching, we have |(change_limit=0)and not changing| if and
only if the change file is exhausted. This procedure is called only
when |changing| is true; hence error messages will be reported correctly.

@p procedure prime_the_change_buffer;
label continue, done, exit;
var k:0..buf_size; {index into the buffers}
begin change_limit:=0; {this value will be used if the change file ends}
@<Skip over comment lines in the change file; |return| if end of file@>;
@<Skip to the next nonblank line; |return| if end of file@>;
@<Move |buffer| and |limit| to |change_buffer| and |change_limit|@>;
exit: end;
@y
@ Procedure |prime_the_change_buffer| sets the indexed
|change_buffer| in preparation
for the next matching operation. Since blank lines in the change file are
not used for matching, we have |(change_limit=0)and not changing| if and
only if the change file is exhausted. This procedure is called only
when |changing| is true; hence error messages will be reported correctly.

The procedure |pre_prime_the_change_buffer| is necessary if there are two
equal |change_buffers|. The one with the lower priority or the one which
is currently not compared with the buffer filled by |web_file| will be advanced
to the next matching position behind the @@x-line. First we have to skip over
the @@x...@@y...@@z passage before we call the original
|prime_the_change_buffer|.

The procedure |compare_change_files| checks if the |change_buffers| of all
|change_files| are equal. If so, those one with the lower priority will be
advanced.

@p procedure prime_the_change_buffer(i:ch_type);
label continue, done, exit;
var k:0..buf_size; {index into the buffers}
begin change_limit[i]:=0; {this value will be used if the change file ends}
@<Skip over comment lines in the change file; |return| if end of file@>;
@<Skip to the next nonblank line; |return| if end of file@>;
@<Move |buffer| and |limit| to |change_buffer| and |change_limit|@>;
exit: end;
@t\hskip 2in@>

procedure pre_prime_the_change_buffer(i:ch_type);
label continue, done, exit;
begin change_limit[i]:=0; {this value will be used if the change file ends}
loop@+ begin line:=ch_line[i];incr(line);ch_line[i]:=line;
   input_ch(i);
   if not ch_not_eof then
      begin err_print ('! Change file ',i:1,' ended without @@z');
      return end;
   if limit < 2 then goto continue;
   if ((buffer[0]="@@") and ((buffer[1]="z") or (buffer[1]="Z")))
      then goto done;
continue:end;
done:
prime_the_change_buffer(i);
exit: end;
@t\hskip 2in@>

procedure compare_change_files;
label restart;
var i,j:0..ch_max;
begin
restart:
for i:=1 to ch_max-1 do
   for j:=i+1 to ch_max do
   if not ch_dont_match(i,j) then
      begin
      print_nl(' WARNING: change_file ',i:1,' line ',ch_line[i]:4);
      print_nl('      and change_file ',j:1,' line ',ch_line[j]:4,
	      ' refer to the same text !!!!');
      print_nl(' Change_file ',j:1,' will be advanced.');new_line;
      pre_prime_the_change_buffer(j);
      mark_harmless; goto restart
      end;
end;
@z

@x (76) we have to use an index for change-file
@ While looking for a line that begins with \.{@@x} in the change file,
we allow lines that begin with \.{@@}, as long as they don't begin with
\.{@@y} or \.{@@z} (which would probably indicate that the change file is
fouled up).

@<Skip over comment lines in the change file...@>=
loop@+  begin incr(line);
  if not input_ln(change_file) then return;
  if limit<2 then goto continue;
  if buffer[0]<>"@@" then goto continue;
  if (buffer[1]>="X")and(buffer[1]<="Z") then
    buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
  if buffer[1]="x" then goto done;
  if (buffer[1]="y")or(buffer[1]="z") then
    begin loc:=2; err_print('! Where is the matching @@x?');
@.Where is the match...@>
    end;
continue: end;
done:
@y
@ While looking for a line that begins with \.{@@x} in the change file,
indexed by |i|,
we allow lines that begin with \.{@@}, as long as they don't begin with
\.{@@y} or \.{@@z} (which would probably indicate that the change file is
fouled up).

@<Skip over comment lines in the change file...@>=
loop@+  begin line:=ch_line[i];incr(line);ch_line[i]:=line;
  input_ch(i);
  if not ch_not_eof then return;
  if limit<2 then goto continue;
  if buffer[0]<>"@@" then goto continue;
  if (buffer[1]>="X")and(buffer[1]<="Z") then
    buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
  if buffer[1]="x" then goto done;
  if (buffer[1]="y")or(buffer[1]="z") then
    begin loc:=2;
	  err_print('! Where is the matching @@x in change-file ',i:1,' ?');
@.Where is the match...@>
    end;
continue: end;
done:
@z

@x (77) we have to use an index using change-file
@ Here we are looking at lines following the \.{@@x}.

@<Skip to the next nonblank line...@>=
repeat incr(line);
  if not input_ln(change_file) then
    begin err_print('! Change file ended after @@x');
@.Change file ended...@>
    return;
    end;
until limit>0;
@y
@ Here we are looking at lines following the \.{@@x}.

@<Skip to the next nonblank line...@>=
repeat line:=ch_line[i];incr(line);ch_line[i]:=line;
  input_ch(i);
  if not ch_not_eof then
    begin err_print('! Change file ',i:1,' ended after @@x');
@.Change file ended...@>
    return;
    end;
until limit>0;
@z

@x (78) we have to specify the change-file by an index
@ @<Move |buffer| and |limit| to |change_buffer| and |change_limit|@>=
begin change_limit:=limit;
if limit>0 then for k:=0 to limit-1 do change_buffer[k]:=buffer[k];
end
@y
@ @<Move |buffer| and |limit| to |change_buffer| and |change_limit|@>=
begin change_limit[i]:=limit;
if limit>0 then for k:=0 to limit-1 do change_buffer[i,k]:=buffer[k];
end
@z

@x (79) we have to specify and work with the actual change-file
@ The following procedure is used to see if the next change entry should
go into effect; it is called only when |changing| is false.
The idea is to test whether or not the current
contents of |buffer| matches the current contents of |change_buffer|.
If not, there's nothing more to do; but if so, a change is called for:
All of the text down to the \.{@@y} is supposed to match. An error
message is issued if any discrepancy is found. Then the procedure
prepares to read the next line from |change_file|.

@p procedure check_change; {switches to |change_file| if the buffers match}
label exit;
var n:integer; {the number of discrepancies found}
@!k:0..buf_size; {index into the buffers}
begin if lines_dont_match then return;
n:=0;
loop@+  begin change_changing; {now it's |true|}
  incr(line);
  if not input_ln(change_file) then
    begin err_print('! Change file ended before @@y');
@.Change file ended...@>
    change_limit:=0;  change_changing; {|false| again}
    return;
    end;
  @<If the current line starts with \.{@@y},
    report any discrepancies and |return|@>;
  @<Move |buffer| and |limit|...@>;
  change_changing; {now it's |false|}
  incr(line);
  if not input_ln(web_file) then
    begin err_print('! WEB file ended during a change');
@.WEB file ended...@>
    input_has_ended:=true; return;
    end;
  if lines_dont_match then incr(n);
  end;
exit: end;
@y
@ The following procedure is used to see if the next change entry should
go into effect; it is called only when |changing| is false.
The idea is to test whether or not the current contents of |buffer|
matches the current contents of one of the |change_buffers|.
If not, there's nothing more to do; but if so, a change is called for:
All of the text down to the \.{@@y} is supposed to match. An error
message is issued if any discrepancy is found. Then the procedure
prepares to read the next line from the current |change_file|.

Since we use several |change_file|s we first have to examine all
|change_buffer|s. The index of the matching |change_file| is written to the
variable |ch_act| by the function |lines_dont_match|.
Each time one line of the current |change_file| is read into its
|change_buffer| this one is compared with all other |change_buffers|.
In case of equality the latter |change_file| will be advanced by the
function |pre_prime_the_change_buffer|.


@p procedure check_change; {switches to |change_file| if the buffers match}
label exit,done,continue;
var n:integer; {index, the number of discrepancies found}
    i:integer; {loop variable for processing the |change_files|}
    temp:integer; {for temporary storage of the contents of |ch_act|}
@!k:0..buf_size; {index into the buffers}
begin
for i:= 1 to ch_max do
   if not lines_dont_match(i) then goto done;
   return;{no change-file matches}
done:{one change-file matches, the index has been written to ch-act}
  n:=0;
loop@+  begin change_changing; {now it's |true|}
  line:=ch_line[ch_act];incr(line);ch_line[ch_act]:=line;
  input_ch(ch_act);
  if not ch_not_eof then
    begin err_print('! Change file ',ch_act:1,' ended before @@y');
@.Change file ended...@>
    change_limit[ch_act]:=0;  change_changing; {|false| again}
    return;
    end;
  @<If the current line starts with \.{@@y},
    report any discrepancies and |return|@>;
  @<Move |buffer| and |limit|...@>;
{the actual change-buffer has to be compared with the other change-buffers,
the index of the actual |change_file| is stored to the variable temp}
	temp:=ch_act;
    for i:=1 to ch_max do
	begin
	if i=ch_act then goto continue;
	if not ch_dont_match(i,ch_act) then
	   begin
	   print_nl(' WARNING: change_file ',i:1,' line ',ch_line[i]:4);
	   print_nl('      and change_file ',ch_act:1,' line ',
		     ch_line[ch_act]:4,' refer to the same text !!!!');
	   print_nl(' Change_file ',i:1,' will be advanced.');new_line;
	   pre_prime_the_change_buffer(i);
	   end;
	continue: end;
  ch_act:=temp;i:=ch_act; {the actual index has to be reloaded}
  change_changing; {now it's |false|}
  incr(line);
  if not input_ln(web_file) then
    begin err_print('! WEB file ended during a change');
@.WEB file ended...@>
    input_has_ended:=true; return;
    end;
  if lines_dont_match(ch_act)  then incr(n);
  end;
exit: end;
@z

@x (80) we specify the actual change-file in the error message
@ @<If the current line starts with \.{@@y}...@>=
if limit>1 then if buffer[0]="@@" then
  begin if (buffer[1]>="X")and(buffer[1]<="Z") then
    buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
  if (buffer[1]="x")or(buffer[1]="z") then
    begin loc:=2; err_print('! Where is the matching @@y?');
@.Where is the match...@>
    end
  else if buffer[1]="y" then
    begin if n>0 then
      begin loc:=2; err_print('! Hmm... ',n:1,
	' of the preceding lines failed to match');
@.Hmm... n of the preceding...@>
      end;
    return;
    end;
  end
@y
@ @<If the current line starts with \.{@@y}...@>=
if limit>1 then if buffer[0]="@@" then
  begin if (buffer[1]>="X")and(buffer[1]<="Z") then
    buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
  if (buffer[1]="x")or(buffer[1]="z") then
    begin loc:=2;
    err_print('! Where is the matching @@y in change_file ',ch_act:1,' ?');
@.Where is the match...@>
    end
  else if buffer[1]="y" then
    begin if n>0 then
      begin loc:=2; err_print('! Hmm... ',n:1,
	' of the preceding lines failed to match in change_file ',ch_act:1);
@.Hmm... n of the preceding...@>
      end;
    return;
    end;
  end
@z

@x (81) to initialize we have to look at all change buffers
@ The |reset_input| procedure, which gets \.{WEAVE} ready to read the
user's \.{WEB} input, is used at the beginning of phases one and two.

@p procedure reset_input;
begin open_input; line:=0; other_line:=0;@/
changing:=true; prime_the_change_buffer; change_changing;@/
limit:=0; loc:=1; buffer[0]:=" "; input_has_ended:=false;
end;
@y
@ The |reset_input| procedure, which gets \.{WEAVE} ready to read the
user's \.{WEB} input, is used at the beginning of phases one and two.

@p procedure reset_input;
var i : integer; {local loop variable}
begin open_input; line:=0; other_line:=0;@/
for i:=1 to ch_max do ch_line[i]:=0;@/
changing:=true;
for i:=1 to ch_max do  prime_the_change_buffer(i);@/
compare_change_files;change_changing;@/
limit:=0; loc:=1; buffer[0]:=" "; input_has_ended:=false;
end;
@z

@x (82) we need a loop variable i and to store the module number
@ The |get_line| procedure is called when |loc>limit|; it puts the next
line of merged input into the buffer and updates the other variables
appropriately. A space is placed at the right end of the line.

@p procedure get_line; {inputs the next line}
label restart;
begin restart: if changing then changed_module[module_count]:=true
else  @<Read from |web_file| and maybe turn on |changing|@>;
if changing then
  begin @<Read from |change_file| and maybe turn off |changing|@>;
  if not changing then
    begin changed_module[module_count]:=true; goto restart;
    end;
  end;
loc:=0; buffer[limit]:=" ";
end;
@y
@ The |get_line| procedure is called when |loc>limit|; it puts the next
line of merged input into the buffer and updates the other variables
appropriately. A space is placed at the right end of the line.

In \.{TWIST} we add the procedure |set_index|. The idea is, that we don't
only want to mark a module as changed, as in the \.{WEAVE}--version with a
single |change_file|, but we want to enumerate all numbers of those
|change_files| influencing the module.

The i-th |change_file| is represented by the i-th prime. Using
primes allows us to verify easily whether the actual |change_file|
is already marked by the value of |changed_module[m]|. This
is neccessary since each time a line is read from a |change_file|
\.{TWIST}  wants to mark its number in |changed_module[m]|.


@p procedure set_index(m,i:integer);
{ m specifies the module, i specifies the |change_file| to be taken as index}
begin
if changed_module[m] mod prime[i] <> 0
   then changed_module[m] := changed_module[m] * prime[i];
end;
@#
procedure get_line; {inputs the next line}
label restart;var i:integer;{ loop variable for processing the change files}
ch_flag:boolean; {flag whether to use |check_change| or not}
begin restart: if changing then set_index(module_count,ch_act)
else  @<Read from |web_file| and maybe turn on |changing|@>;
if changing then
  begin @<Read from |change_file| and maybe turn off |changing|@>;
  if not changing then
    begin set_index(module_count,ch_act); goto restart;
    end;
  end;
loc:=0; buffer[limit]:=" ";
end;
@z

@x (83) we have to specify the actual change-file
@ @<Read from |web_file|...@>=
begin incr(line);
if not input_ln(web_file) then input_has_ended:=true
else if limit=change_limit then
  if buffer[0]=change_buffer[0] then
    if change_limit>0 then check_change;
end
@y
@ @<Read from |web_file|...@>=
begin incr(line);
if not input_ln(web_file) then input_has_ended:=true
else begin ch_flag:=false;
     for i:=1 to ch_max do begin
     if limit=change_limit[i] then
  if buffer[0]=change_buffer[i,0] then
    if change_limit[i]>0 then ch_flag:=true;  end;
     if ch_flag then check_change;end;
end
@z

@x (84) we have to specify the change-file
@ @<Read from |change_file|...@>=
begin incr(line);
if not input_ln(change_file) then
  begin err_print('! Change file ended without @@z');
@.Change file ended...@>
  buffer[0]:="@@"; buffer[1]:="z"; limit:=2;
  end;
if limit>1 then {check if the change has ended}
  if buffer[0]="@@" then
    begin if (buffer[1]>="X")and(buffer[1]<="Z") then
      buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
    if (buffer[1]="x")or(buffer[1]="y") then
      begin loc:=2; err_print('! Where is the matching @@z?');
@.Where is the match...@>
      end
    else if buffer[1]="z" then
      begin prime_the_change_buffer; change_changing;
      end;
    end;
end
@y
@ @<Read from |change_file|...@>=
begin line:=ch_line[ch_act];incr(line);ch_line[ch_act]:=line;
input_ch(ch_act);
if not ch_not_eof then
  begin err_print('! Change file ',ch_act:1,' ended without @@z');
@.Change file ended...@>
  buffer[0]:="@@"; buffer[1]:="z"; limit:=2;
  end;
if limit>1 then {check if the change has ended}
  if buffer[0]="@@" then
    begin if (buffer[1]>="X")and(buffer[1]<="Z") then
      buffer[1]:=buffer[1]+"z"-"Z"; {lowercasify}
    if (buffer[1]="x")or(buffer[1]="y") then
      begin loc:=2; err_print('! Where is the matching @@z in change_file ',
			       ch_act:1,' ?');
@.Where is the match...@>
      end
    else if buffer[1]="z" then
      begin prime_the_change_buffer(ch_act);
	    compare_change_files; change_changing;
      end;
    end;
end
@z

@x (85) we have to specify the actual change-file
@ At the end of the program, we will tell the user if the change file
had a line that didn't match any relevant line in |web_file|.

@<Check that all changes have been read@>=
if change_limit<>0 then {|changing| is false}
  begin for loc:=0 to change_limit do buffer[loc]:=change_buffer[loc];
  limit:=change_limit; changing:=true; line:=other_line; loc:=change_limit;
  err_print('! Change file entry did not match');
@y
@ At the end of the program, we will tell the user if one of the
|change_file|s had a line that didn't match any relevant line in |web_file|.

@<Check that all changes have been read@>=
for i:=1 to ch_max do begin
if change_limit[i]<>0 then {|changing| is false}
  begin for loc:=0 to change_limit[i] do buffer[loc]:=change_buffer[i,loc];
  limit:=change_limit[i]; changing:=true; line:=ch_line[i];
  loc:=change_limit[i];
  ch_act:=i;
  err_print('! Change_file ',i:1,' entry did not match');
@.Change file entry did not match@>
  end
@z

@x (109) we have to specify the change-file number
@ The overall processing strategy in phase one has the following
straightforward outline.

@<Phase I: Read all the user's text and store the cross references@>=
phase_one:=true; phase_three:=false;
reset_input;
module_count:=0; skip_limbo; change_exists:=false;
while not input_has_ended do
  @<Store cross reference data for the current module@>;
changed_module[module_count]:=change_exists;
  {the index changes if anything does}
phase_one:=false; {prepare for second phase}
@<Print error messages about unused or undefined module names@>;
@y
@ The overall processing strategy in phase one has the following
straightforward outline.

@<Phase I: Read all the user's text and store the cross references@>=
phase_one:=true; phase_three:=false;
reset_input;
module_count:=0; skip_limbo; change_exists:=1;
while not input_has_ended do
  @<Store cross reference data for the current module@>;
changed_module[module_count]:=change_exists;
  {the index changes if anything does}
phase_one:=false; {prepare for second phase}
@<Print error messages about unused or undefined module names@>;
@z

@x (110) we have to ask for zero instead of false
@ @<Store cross reference data...@>=
begin incr(module_count);
if module_count=max_modules then overflow('section number');
changed_module[module_count]:=false; {it will become |true| if any line changes}
if buffer[loc-1]="*" then
  begin print('*',module_count:1);
  update_terminal; {print a progress report}
  end;
@<Store cross references in the \TeX\ part of a module@>;
@<Store cross references in the \(definition part of a module@>;
@<Store cross references in the \PASCAL\ part of a module@>;
if changed_module[module_count] then change_exists:=true;
end
@y
@ @<Store cross reference data...@>=
begin incr(module_count);
if module_count=max_modules then overflow('section number');
changed_module[module_count]:=1;{it will become |<>1| if any line changes}
if buffer[loc-1]="*" then
  begin print('*',module_count:1);
  update_terminal; {print a progress report}
  end;
@<Store cross references in the \TeX\ part of a module@>;
@<Store cross references in the \(definition part of a module@>;
@<Store cross references in the \PASCAL\ part of a module@>;
if changed_module[module_count] <> 1 then
change_exists:=changed_module[module_count];
end
@z

@x
`\.{\\input webmac}'.

@<Set init...@>=
out_ptr:=1; out_line:=1; out_buf[1]:="c"; write(tex_file,'\input webma');
@y
`\.{\\input patchmac}'.

@<Set init...@>=
out_ptr:=1; out_line:=1; out_buf[1]:="c"; write(tex_file,'\input patchma');
@z

@x (130)we indicate the change-file's number instead of an asteriks
@ The number to be converted by |out_mod| is known to be less than
|def_flag|, so it cannot have more than five decimal digits.  If
the module is changed, we output `\.{\\*}' just after the number.

@p procedure out_mod(@!m:integer); {output a module number}
var k:0..5; {index into |dig|}
@!a:integer; {accumulator}
begin k:=0; a:=m;
repeat dig[k]:=a mod 10; a:=a div 10; incr(k);
until a=0;
repeat decr(k); out(dig[k]+"0");
until k=0;
if changed_module[m] then out2("\")("*");
@.\\*@>
end;
@y
@ The number to be converted by |out_mod| is known to be less than
|def_flag|, so it cannot have more than five decimal digits.  If
the module is changed, we output the numbers of the corresponding
|change_files| as indices just after the number.
Since we coded the numbers of the influencing |change_files| as
the product of the corresponding primes, we have to use the procedure
|out_index| to decode the corresponding numbers of the |change_files|.

@p procedure out_index(m:integer); {output the |change--file| number}
var i:integer;
begin
i:=1;
while m mod prime[i] <> 0 do incr(i);
out(i+"0");incr(i);
while i <= ch_max do begin
   if m mod prime[i] = 0 then begin out(",");out(i+"0") end;
   incr(i) end;
end;
@#
procedure out_mod(@!m:integer); {output a module number}
var k:0..5; {index into |dig|}
@!a:integer; {accumulator}
begin k:=0; a:=m;
repeat dig[k]:=a mod 10; a:=a div 10; incr(k);
until a=0;
repeat decr(k); out(dig[k]+"0");
until k=0;
if changed_module[m] <> 1 then begin out3("$")("^")("{");
 out_index(changed_module[m]);out2("}")("$");end
@.\\*@>
end;
@z

@x (239) change_exist now is an integer-variable
We are nearly finished! \.{WEAVE}'s only remaining task is to write out the
index, after sorting the identifiers and index entries.

@<Phase III: Output the cross-reference index@>=
phase_three:=true; print_nl('Writing the index...');
if change_exists then
  begin finish_line; @<Tell about changed modules@>;
  end;
finish_line; out4("\")("i")("n")("x"); finish_line;
@.\\inx@>
@<Do the first pass of sorting@>;
@<Sort and output the index@>;
out4("\")("f")("i")("n"); finish_line;
@.\\fin@>
@<Output all the module names@>;
out4("\")("c")("o")("n"); finish_line;
@.\\con@>
print('Done.');
@y
We are nearly finished! \.{WEAVE}'s only remaining task is to write out the
index, after sorting the identifiers and index entries.

@<Phase III: Output the cross-reference index@>=
phase_three:=true; print_nl('Writing the index...');
if change_exists <> 1 then
  begin finish_line; @<Tell about changed modules@>;
  end;
finish_line; out4("\")("i")("n")("x"); finish_line;
@.\\inx@>
@<Do the first pass of sorting@>;
@<Sort and output the index@>;
out4("\")("f")("i")("n"); finish_line;
@.\\fin@>
@<Output all the module names@>;
out4("\")("c")("o")("n"); finish_line;
@.\\con@>
print('Done.');
@z

@x (241) we indicate the change-file's number instead of an asteriks
@ @<Tell about changed modules@>=
begin {remember that the index is already marked as changed}
k_module:=1;
while not changed_module[k_module] do incr(k_module);
out4("\")("c")("h")(" ");
out_mod(k_module);
repeat repeat incr(k_module)@+ until changed_module[k_module];
  out2(",")(" "); out_mod(k_module);
until k_module=module_count;
out(".");
end
@y
@ @<Tell about changed modules@>=
begin {remember that the index is already marked as changed}
k_module:=1;
while changed_module[k_module] = 1 do incr(k_module);
out4("\")("c")("h")(" ");
out_mod(k_module);
repeat repeat incr(k_module)@+ until changed_module[k_module] <> 1;
  out2(",")(" "); out_mod(k_module);
until k_module=module_count;
out(".");
end
@z
