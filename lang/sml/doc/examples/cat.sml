datatype limit = Unbounded | Limited of int

fun cat Unbounded (instream,outstream) =
    while not (end_of_stream instream) do
	  outputc outstream (inputc instream (can_input instream))
  | cat (Limited limit) (instream,outstream) =
    while not (end_of_stream instream) do
	  outputc outstream (inputc instream (min(can_input instream,limit)))
      
val ucat = cat Unbounded
val lcat = cat(Limited 1024)

fun cat1 (instream,outstream) =
    while not (end_of_stream instream) do
	  outputc outstream (inputc instream 1)

fun filecat cat = fn (fin,fout) => cat(open_in fin,open_out fout)
fun screencat cat = fn fin => cat(open_in fin,std_out)

