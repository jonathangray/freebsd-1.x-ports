(* picture.sml *)

(* Picture example based on Andy Koenig's columns on "Analysis of a
   classroom exercise" (Parts 1-3).

   The abstraction implements two-dimensional character pictures that
   can be framed, composed horizontally and vertically, and printed. *)

signature PICTURE =
sig
  type picture
  val picture : string list -> picture
       (* the strings in the argument form successive rows of the picture *)
  val frame : picture -> picture
       (* add a frame around the picture *)
  val hcat : picture * picture -> picture
       (* concatenate two pictures horizontally, alligning tops *)
  val vcat : picture * picture -> picture
       (* concatenate two pictures vertically, alligning left edges *)
  val width : picture -> int
       (* return the width of a picture in characters *)
  val height : picture -> int
       (* return the height of a picture in characters *)
  val display : outstream -> picture -> unit
       (* print a two-dimensional representation of the picture on the
	  given outstream *)
end

abstraction Picture: PICTURE =  (* Picture.picture is an abstract type *)
struct

  datatype contents
    = ROWS of string list
    | FRAME of picture
    | HCAT of picture * picture
    | VCAT of picture * picture
  withtype picture = {cont: contents, width: int, height: int}

  fun picture(rows: string list) =
      {cont = ROWS rows,
       width = fold (fn (s,m) => max(m, size s)) rows 0,
       height = length rows}

  fun frame (p as {cont,width,height}: picture) =
      {cont = FRAME p, width = width + 2, height = height + 2}

  fun hcat(p1 as {width = w1, height = h1,...}: picture,
	   p2 as {width = w2, height = h2,...}: picture) = 
      {cont = HCAT(p1,p2), width = w1+w2, height = max(h1,h2)}

  fun vcat(p1 as {width = w1, height = h1,...}: picture,
	   p2 as {width = w2, height = h2,...}: picture) = 
      {cont = VCAT(p1,p2), width = max(w1,w2), height = h1+h2}

  fun width({width,...}: picture) = width
  fun height({height,...}: picture) = height

  fun display (os: outstream) (p as {width = wd, height = ht,...}: picture) =
      let val pr = outputc os
	  fun pad(ch,n) =
	      let fun for(0) = ()
		    | for(i) = (pr ch; for(i-1))
	       in for n
	      end
	  fun display_row({cont,width=w,height=h},row,row_width) =
	      case cont
		of ROWS rows =>
		    (let val r = nth(rows,row)
		      in pr r; pad(" ", row_width - size r)
		     end
		     handle Nth => pad(" ", row_width))
	         | FRAME p =>
		     if row < 0 orelse row >= h
		     then pad(" ",row_width)
		     else (if row = 0 orelse row = h-1
		           then (pr "+"; pad("-",width(p)); pr "+")
		           else (pr "|"; display_row(p,row-1,width(p)); pr "|");
			   pad(" ", row_width - w))
	         | HCAT(left,right) =>
		    (display_row(left,row,width(left));
		     display_row(right,row,width(right));
		     pad(" ",row_width-w))
		 | VCAT(top,bottom) =>
		     if row >= 0 andalso row < height(top)
		     then display_row(top,row,row_width)
		     else if row >= height(top) andalso row < h
		     then display_row(bottom,row-height(top),row_width)
		     else pad(" ",row_width)
	  fun scan row =
	      if row >= ht then ()
	      else (display_row(p,row,wd); pr "\n"; scan(row+1))
       in scan 0
      end

end (* structure Picture *)
