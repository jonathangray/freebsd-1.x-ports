(* color-state.sml
 *)

signature COLORSTATE =
  sig

    structure CML : CONCUR_ML
    structure W : WIDGET

    type rgb

    val colorOf : W.root -> rgb -> W.EXB.color

    datatype change_color_msg = 
	ChangeR of int 
      | ChangeG of int 
      | ChangeB of int 
      | ChangeRGB of rgb
    
    type color_state

    val mkColorState : rgb -> color_state
    val sendChangeColor : color_state -> change_color_msg -> unit
    val evtOfColorState : color_state -> rgb CML.event

  end;

structure ColorState : COLORSTATE = 
  struct

    structure CML = CML
    structure W = Widget
    open CML
 
    type rgb = {red : int, green : int, blue : int}

    fun colorOf root = let
          val scr = W.screenOf root
          in
            fn rgb => W.EXB.colorOfScr scr (W.EXB.CMS_RGB rgb)
          end

    datatype change_color_msg = 
	ChangeR of int 
      | ChangeG of int 
      | ChangeB of int
      | ChangeRGB of rgb

    datatype color_state = 
             ColorState of (change_color_msg chan * rgb chan)

    fun mkColorState initial_color = 
        let 
          open EXeneBase
	  val in_chan = channel()
	  val out_chan = channel()
	  fun get_msg () = accept in_chan
	  fun send_color c = send(out_chan,c)

          fun change_color ({red=r,green=g,blue=b}, msg) = 
               let val new_color = 
                   (case msg 
                    of (ChangeR n) => {red=n,green=g,blue=b}
		     | (ChangeG n) => {red=r,green=n,blue=b}
		     | (ChangeB n) => {red=r,green=g,blue=n}
		     | (ChangeRGB r) => r)
                   in (send_color new_color; new_color)
	       end

	  fun loop color = loop(change_color(color, get_msg ()))

	  in
	    spawn (fn () => loop initial_color);
	    ColorState(in_chan, out_chan)
	  end 

    fun sendChangeColor (ColorState(in_chan, _)) msg = send (in_chan, msg)

    fun evtOfColorState (ColorState(_, out_chan)) = receive out_chan

end; (* ColorState *)
