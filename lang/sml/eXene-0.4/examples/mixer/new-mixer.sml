(* new-mixer.sml
 *)

local
    open CML EXeneBase Widget Geometry Box
    open ColorState

    val maxcolor = 65535
    val mincolor = 0 
    val display_border_width = 10
    val slider_width = 20 
    val hue_box_dim = 25
    val big_spot_ht = 400
    val big_spot_wid = 150

    val hglue = Glue {nat=5, min=5, max=SOME 5}
    val vglue = Glue {nat=5, min=1, max=NONE}
	  
    val pause = TIME{sec=0,usec=500000}

    val redc = {red=maxcolor,green=0,blue=0}
    val greenc = {red=0,green=maxcolor,blue=0}
    val bluec = {red=0,green=0,blue=maxcolor}
    val blackc = {red=0,green=0,blue=0}
    val whitec = {red=maxcolor,green=maxcolor,blue=maxcolor}
    val mixer_back = {red=47822,green=21254,blue=49594}
    fun mk_red n = {red = n,green=mincolor,blue=mincolor}
    fun mk_green n = {red = mincolor,green=n,blue=mincolor}
    fun mk_blue n = {red = mincolor,green=mincolor,blue=n}

 fun MakeMixer root = let

    val white = whiteOfScr (screenOf root)
    val colorOf = ColorState.colorOf root
    fun quit () = 
      spawn (fn () => (sync(timeout pause); delRoot root; RunCML.shutdown()))
    
    val switch_line = HzCenter [
	    		vglue,
	    		WBox (Toggle.widgetOf (Toggle.mkToggleSwitch root {
		                action = fn _ => (quit ();()),
		                state = W.Active false,
		                backgrnd = NONE,
		                foregrnd = NONE})),
	    		hglue
	  	      ]

    fun mkDisplayBox c w = let
	  val dpy = Frame.mkFrame {
		  widget = Shape.mkRigid w,
		  color = SOME c,
		  width = display_border_width
		}
	  in
	    HzCenter [vglue,WBox (Frame.widgetOf dpy),vglue]
	  end

    fun paintSpot spot c = 
          (Spot.setSpot spot c) handle _ => (print "out of color cells\n"; quit(); ())

    val spot = Spot.mkSpot root { 
		init_rgb = blackc, 
                ht = big_spot_ht, wid =big_spot_wid}
    val paint = paintSpot spot 
    val color_screen = mkDisplayBox white (Spot.widgetOf spot)

    val cc = mkColorState blackc
    val send_cc = sendChangeColor cc 
    val cc_evt = evtOfColorState cc
    fun painter () = painter (paint (sync cc_evt)) 

    fun mkcolorcomplex c mk_color mkmsg = 
	let val label = Label.mkLabel root {
			 label = "          0",
			 foregrnd = SOME white,
			 backgrnd = NONE,
			 font = NONE,
		         align = HRight}
            val color = colorOf c
            val display = mkDisplayBox color (Label.widgetOf label)
            val slider = Slider.mkVSlider root { 
			foregrnd = SOME color,
			wid = slider_width,
			init = 0,
			scale = maxcolor
		     }
            val spot = Spot.mkSpot root { 
		     init_rgb = blackc,
                     ht = hue_box_dim, wid = hue_box_dim}
	    val screen = mkDisplayBox white (Spot.widgetOf spot)
            val line = HzCenter [
			 hglue, 
			 screen, 
                         hglue, 
                         WBox (Slider.widgetOf slider), 
                         hglue,
			 display, 
                         hglue]

	    val set = Label.setLabel label
            val evt = Slider.evtOf slider
	    val paint = paintSpot spot 
	    fun printer_loop () = 
	           printer_loop (let val n = sync evt
                                 in (set (makestring n); 
			             paint (mk_color n);
			             send_cc (mkmsg n))
			         end) 
	    in (line, printer_loop) end 

    val (red_line, red_printer_loop) =  mkcolorcomplex redc mk_red ChangeR
    val (green_line, green_printer_loop) =  mkcolorcomplex greenc mk_green ChangeG
    val (blue_line, blue_printer_loop) = mkcolorcomplex bluec mk_blue ChangeB

    in
        (spawn red_printer_loop; 
	 spawn green_printer_loop;
	 spawn blue_printer_loop ;
	 spawn painter);
	 mkLayout root (VtCenter [
		vglue,
		color_screen, 
		vglue,		
		switch_line,
		vglue,
		red_line,
		vglue,
		green_line,
		vglue,
		blue_line,
		vglue
	      ])
      end (* end MakeMixer *)


fun initshell server = let
	val root = mkRoot server
	val mix = MakeMixer root
	val shell = Shell.mkShell
	      (Box.widgetOf mix, SOME (ColorState.colorOf root mixer_back), 
                {win_name = SOME "RGB Mixer", icon_name = SOME "MIX"})
	in
	  Shell.init shell
	end
in

fun mixer (debugFlgs, server) = (
      XDebug.init debugFlgs;
      RunCML.doit (fn () => initshell server, SOME 10))

fun doit s = mixer([],s)

fun main (prog::server::_,_) = doit server
  | main _ = doit ""

end; (* local *)
