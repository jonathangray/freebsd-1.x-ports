(* slider-view.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Slider views.
 *)

signature SLIDER_VIEW = 
  sig

    type slider_view

    val horzSlider : slider_view
    val vertSlider : slider_view

  end (* SLIDER_VIEW *)

structure SliderView : SLIDER_VIEW = 
  struct

    local open CML Geometry EXeneBase EXeneWin Interact Drawing Widget in

    type slider_state = {
      maxx : int,
      swid : int,
      coord : point -> int,
      draw : (int -> unit),
      move : ((int * int) -> unit)
    }

    type config_fn = size -> slider_state

    type slider_view = {
      bounds_of : int -> unit -> bounds,
      realize : root * window * color -> config_fn
    }

    fun drawableOf win = Drawing.feedback(Drawing.drawableOfWin win)

    fun major true = (fn PT{y,...} => y)
      | major false = (fn PT{x,...} => x)
    fun minor false = (fn PT{y,...} => y)
      | minor true = (fn PT{x,...} => x)

    fun drawFn (true,xoff,ht,sht,swid,fill,clear) = let
          val barRect = RECT{x=xoff,y=0,wid=xoff,ht=ht}
          in
            fn x => (clear (); fill barRect; fill (RECT{x=0,y=x,ht=sht,wid=swid}))
          end
      | drawFn (false,xoff,ht,sht,swid,fill,clear) = let
          val barRect = RECT{y=xoff,x=0,ht=xoff,wid=ht}
          in
            fn x => (clear (); fill barRect; fill (RECT{y=0,x=x,wid=sht,ht=swid}))
          end
                  
    fun moveFn (true,xoff,ht,sht,swid,fill,clear) = let
          val xoff' = xoff + xoff
          val rtwid = swid - xoff'
          in
            fn (x,x') => (clear (RECT{y=x,x=0,ht=sht,wid=xoff});
                          clear (RECT{y=x,x=xoff',ht=sht,wid=rtwid});
                          fill (RECT{y=x',x=0,ht=sht,wid=swid}))
          end
      | moveFn (false,xoff,ht,sht,swid,fill,clear) = let
          val xoff' = xoff + xoff
          val rtwid = swid - xoff'
          in
            fn (x,x') => (clear (RECT{x=x,y=0,wid=sht,ht=xoff});
                          clear (RECT{x=x,y=xoff',wid=sht,ht=rtwid});
                          fill (RECT{x=x',y=0,wid=sht,ht=swid}))
          end

    fun realize isVert (root, win, color) = let
          val pen = newPen [PV_Foreground color]
          val draww = drawableOf win
          val fill = fillRect draww pen 
          val clear = clearArea draww
          fun clearD () = clearDrawable draww
          val major = major isVert
          val minor = minor isVert

          fun config (SIZE{wid,ht}) = let
                val ptsz = PT{x=wid,y=ht}
                val major_dim = major ptsz
                val minor_dim = minor ptsz
                val swid = minor_dim div 3
                val maxx = major_dim - swid
                in
                  {
                    swid = swid,
                    maxx = maxx,
                    coord = major,
                    draw = drawFn (isVert,swid,major_dim,swid,minor_dim,fill,clearD),
                    move = moveFn (isVert,swid,major_dim,swid,minor_dim,fill,clear)
                  }
                end
          in
            config
          end

    fun bounds isVert dim = let
          val swid = dim div 3
          val (dim,dim') = 
            (fixDim dim, DIM{base=swid,incr=1,min=0,nat=0,max=NONE})
          in
            if isVert then fn () => {x_dim = dim, y_dim = dim'}
            else fn () => {x_dim = dim', y_dim = dim}
          end

    val horzSlider = { bounds_of=bounds false, realize=realize false}
    val vertSlider = { bounds_of=bounds true, realize=realize true}

  end (* local *)
  end (* SliderView *)
