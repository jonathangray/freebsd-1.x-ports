(* scroll-view.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Scrollbar views.
 *)

signature SCROLL_VIEW =
  sig
    type scroll_view

    val horzScrollbar : scroll_view
    val vertScrollbar : scroll_view

  end (* SCROLL_VIEW *)

structure ScrollView : SCROLL_VIEW = struct

  type scroll_state = {
    size : int,
    coord : Geometry.point -> int,
    draw : int * int -> unit,
    move : int * int * int * int -> unit
  }

  type config_fn = Geometry.size -> scroll_state

  type scroll_view = {
    bounds_of : int -> unit -> Widget.bounds,
    realize : (Widget.root * EXeneBase.window * EXeneBase.color * int) 
      -> config_fn
  }

  open CML Geometry EXeneBase EXeneWin Interact Drawing Widget

  fun drawableOf win = Drawing.feedback(Drawing.drawableOfWin win)

  fun move_fn (clear, fill) (x:int,w:int,x',w') = let
    val e = x+w
    val e' = x'+w'
    fun noIntersect () = (
      clear (x,w);
      fill (x',w')
    )
  in
    if x < x' then
      if e <= x' then noIntersect ()
      else if e < e' then (
        clear (x,x'-x);
        fill (e,e'-e)
      )
      else (
        clear (x,x'-x);
        clear (e',e-e')
      )
    else if x = x' then
      if e <= e' then fill (e,e'-e)
      else clear (e',e-e')
    else if x < e' then
      if e > e' then (
        fill(x',x-x');
        clear(e',e-e')
      )
      else (
        fill(x',x-x');
        fill(e,e'-e)
      )
    else noIntersect ()
  end

  fun vertRealize (root, win, color, dim) = let
    val pen = newPen [PV_Foreground color]
    val fill = fillRect (drawableOf win) pen 
    val clear = clearArea (drawableOf win)

    fun config (SIZE{wid,ht}) = let
      fun drawFn (x,w) = (
        clear (RECT{x=0,y=0,wid=wid,ht=ht});
        fill (RECT{y=x,x=0,ht=w,wid=wid})
      )
      fun clr (x,w) = clear (RECT{y=x,x=0,ht=w,wid=wid})
      fun fil (x,w) = fill (RECT{y=x,x=0,ht=w,wid=wid})
      val moveFn = move_fn (clr,fil)
    in
      {
        size = ht,
        coord = fn PT{x,y} => y,
        draw = drawFn,
        move = moveFn
      }
    end
  in
    config
  end

  fun vertBounds dim = let
    val swid = dim div 3
    val bnds = { 
        x_dim = fixDim dim,
        y_dim = DIM{base=swid,incr=1,min=0,nat=0,max=NONE}
      }
  in
    fn () => bnds
  end

  fun horzRealize (root, win, color, dim) = let
    val pen = newPen [PV_Foreground color]
    val fill = fillRect (drawableOf win) pen 
    val clear = clearArea (drawableOf win)

    fun config (SIZE{wid,ht}) = let
      fun drawFn (x,w) = (
        clear (RECT{x=0,y=0,wid=wid,ht=ht});
        fill (RECT{x=x,y=0,wid=w,ht=ht})
      )
      fun moveFn (x,w,x',w') = (
        clear (RECT{x=x,y=0,wid=w,ht=ht});
        fill (RECT{x=x',y=0,wid=w',ht=ht})
      )
      fun clr (x,w) = clear (RECT{x=x,y=0,wid=w,ht=ht})
      fun fil (x,w) = fill (RECT{x=x,y=0,ht=ht,wid=w})
      val moveFn = move_fn (clr,fil)
    in
      {
        size = wid,
        coord = fn PT{x,y} => x,
        draw = drawFn,
        move = moveFn
      }
    end
  in
    config
  end

  fun horzBounds dim = let
    val swid = dim div 3
    val bnds = { 
        y_dim = fixDim dim,
        x_dim = DIM{base=swid,incr=1,min=0,nat=0,max=NONE}
      }
  in
    fn () => bnds
  end

  val horzScrollbar = { bounds_of=horzBounds, realize=horzRealize }
  val vertScrollbar = { bounds_of=vertBounds, realize=vertRealize }

end (* ScrollView *)
