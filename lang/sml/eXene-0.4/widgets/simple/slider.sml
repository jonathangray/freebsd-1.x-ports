(* slider.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Analogue slider.
 *)

signature SLIDER = 
  sig

    structure CML : CONCUR_ML
    structure W : WIDGET

    type slider

    val mkHSlider : W.root -> 
      {foregrnd : W.EXB.color option,init : int, scale : int,wid : int} -> slider

    val mkVSlider : W.root -> 
      {foregrnd : W.EXB.color option,init : int, scale : int,wid : int} -> slider

    val widgetOf : slider -> W.widget
    val evtOf : slider -> int CML.event
    val setValue : slider -> int -> unit
    val getValue : slider -> int
    val getScale : slider -> int

  end (* SLIDER *)

structure Slider : SLIDER = 
  struct

    structure CML = CML
    structure W = Widget

    local 
      open Geometry EXeneWin Drawing Widget SliderView

      datatype mse_msg = Grab of point | Move of point | Ungrab of point

      type slider_rep = {
        curx : int,
        curv : int
      }

            (* mouse reader *)
      fun mseP (mseChan,m) = let
            open Interact
            fun downLoop btn =
                  case msgBodyOf (CML.sync m) of 
                    MOUSE_LastUp {pt,...} => CML.send (mseChan, Ungrab pt)
                  | MOUSE_Motion {pt,...} => 
                      (CML.send(mseChan, Move pt);downLoop btn)
                  | _ => downLoop btn 

            fun loop () =
                  case msgBodyOf (CML.sync m) of 
                    MOUSE_FirstDown {but,pt,...} => (
                      CML.send (mseChan, Grab pt);
                      downLoop but;
                      loop ()
                    )
                  | _ => loop ()
            in
              loop ()
            end

    in

    datatype rqst = 
      SetValue of int
    | GetValue of int CML.chan
    | GetScale of int CML.chan

    datatype reply = Okay | Error
    
    datatype slider = Slider of {
        widget : W.widget,
        repChan : reply CML.chan,
        rqstChan : rqst CML.chan,
        evt : int CML.event
      }

    fun buffer (inevt, outchan) = let
          fun loop ([],[]) = loop([],[CML.sync inevt])
            | loop (q,[]) = loop([],rev q)
            | loop (q,q' as (m::rest)) = 
                CML.select [
                  CML.wrap(inevt, fn msg => loop(msg::q,q')),
                  CML.wrap(CML.transmit(outchan,m), fn () => loop(q,rest))
                ]
          in loop ([],[]) end

    fun mkSlider (root,colorOpt,dim,init,scale,slideview : slider_view) = let
          val _ = if init < 0 orelse init > scale orelse scale = 0 
                    then LibBase.badArg{
                           module="Slider",
                           func="mkSlider",
                           msg="invalid init or scale"
                         }
                    else ()
          open Interact
          val mseChan = CML.channel ()
          val valChan = CML.channel ()
          val repChan = CML.channel ()
          val rqstChan = CML.channel ()
          val realizeVar : 
            {env : in_env, win : window, sz : size} CML.cond_var = CML.condVar ()
          val mevt = CML.receive mseChan
          val rqstevt = CML.receive rqstChan
          val {bounds_of,realize} = slideview

          val color = 
                case colorOpt of 
                  NONE => W.EXB.blackOfScr (W.screenOf root)
                | SOME color => color

          fun update (v,v') = (if v <> v' then CML.send (valChan, v') else (); v') 

          fun doRqst (GetScale rc,v) = (CML.send (rc, scale);v)
            | doRqst (GetValue rc,v) = (CML.send (rc, v);v)
            | doRqst (SetValue v',v) = 
                if 0 <= v' andalso v' <= scale 
                  then (CML.send (repChan, Okay);update(v,v'))
                  else (CML.send(repChan, Error); v)
  
          fun moveSlide (maxx,move) (me as {curx,curv}, x) = let
                val curx' = min(maxx,max(0,x))
                in
                  if curx' <> curx then let
                    val curv' = update(curv,(scale*curx') div maxx)
                    in
                      move (curx, curx');
                      {curx=curx',curv=curv'}
                    end
                  else me
                end

          fun moveValue (maxx,move,me as {curx,curv}, curv') = let
                val curx' = (curv' * maxx) div scale
                in
                  if curx' <> curx then move (curx, curx') else ();
                  {curx=curx',curv=curv'}
                end

          fun handleCIEvt (draw,reconfig) =
                fn (CI_OwnDeath, me : slider_rep) => me
                 | (CI_Redraw rectlist, me) => (draw (#curx me); me)
                 | (CI_Resize (RECT{wid,ht,...}), {curv,...}) =>
                      reconfig (SIZE{wid=wid,ht=ht}, curv)
                 | (_,me) => me

          fun realizeSlider {env=inenv, win, sz=winsz} init = let
                val InEnv{m,ci,...} = Interact.ignoreKey inenv
                val config = realize (root, win, color)
                val rqstChan' = CML.channel ()
                val rqstevt' = CML.receive rqstChan'

                fun reconfig (sz, curv) = let
                      val data = config sz
                      in
                        cmdP ({curx=(curv*(#maxx data)) div scale, curv=curv}, data)
                      end

                and cmdP (me : slider_rep, {maxx,swid,coord,draw,move}) = let
                     val moveSlide = moveSlide (maxx,move)
                     val handleCIEvt = handleCIEvt (draw,reconfig)
                     fun updateM(x,curx,me) =
                           if curx <= x andalso x < curx + swid 
                             then ((x - curx), me)
                           else if 0 <= x andalso x < maxx+swid then let
                             val curx' = min(maxx, max(0, x - (swid div 2)))
                             in
                               (x - curx', moveSlide (me, curx'))
                             end
                           else if x < 0 then (swid div 2, moveSlide (me, 0))
                           else (swid div 2, moveSlide (me, maxx))

                     fun hMEvt (Ungrab x, me, xoff) = 
                           (false, moveSlide (me, (coord x) - xoff))
                       | hMEvt (Move x, me, xoff) = 
                           (true, moveSlide (me, (coord x) - xoff))
                       | hMEvt (Grab x, me,_) = (true, me)  (* protocol error *)

                     fun handleRqst (evt, me as {curv,...}) = let
                           val curv' = doRqst(evt, curv)
                           in
                             if curv = curv' then me
                             else moveValue(maxx,move,me, curv')
                           end

                     fun handleMEvt (Grab p, me as {curx,...}) = let
                           val x = coord p
                           val (xoff, me') = updateM(x,curx,me)
                           fun loop me = 
                             CML.select [
                               CML.wrap (ci, fn evt => 
                                 loop (handleCIEvt (msgBodyOf evt, me))),
                               CML.wrap (mevt, fn evt => 
                                 case hMEvt (evt, me, xoff) of
                                   (true, m) => loop m
                                 | (false, m) => m)
                             ]
                           in
                             loop me'
                           end
                       | handleMEvt (_,me) = me   (* protocol error *)

                     fun cmdLoop me =
                       cmdLoop (CML.select [
                         CML.wrap (mevt, fn evt => handleMEvt (evt, me)),
                         CML.wrap (rqstevt', fn evt => handleRqst (evt, me)),
                         CML.wrap (ci, fn evt => handleCIEvt (msgBodyOf evt, me))
                       ])
                     in
                       cmdLoop me
                     end
                in
                  CML.spawn (fn () => buffer (rqstevt,rqstChan'));
                  CML.spawn (fn () => mseP (mseChan,m));
                  reconfig (winsz, init)
                end

          fun initLoop init =
                CML.select [
                  CML.wrap(rqstevt, fn arg => initLoop(doRqst (arg,init))),
                  CML.wrap(CML.readVarEvt realizeVar, fn arg => realizeSlider arg init)
                ]
          in
            CML.spawn (fn () => (initLoop init; ()));
            Slider {
              widget = mkWidget {
                root=root,
                boundsOf=bounds_of dim, 
                realize = fn arg => CML.writeVar(realizeVar,arg)
              }, 
              rqstChan = rqstChan,
              repChan = repChan,
              evt = CML.receive valChan
            }
          end

    fun mkHSlider root {foregrnd, wid, init, scale} = 
      mkSlider (root, foregrnd, wid, init, scale, horzSlider)

    fun mkVSlider root {foregrnd, wid, init, scale} = 
      mkSlider (root, foregrnd, wid, init, scale, vertSlider)

    fun widgetOf (Slider{widget,...}) = widget
    fun evtOf (Slider{evt,...}) = evt
    fun setValue (Slider{rqstChan,repChan,...}) v = (
          CML.send(rqstChan, SetValue v);
          case CML.accept repChan of
            Okay => ()
          | Error => LibBase.badArg{module="Slider",func="setValue",msg="improper value"}
        )
    fun getInt msg (Slider{rqstChan,repChan,...}) = let
          val rc = CML.channel ()
          in
            CML.send(rqstChan, msg rc);
            CML.accept rc
          end
    val getScale = getInt GetScale
    val getValue = getInt GetValue

    end (* local *)
  end (* Slider *)
