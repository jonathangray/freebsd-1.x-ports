(* button-ctrl.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Protocol and various controllers for buttons.
 *)

signature BUTTON_CTRL = 
  sig

    structure CML : CONCUR_ML
    structure W : WIDGET
    structure Interact : INTERACT
    structure BV : BUTTON_VIEW

    datatype button_act = 
      BtnDown of Interact.mbutton 
    | BtnUp of Interact.mbutton
    | BtnExit

    type button

    val mkButton : W.root -> bool * BV.button_view -> button
    val evtOf : button -> button_act CML.event
    val widgetOf : button -> W.widget
    val setActive : (button * bool) -> unit
    val getActive : button -> bool

    val mkCommandBtn : W.root -> 
      (Interact.mbutton -> unit) * BV.button_view -> button

  end (* BUTTON_CTRL *)

structure ButtonCtrl : BUTTON_CTRL = struct

  structure W = Widget
  structure Interact = Interact
  structure CML = CML
  structure BV = ButtonView

  open CML Geometry EXeneBase EXeneWin Interact Widget ButtonView

  datatype button_act = 
    BtnDown of Interact.mbutton 
  | BtnUp of Interact.mbutton
  | BtnExit

  datatype req_msg = 
    GetActive of bool chan
  | SetActive of bool
  | DoRealize of {
      env : in_env,
      win : window,
      sz : size
    }

  datatype button = Button of {
    widget : Widget.widget,
    rqst : req_msg CML.chan,
    evt : button_act CML.event
  }

  fun mseP (m, mchan) = let

    fun waitLoop () =
      case msgBodyOf (sync m) of 
        MOUSE_LastUp _ => ()
      | _ => waitLoop () 

    fun downLoop btn =
      case msgBodyOf (sync m) of 
        MOUSE_LastUp _ => send (mchan, BtnUp btn)
      | MOUSE_Leave => (send (mchan, BtnExit); waitLoop ())
      | _ => downLoop btn 

    fun loop () =
      case msgBodyOf (sync m) of 
        MOUSE_FirstDown {but,...} => (
          send (mchan, BtnDown but);
          downLoop but;
          loop ()
        )
      | _ => loop ()
  in
    loop ()
  end

  fun mkButton root (isCont, {bounds_of,config} : button_view) = let
    val evtChan = channel ()
    val rqstChan = channel ()

    fun realizeButton {env=inenv, win, sz} state = let
      val InEnv{m,ci,...} = Interact.ignoreKey inenv
      val mchan = channel ()
      val config = config win

      fun handleCI (CI_Redraw _, me as (state,setf)) = (setf state; me)
        | handleCI (CI_Resize (RECT{wid,ht,...}), (state,_)) = 
            (state, config (SIZE{wid=wid,ht=ht}))
        | handleCI (_,me) = me

      fun handleReq (GetActive retc,(state,_)) = 
          (send (retc, getActive state); state)
        | handleReq (SetActive arg,(state,setf)) = 
            let
              val state' = setActive (arg,state)
            in
              if state <> state' then setf state' else ();
              state'
            end
        | handleReq (_,(state,_)) = state

      fun handleM (BtnDown btn,(state,setf)) = let
        fun endMouse (evt,(state,setf)) =  let
          val state' = flip state
        in
          setf state';
          send (evtChan, evt);
          (state', setf)
        end

        fun mkEvtList (retf,me as (state,setf)) = [
            wrap(receive rqstChan, fn evt =>
              let val state' = handleReq (evt,me) in
                if state' = state then retf me
                else inactiveCmdP (endMouse(BtnExit,(state',setf)))
              end
            ),
            wrap(receive mchan, fn evt => activeCmdP(endMouse (evt,me))),
            wrap(ci, fn evt => retf(handleCI (msgBodyOf evt,me)))
          ]

        fun oneWait me = select (mkEvtList(oneWait,me))
        fun contWait me = select (
            (wrap(transmit(evtChan,BtnDown btn), fn () => contWait me))::
            (mkEvtList(contWait,me))
          )

        val state' = flip state
      in
        setf state';
        send (evtChan, BtnDown btn);
        if isCont then contWait (state', setf)
        else oneWait (state', setf)
      end
        | handleM (_,me) = activeCmdP me   (* impossible *)

      and activeCmdP (me as (state,setf)) =
        select [
          wrap(receive rqstChan, fn evt =>
            let val state' = handleReq (evt,me) in
              if state' = state then activeCmdP me
              else inactiveCmdP (state',setf)
            end
          ),
          wrap(receive mchan, fn evt => handleM (evt,me)),
          wrap(ci, fn evt => activeCmdP(handleCI (msgBodyOf evt,me)))
        ]
      and inactiveCmdP (me as (state,setf)) =
        select [
          wrap(receive rqstChan, fn evt => 
            let val state' = handleReq (evt,me) in
              if state' = state then inactiveCmdP me
              else activeCmdP (state',setf)
            end
          ),
          wrap(receive mchan, fn _ => inactiveCmdP me),
          wrap(ci, fn evt => inactiveCmdP(handleCI (msgBodyOf evt,me)))
        ]
    in
      spawn (fn () => mseP(m,mchan));
      if getActive state then activeCmdP(state,config sz)
      else inactiveCmdP(state,config sz)
    end

    fun initP me =
      case accept rqstChan of
        GetActive retc => (send (retc, getActive me); initP me)
      | SetActive arg => initP (setActive (arg,me))
      | DoRealize arg => realizeButton arg me
  in
    spawn (fn () => initP(Active false));
    Button {
      widget = mkWidget{
        root=root,
        boundsOf = bounds_of,
        realize = fn arg => send(rqstChan,DoRealize arg)
      },
      rqst = rqstChan,
      evt = receive evtChan
    }
  end

  fun widgetOf (Button{widget,...}) = widget
  fun evtOf (Button{evt,...}) = evt
  fun setActive (Button{rqst,...}, arg) = send(rqst, SetActive arg)
  fun getActive (Button{rqst,...}) = let
    val ret = channel ()
  in
    send(rqst, GetActive ret);
    accept ret
  end

  fun mkCommandBtn root (action : mbutton -> unit, bview) = 
  let
    val button = mkButton root (false, bview) 
    val evt = evtOf button
    fun listener () =
      listener (case sync evt of
        BtnUp btn => action btn
      | _ => ()
      )
  in
    spawn listener;
    button
  end

end (* ButtonCtrl *)
