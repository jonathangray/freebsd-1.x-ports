(* toggle-ctrl.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Protocol for toggles.
 *)

signature TOGGLE_CTRL = 
  sig

    structure CML : CONCUR_ML
    structure W : WIDGET
    structure Interact : INTERACT
    structure BV : BUTTON_VIEW

    type toggle

    val mkToggle : W.root -> 
          (bool -> unit) * W.wstate * BV.button_view -> toggle
    val getState : toggle -> bool
    val setState : (toggle * bool) -> unit
    val widgetOf : toggle -> W.widget
    val setActive : (toggle * bool) -> unit
    val getActive : toggle -> bool

  end (* TOGGLE_CTRL *)

structure ToggleCtrl : TOGGLE_CTRL = struct

  structure W = Widget
  structure Interact = Interact
  structure CML = CML
  structure BV = ButtonView

  open CML Geometry EXeneBase EXeneWin Interact Widget ButtonView

  datatype req_msg = 
    GetState of bool chan
  | SetState of bool
  | GetActive of bool chan
  | SetActive of bool
  | DoRealize of {
      env : in_env,
      win : window,
      sz : size
    }

  datatype toggle = Toggle of {
    widget : Widget.widget,
    rqst : req_msg CML.chan
  }

  datatype button_act = 
    BtnDown
  | BtnUp
  | BtnLeft

  fun mseP (m, mchan) = let

    fun waitLoop () =
      case msgBodyOf (sync m) of 
        MOUSE_LastUp {but,...} => ()
      | _ => waitLoop () 

    fun downLoop () =
      case msgBodyOf (sync m) of 
        MOUSE_LastUp {but,...} => send (mchan, BtnUp)
      | MOUSE_Leave => (send (mchan, BtnLeft); waitLoop ())
      | _ => downLoop () 

    fun loop () =
      case msgBodyOf (sync m) of 
        MOUSE_FirstDown {but,...} => (
          send (mchan, BtnDown);
          downLoop ();
          loop ()
        )
      | _ => loop ()
  in
    loop ()
  end

  fun mkToggle root 
    (action : bool -> unit, state, {bounds_of,config} : button_view) = let
    val reqChan = channel ()

    fun set_state (state,arg,setf) = let
            val state' = setState (arg,state)
            in
              if state <> state' then (setf state'; action (getState state'))
              else ();
              state'
            end

    fun realizeToggle {env=inenv, win, sz} me = let
      val w = win
      val InEnv{m,ci,...} = Interact.ignoreKey inenv
      val mchan = channel ()
      val config = config w

      fun flipState (state,setf) =
        let
          val state' = flip state
        in
          setf state';
          (state', setf)
        end

      fun handleM (BtnDown,me) = flipState me
        | handleM (BtnLeft,me) = flipState me
        | handleM (BtnUp,me as (state,_)) = (action (getState state); me)
      
      fun handleReq (GetState retc,(state,_)) = 
            (send (retc, getState state); state)
        | handleReq (SetState arg,(state,setf)) = set_state(state,arg,setf)
        | handleReq (GetActive retc,(state,_)) = 
            (send (retc, getActive state); state)
        | handleReq (SetActive arg,(state,setf)) =
            let
              val state' = setActive (arg,state)
            in
              if state <> state' then setf state' else ();
              state'
            end
        | handleReq (_,(state,_)) = state

      fun handleCI (CI_Redraw _, me as (state,setf)) = (setf state; me)
        | handleCI (CI_Resize (RECT{wid,ht,...}), (state,_)) = 
            (state, config (SIZE{wid=wid,ht=ht}))
        | handleCI (_,me) = me

      fun cmdP (me as (state,setf)) =
        select [
          wrap(receive mchan, fn evt => cmdP(handleM (evt,me))),
          wrap(ci, fn evt => cmdP(handleCI (msgBodyOf evt,me))),
          wrap(receive reqChan, fn evt => 
            let val state' = handleReq (evt,me) in
              if getActive state' then cmdP (state',setf)
              else inactiveCmdP (state',setf)
            end
          )
        ]
      and inactiveCmdP (me as (state,setf)) =
        select [
          wrap(receive mchan, fn evt => inactiveCmdP me),
          wrap(ci, fn evt => inactiveCmdP(handleCI (msgBodyOf evt,me))),
          wrap(receive reqChan, fn evt => 
            let val state' = handleReq (evt,me) in
              if getActive state' then cmdP (state',setf)
              else inactiveCmdP (state',setf)
            end
          )
        ]
    in
      spawn (fn () => mseP(m,mchan));
      if getActive me then cmdP(me,config sz)
      else inactiveCmdP(me,config sz)
    end

    fun initP me =
      case accept reqChan of
        GetState retc => (send (retc, getState me); initP me)
      | SetState arg => initP (set_state (me,arg,fn _ => ()))
      | GetActive retc => (send (retc, getActive me); initP me)
      | SetActive arg => initP (setActive (arg,me))
      | DoRealize arg => realizeToggle arg me
  in
    spawn (fn () => initP state);
    Toggle {
      widget = mkWidget{
        root=root,
        boundsOf = bounds_of,
        realize = fn arg => send(reqChan,DoRealize arg)
      },
      rqst = reqChan
    }
  end

  fun widgetOf (Toggle{widget,...}) = widget
  fun setState (Toggle{rqst,...}, arg) = send(rqst, SetState arg)
  fun getState (Toggle{rqst,...}) = let
    val ret = channel ()
  in
    send(rqst, GetState ret);
    accept ret
  end
  fun setActive (Toggle{rqst,...}, arg) = send(rqst, SetActive arg)
  fun getActive (Toggle{rqst,...}) = let
    val ret = channel ()
  in
    send(rqst, GetActive ret);
    accept ret
  end

end (* ToggleCtrl *)
