namespace Blazor.Formlet

module Core =
  open FSharp.Core.Printf
  open Microsoft.AspNetCore.Components
  open Microsoft.AspNetCore.Components.Rendering
  open System
  open System.Collections.Generic
  open System.Text

  type FormletEvalContext   = FEC of unit
  type FormletRenderContext = 
    {
      EventReceiver     : obj
      RequestRebuild    : unit -> unit
    }
  type FormletPath          = string list

  type [<RequireQualifiedAccess>] FormletState =
    | Empty
    | Value         of string ref
    | Tag           of string*FormletState
    | Fork          of FormletState*FormletState
    static member inline (+++) (l, r) = Fork (l, r)
    static member inline (!>)  x      =
      match x with
      | Fork (l, r) -> l, r
      | _           -> Empty, Empty

  type [<RequireQualifiedAccess>] FormletError =
    | Empty
    | Message       of FormletPath*string
    | Fork          of FormletError*FormletError

    member x.Fold f z =
      let f = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt f
      let rec loop f s i t =
        match t with
        | Empty               -> s, i
        | Message (fp, msg)   -> 
          let s = (f : OptimizedClosures.FSharpFunc<_, _, _, _, _>).Invoke (s, i, fp, msg)
          s, i + 1
        | Fork    (l, r)      -> 
          let s, i = (loop f s i l)
          loop f s i r
      let s, _ = loop f z 0 x
      s

    static member inline (+++) (l, r) = 
      match l, r with
      | Empty , _     -> r
      | _     , Empty -> l
      | _     , _     -> Fork (l, r)
    static member inline (!!!) x =
      match x with
      | Empty -> false
      | _     -> true

  type [<RequireQualifiedAccess>] FormletView =
    | Empty
    | Content           of int*string
    | Element           of int*string*FormletView
    | WithAttribute     of int*string*string*FormletView
    | WithClass         of string*FormletView
    | WithId            of string*FormletView
    | WithChangeBinding of string ref*FormletView
    | Fork              of FormletView*FormletView
    | Group             of int*FormletView array
    static member inline (+++) (l, r) = Fork (l, r)

  module FormletViews =
        
    // +1, +2, +3 for the implcit id, class and change attribute on all elements
    let elementRange                  = 4
    let divRange                      = 4
    let spanRange                     = 4
    let buttonRange                   = 6

    let empty                         = FormletView.Empty
    let content           sno c       = FormletView.Content           (sno, c)
    let element           sno e   fv  = FormletView.Element           (sno, e, fv)
    let group             sno     fvs = FormletView.Group             (sno, fvs)
    let withAttribute     sno k v fv  = FormletView.WithAttribute     (sno, k, v, fv)
    let withClass             cls fv  = FormletView.WithClass         (cls, fv)
    let withChangeBinding     rv  fv  = FormletView.WithChangeBinding (rv, fv)
    let withId                id  fv  = FormletView.WithId            (id , fv)


    let div               sno     fv  = element sno "div"  fv
    let span              sno     fv  = element sno "span" fv

    let button            sno cls nm  = 
      content           (sno + 0 + elementRange)  nm 
      |> element        sno                       "button" 
      |> withAttribute  (sno + 1 + elementRange)  "style"   "margin-right: 8px" 
      |> withClass                                cls 
      |> withClass                                "btn"


  type [<Struct>] FormletMapView = FMV of int*(FormletView -> FormletView)

  type [<Struct>] FormletResult<'T> = FR of 'T*FormletError*FormletState*FormletView

  type [<Struct>] Formlet<'T> = FT of int*(FormletEvalContext -> FormletPath -> FormletState -> int -> FormletResult<'T>)

  module Details =
    let inline adaptfmv  (FMV (r,f))  = r, f
    let inline invokefmv f x          = f x
    let inline adaptft  (FT (i,f))    = i, OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt f
    let inline invokeft f x y z w     = (f : OptimizedClosures.FSharpFunc<_, _, _, _, _>).Invoke (x, y, z, w)

    open Microsoft.AspNetCore.Components
    open Microsoft.AspNetCore.Components.Rendering
    open Microsoft.AspNetCore.Components.Web

    type GroupComponent ()=
      class
        inherit ComponentBase ()

        override x.BuildRenderTree builder =
          ()
      end

  open Details

  module Formlet =
    open Log

    let inline ft   r f   = FT  (r, f)
    let inline fmv   r f  = FMV (r, f)
    let value v = 
      ft 0 <| fun fc fp fs sno ->
        FR (v, FormletError.Empty, FormletState.Empty, FormletView.Empty)

    let error fv msg = 
      ft 0 <| fun fc fp fs sno ->
        FR (fv, FormletError.Message (fp, msg), FormletState.Empty, FormletView.Empty)

    let errorf fv fmt = kprintf (error fv) fmt

    let eval fc f fs =
      let fr, f = adaptft f
      fr, invokeft f fc [] fs 0

    let render frc (rtb : RenderTreeBuilder) fv =
      let sb = StringBuilder ()

#if DEBUG
      let seen = Dictionary<_, _> ()

      let mutable section = [0]

      let log sno msg =
        infof "%d - %s" sno msg
        if sno > -1 then
          let sno = sno::section
          let b, vs = seen.TryGetValue sno
          let vs =
            if b then
              vs
            else
              let vs = ResizeArray ()
              seen.Add (sno, vs)
              vs
          vs.Add msg
      let logf sno fmt = kprintf (log sno) fmt

      let renderAttribute sno k v = 
        logf sno "render.attribute: %A, %A" k v
        rtb.AddAttribute (sno, k, (v : string))

      let renderOnChangeAttribute sno cb = 
        log sno "render.attribute: 'onchange'"
        rtb.AddAttribute<ChangeEventArgs> (sno, "onchange", cb)

      let renderContent sno c = 
        logf sno "render.content: %A" c
        rtb.AddContent (sno, (c : string))

      let renderOpenElement sno e = 
        logf sno "render.openElement: %A" e
        rtb.OpenElement (sno, e)

      let renderCloseElement () =
        logf -1 "render.closeElement:"
        rtb.CloseElement ()

      let renderOpenRegion sno = 
        logf sno "render.openRegion: %d" sno
        section <- sno::section
        rtb.OpenRegion sno

      let renderCloseRegion () =
        logf -1 "render.closeRegion:"
        section <- section |> List.tail
        rtb.CloseRegion ()
#else
      let inline renderAttribute sno k v = 
        rtb.AddAttribute (sno, k, (v : string))

      let inline renderOnChangeAttribute sno cb = 
        rtb.AddAttribute<ChangeEventArgs> (sno, "onchange", cb)

      let inline renderContent sno c = 
        rtb.AddContent (sno, (c : string))

      let inline renderOpenElement sno e = 
        rtb.OpenElement (sno, e)

      let inline renderCloseElement () =
        rtb.CloseElement ()

      let inline renderOpenRegion sno = 
        rtb.OpenRegion sno

      let inline renderCloseRegion () =
        rtb.CloseRegion ()
#endif

      let rec renderClass sno cs =
        match cs with
        | []    -> 
          if sb.Length > 0 then 
            renderAttribute sno "class" <| sb.ToString ()
            sb.Clear () |> ignore
        | h::t  -> 
          if sb.Length > 0 then sb.Append ' ' |> ignore
          sb.Append (h : string) |> ignore
          renderClass sno t

      let rec renderChangeBinding sno cbs =
        match cbs with
        | []  -> ()
        | _   ->
          let onChange (e : ChangeEventArgs) =
            let v =
              if isNull e.Value then ""
              else e.Value.ToString ()
            for cb in cbs do
              cb := v
            frc.RequestRebuild ()
          let cb = EventCallback.Factory.Create<ChangeEventArgs> (frc.EventReceiver, Action<ChangeEventArgs> onChange)
          renderOnChangeAttribute sno cb

      let rec recurse oid cs kvs cbs fv =
        let renderAttributes sno =
          match oid with
          | ValueSome id  -> renderAttribute (sno + 1) "id" id
          | _             -> ()

          renderClass         (sno + 2)  cs
          
          renderChangeBinding (sno + 3) cbs

          for struct (sno, k, v) in kvs do
            renderAttribute sno k v
        match fv with
        | FormletView.Empty                             -> ()
        | FormletView.Content           (sno, c)        -> renderContent sno c
        | FormletView.WithAttribute     (sno, k, v, fv) -> recurse oid cs (struct (sno, k, v)::kvs) cbs fv
        | FormletView.WithClass         (c, fv)         -> recurse oid (c::cs) kvs cbs fv
        | FormletView.WithId            (id, fv)        -> recurse (ValueSome id) cs kvs cbs fv
        | FormletView.WithChangeBinding (cb, fv)        -> recurse oid cs kvs (cb::cbs) fv
        | FormletView.Fork              (lfv, rfv)      ->
          recurse oid cs kvs cbs lfv
          recurse ValueNone cs kvs cbs rfv
        | FormletView.Element           (sno, e, fv)    ->
          renderOpenElement sno e
          renderAttributes sno
          recurse ValueNone [] [] [] fv
          renderCloseElement ()
        | FormletView.Group           (sno, fvs)        ->
          if fvs.Length > 0  then
            renderOpenRegion sno
            let fv = fvs.[0]
            recurse oid cs kvs cbs fv
            for i = 1 to fvs.Length - 1 do
              let fv = fvs.[i]
              recurse ValueNone cs kvs cbs fv
            renderCloseRegion ()
      recurse ValueNone [] [] [] fv

#if DEBUG
      for kv in seen do
        let k = kv.Key
        let v = kv.Value |> Seq.toArray
        if v.Length > 1 then
          Log.errorf "Duplicate sequence number detected: %A - %A" k v
#endif

    let apply s f =
      let fr, f = adaptft f
      let sr, s = adaptft s
      ft (fr + sr) <| fun fc fp fs sno ->
        let ffs, sfs = !> fs

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp ffs sno
        let (FR (sv, sfe, sfs, sfv)) = invokeft s fc fp sfs (sno + fr)

        FR (fv sv, ffe +++ sfe, ffs +++ sfs, ffv +++ sfv)

    let andAlso s f =
      let fr, f = adaptft f
      let sr, s = adaptft s
      ft (fr + sr) <| fun fc fp fs sno ->
        let ffs, sfs = !> fs

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp ffs sno
        let (FR (sv, sfe, sfs, sfv)) = invokeft s fc fp sfs (sno + fr)

        FR ((fv, sv), ffe +++ sfe, ffs +++ sfs, ffv +++ sfv)

    let combine s f =
      let fr, f = adaptft f
      let sr, s = adaptft s
      ft (fr + sr) <| fun fc fp fs sno ->
        let ffs, sfs = !> fs

        let (FR (_ , ffe, ffs, ffv)) = invokeft f fc fp ffs sno
        let (FR (sv, sfe, sfs, sfv)) = invokeft s fc fp sfs (sno + fr)

        FR (sv, ffe +++ sfe, ffs +++ sfs, ffv +++ sfv)

    let debug nm f =
      let fr, f = adaptft f
      ft fr <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) as fr = invokeft f fc fp fs sno
        infof "Formlet.debug - %s - sno:%d - fp:%A - %A" nm sno fp fv
        fr

    let tag nm f =
      let fr, f = adaptft f
      ft fr <| fun fc fp fs sno ->
        let fs =
          match fs with
          | FormletState.Tag (tnm, fs) when tnm = nm  -> fs
          | _                                         -> FormletState.Empty

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs sno

        FR (fv, ffe, FormletState.Tag (nm, ffs), ffv)

    let map m f =
      let fr, f = adaptft f
      ft fr <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs sno

        FR (m fv, ffe, ffs, ffv)

    let mapView m f =
      let mr, m = adaptfmv m
      let fr, f = adaptft f
      ft (mr + fr) <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs sno

        FR (fv, ffe, ffs, m ffv)

    let choose ss f =
      let fr, f = adaptft f
      let mf s v =
        let ir, is = adaptft v
        (ir, s, is), (ir + s)
      let ss, sr = ss |> Array.mapFold mf 0
      ft (fr + sr) <| fun fc fp fs sno ->
        let ffs, sfs = !> fs

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp ffs sno

        if fv >= 0 && fv < ss.Length then
          let _, sroff, s = ss.[fv]
          let (FR (sv, sfe, sfs, sfv)) = invokeft s fc fp sfs (sno + fr + sroff)

          FR (sv, ffe +++ sfe, ffs +++ sfs, ffv +++ sfv)

        else
          failwithf "choose: Index out of range: %d not in [0..%d[" fv ss.Length


    let validate v f =
      let fr, f = adaptft f
      ft fr <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) as fres = invokeft f fc fp fs sno

        match v fv with
        | None    -> fres
        | Some m  -> FR (fv, ffe +++ (FormletError.Message (fp, m)), ffs, ffv)

  module Validate =
    open System.Text.RegularExpressions

    let notEmpty f = 
      let v s = 
        if String.IsNullOrEmpty s then Some "Input must not be empty" else None
      f |> Formlet.validate v

    let fromRegex (r : Regex) msg f = 
      let v s =
        if r.IsMatch s then None else Some msg
      f |> Formlet.validate v

    let regex r msg f = 
      let r = Regex (r, RegexOptions.Compiled)
      f |> fromRegex r msg

  module Components =
    open Log

    open Microsoft.AspNetCore.Components
    open Microsoft.AspNetCore.Components.Rendering
    open Microsoft.AspNetCore.Components.Web

    type [<AbstractClass>] FormletComponent () =
      inherit ComponentBase ()

    type [<AbstractClass>] FormletComponent<'T> () =
      inherit FormletComponent ()

      let mutable formletError  = FormletError.Empty
      let mutable formletState  = FormletState.Empty
      let mutable formletView   = FormletView.Empty

      abstract Formlet : Formlet<'T>

      override x.BuildRenderTree builder =
#if DEBUG
        hilight "FormletComponent.BuildRenderTree"

        info "FormletComponent.BuildRenderTree.eval"
#endif

        let fc = FEC ()
        let (r, FR (_, fe, fs, fv)) = Formlet.eval fc x.Formlet formletState

#if DEBUG
        infof "Error:\n%A" fe
        infof "State:\n%A" fs
        infof "View :\n%A" fv
#endif

        formletError  <- fe
        formletState  <- fs
        formletView   <- fv

        let frc : FormletRenderContext =
          {
            EventReceiver   = x
            RequestRebuild  = fun () -> ()  // TODO:
          }
         
#if DEBUG
        info "FormletComponent.BuildRenderTree.render"
#endif
        Formlet.render frc builder formletView

  type Formlet<'T> with
    static member inline (<&>) (f, s) = Formlet.andAlso   s f
    static member inline (<*>) (f, s) = Formlet.apply     s f
    static member inline (>>.) (f, s) = Formlet.combine   s f
    static member inline (|>>) (f, m) = Formlet.map       m f

  module Input =
    open Formlet
    open FormletViews

    let text placeholder initial : Formlet<string> =
      let er = 3
      ft (er + elementRange) <| fun fc fp fs sno ->
        let rv =
          match fs with
          | FormletState.Value rv -> rv
          | _                     -> ref initial

        let fv =
          empty 
          |> element            sno                       "input" 
          |> withClass                                    "form-control"
          |> withChangeBinding                            rv
          |> withAttribute      (sno + 0 + elementRange)  "type"          "text" 
          |> withAttribute      (sno + 1 + elementRange)  "value"         !rv 
          |> withAttribute      (sno + 2 + elementRange)  "placeholder"   placeholder

        FR (!rv, FormletError.Empty, FormletState.Value rv, fv)

    let select (options : (string*'T) []) : Formlet<'T> =
      if options.Length <= 0 then
        failwith "select - expected at least one choice"

      let content = 
        let mapper i (k, _) = content (2*i + 1) k |> element (2*i) "option"
        options
        |> Array.mapi mapper

      let sr = elementRange + 2
      ft sr <| fun fc fp fs sno ->
        let rv =
          match fs with
          | FormletState.Value rv -> rv
          | _                     -> ref (fst options.[0])

        let fs = FormletState.Value rv

        let index = options |> Array.tryFindIndex (fun (k, _) -> k = !rv) |> Option.defaultValue 0

        let content = content |> group (sno + 1 + elementRange)

        let fv =
          content
          |> element            sno                       "select" 
          |> withAttribute      (sno + 0 + elementRange)  "selectedIndex" (string index)
          |> withClass                                    "form-control"
          |> withChangeBinding                            rv

        if index >= 0 && index < options.Length then
          FR (snd options.[index], FormletError.Empty, fs, fv)
        else
          FR (snd options.[0], FormletError.Message (fp, "No value selected"), fs, fv)

    let selectFormlet (options : (string*Formlet<'T>) []) : Formlet<'T> =
      let opts    = options |> Array.mapi (fun i v -> fst v, i)
      let choices = options |> Array.map snd
      select opts |> choose choices

  module Enhance =
    open Formlet
    open FormletViews

    let withLabel lbl f =
      let lr = elementRange + 2
      let fr, f = adaptft f
      ft (fr + lr) <| fun fc fp fs sno ->
        let fp = lbl::fp
        let id = sprintf "lbl-%d" sno

        let lfv = 
          content (sno + 1 + elementRange) lbl 
          |> element sno "label" 
          |> withAttribute (sno + 0 + elementRange) "for" id

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + lr)

        let ffv = lfv +++ (withId id ffv)

        FR (fv, ffe, ffs, ffv)

    let withForm f =
      let lr = elementRange
      let fr, f = adaptft f
      ft (fr + lr) <| fun fc fp fs sno ->

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + lr)

        let ffv = 
          ffv
          |> element sno "form" 

        FR (fv, ffe, ffs, ffv)
