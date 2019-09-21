namespace Blazor.Formlets.Core

open FSharp.Core.Printf

open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Rendering
open Microsoft.AspNetCore.Components.Web

open System
open System.Text

type [<Struct>] Maybe<'T> =
  | Just      of 'T
  | Nothing
and maybe<'T> = Maybe<'T>

module Maybe =
  let inline just v = Just v
  let nothing<'T>   = Nothing

type FormletContext       = 
  {
    CreateId          : unit -> string
  }

type FormletRenderContext =
  {
    EventReceiver     : obj
    RequestRebuild    : unit -> unit
  }

type [<Struct>] FormletFailureContext =
  | FFC of (string list)

  with
    member x.Append name : FormletFailureContext =
      let (FFC names) = x
      FFC (name::names)

type [<RequireQualifiedAccess>] FormletFailureTree =
  | Empty
  | Failure of FormletFailureContext*string
  | Fork    of FormletFailureTree*FormletFailureTree

  static member Join l r : FormletFailureTree =
    match l, r with
    | Empty , Empty -> Empty
    | _     , Empty -> l
    | Empty , _     -> r
    | _     , _     -> Fork (l, r)

  member x.ContextfulFailures () : struct (string*string) [] =
    let ra = ResizeArray 16
    // TODO: Optimize
    let toContext (FFC vs) =
      System.String.Join ('.', vs |> List.rev |> List.toArray)
    let rec loop t =
      match t with
      | Empty -> ()
      | Failure (ffc, msg)  -> ra.Add (struct (toContext ffc, msg))
      | Fork    (l, r)      -> loop l; loop r
    loop x
    ra.ToArray ()

  member x.Failures () : string [] =
    let ra = ResizeArray 16
    let rec loop t =
      match t with
      | Empty -> ()
      | Failure (ffc, msg)  -> ra.Add msg
      | Fork    (l, r)      -> loop l; loop r
    loop x
    ra.ToArray ()

  static member inline (+++) (l, r) = FormletFailureTree.Join l r

type [<RequireQualifiedAccess>] FormletModel =
  | Empty
  // Mutability?? Personally I see pros/cons and the pros of mutability here is not having to
  //  build lenses to update a nested model value, cons are well known but likely managable
  | Value of string ref 
  | Tag   of string*FormletModel
  | Fork  of FormletModel*FormletModel

  static member inline (+++) (l, r) = Fork (l, r)

type [<RequireQualifiedAccess>] FormletTree = 
  | Empty
  | WithClass         of string*FormletTree
  | WithAttribute     of string*string*FormletTree
  | WithId            of string*FormletTree
  | WithChangeBinding of string ref*FormletTree
  | Element           of string*FormletTree
  | Content           of string
  | Fork              of FormletTree*FormletTree

  static member inline (+++) (l, r) = Fork (l, r)

type [<Struct>] FormletResult<'T> = FR of 'T*FormletFailureTree*FormletModel*FormletTree

type [<Struct>] Formlet<'T> =
  | FL of (FormletContext -> FormletFailureContext -> FormletModel -> FormletResult<'T>)

module Common =
  let inline adapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt t
  let inline invoke (t : OptimizedClosures.FSharpFunc<_, _, _, _>) fc ffc fm = t.Invoke (fc, ffc, fm)

module FormletTrees =
  let empty = FormletTree.Empty

  let inline content            c         = FormletTree.Content           c
  let inline element            e   ft    = FormletTree.Element           (e, ft)
  let inline withAttribute      k v ft    = FormletTree.WithAttribute     (k, v, ft)
  let inline withClass          cls ft    = FormletTree.WithClass         (cls, ft)
  let inline withChangeBinding  rv  ft    = FormletTree.WithChangeBinding (rv, ft)
  let inline withId             id  ft    = FormletTree.WithId            (id, ft)

  let inline button             cls name  = content name |> element "button" |> withClass cls |> withClass "btn"
  let inline div                ft        = element "div" ft 

module Formlet =
  open Common
  module Details =
    module Loops =
      let rec renderAttributes context (builder : RenderTreeBuilder) kvs i =
        match kvs with
        | []                  -> i
        | (struct (k, v))::t  -> 
          builder.AddAttribute (i, k, (v : string))
          renderAttributes context builder t (i + 1)

      let rec renderClass context (builder : RenderTreeBuilder) (sb : StringBuilder) cs i =
        match cs with
        | []    -> 
          if sb.Length > 0 then
            builder.AddAttribute (i, "class", sb.ToString ())
            i + 1
          else
            i
        | h::t  -> 
          if sb.Length > 0 then sb.Append ' ' |> ignore
          sb.Append (h : string) |> ignore
          renderClass context builder sb t i

      let rec renderChangeBinding (context : FormletRenderContext) (builder : RenderTreeBuilder) cbs i =
        match cbs with
        | []  -> i
        | _   ->
          let onChange (e : ChangeEventArgs) =
            let v =
              if isNull e.Value then ""
              else e.Value.ToString ()
            for cb in cbs do
              cb := v
            context.RequestRebuild ()
          let cb = EventCallback.Factory.Create<ChangeEventArgs> (context.EventReceiver, Action<ChangeEventArgs> onChange)
          builder.AddAttribute<ChangeEventArgs> (i, "onchange", cb)
          i + 1

      let rec render oid cs kvs cbs (context : FormletRenderContext) (builder : RenderTreeBuilder) (formletTree : FormletTree) i = 
        match formletTree with
        | FormletTree.Empty                         -> i
        | FormletTree.WithClass         (c, ft)     -> render oid (c::cs) kvs cbs context builder ft i
        | FormletTree.WithAttribute     (k, v, ft)  -> render oid cs (struct (k, v)::kvs) cbs context builder ft i
        | FormletTree.WithId            (id, ft)    -> render (Just id) cs kvs cbs context builder ft i
        | FormletTree.WithChangeBinding (cb, ft)    -> render oid cs kvs (cb::cbs) context builder ft i
        | FormletTree.Element           (e, ft)     ->
          builder.OpenElement (i, e)
          let i = i + 1
          let i = renderAttributes context builder kvs i
          let i = 
            match oid with
            | Just id   -> builder.AddAttribute (i, "id", id); i + 1
            | _         -> i
          let i = renderClass context builder (StringBuilder ()) cs i // TODO: StringBuilder could be shared
          let i = renderChangeBinding context builder cbs i
          let i = render Nothing [] [] [] context builder ft i
          builder.CloseElement ()
          i
        | FormletTree.Content       c             ->
          builder.AddContent (i, c)
          i + 1
        | FormletTree.Fork          (lft, rft)    -> 
          let i = render oid cs kvs cbs context builder lft i
          render Nothing cs kvs cbs context builder rft i

  open Details

  let eval (context : FormletContext) (formlet : Formlet<'T>) (model : FormletModel) =
    let f = adapt formlet
    invoke f context (FFC []) model

  let render (context : FormletRenderContext) (builder : RenderTreeBuilder) (formletTree : FormletTree) sequence : int = 
    Loops.render Nothing [] [] [] context builder formletTree sequence

  let inline value (v : 'T) : Formlet<'T> =
    FL <| fun fc ffc fm ->
      FR (v, FormletFailureTree.Empty, FormletModel.Empty, FormletTree.Empty)

  let inline failWith (fv : 'T) msg : Formlet<'T>  =
    FL <| fun fc ffc fm ->
      FR (fv, FormletFailureTree.Failure (ffc, msg), FormletModel.Empty, FormletTree.Empty)

  let inline failWithf fv fmt = kprintf (failWith fv) fmt

  let inline bind (t : Formlet<'T>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let tfm, ufm =
        match fm with
        | FormletModel.Fork (tfm, ufm)  -> tfm                , ufm
        | _                             -> FormletModel.Empty , FormletModel.Empty

      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc tfm

      let u  = uf tv
      let uf = adapt u

      let (FR (uv, ufft, ufm, uft)) = invoke uf fc ffc ufm

      FR (uv, tfft +++ ufft, tfm +++ ufm, tft +++ uft)

  let inline apply (f : Formlet<'T -> 'U>) (t : Formlet<'T>) : Formlet<'U> =
    let ff = adapt f
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let ffm, tfm =
        match fm with
        | FormletModel.Fork (ffm, tfm)  -> ffm                , tfm
        | _                             -> FormletModel.Empty , FormletModel.Empty

      let (FR (ff, ffft, ffm, fft)) = invoke ff fc ffc ffm
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc tfm

      FR (ff tv, ffft +++ tfft, ffm +++ tfm, fft +++ tft)

  let inline map m (t : Formlet<'T>) : Formlet<'U> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      FR (m tv, tfft, tfm, tft)

  let inline andAlso (t: Formlet<'T>) (u : Formlet<'U>) : Formlet<'T*'U> =
    let tf = adapt t
    let uf = adapt u
    FL <| fun fc ffc fm ->
      let tfm, ufm =
        match fm with
        | FormletModel.Fork (tfm, ufm)  -> tfm                , ufm
        | _                             -> FormletModel.Empty , FormletModel.Empty

      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc tfm
      let (FR (uv, ufft, ufm, uft)) = invoke uf fc ffc ufm

      FR ((tv, uv), tfft +++ ufft, tfm +++ ufm, tft +++ uft)

  let inline unwrap (t : Formlet<Formlet<'T>>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let tfm, ufm =
        match fm with
        | FormletModel.Fork (tfm, ufm)  -> tfm                , ufm
        | _                             -> FormletModel.Empty , FormletModel.Empty

      let (FR (u, tfft, tfm, tft)) = invoke tf fc ffc tfm

      let uf = adapt u

      let (FR (uv, ufft, ufm, uft)) = invoke uf fc ffc ufm

      FR (uv, tfft +++ ufft, tfm +++ ufm, tft +++ uft)

  let inline tag nm (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let fm =
        match fm with
        | FormletModel.Tag (snm, sft) when snm.Equals nm  -> sft
        | _                                               -> FormletModel.Empty

      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      FR (tv, tfft, FormletModel.Tag (nm, tfm), tft)

type Formlet<'T> with
  static member inline (>>=) (t, uf)  = Formlet.bind    t uf
  static member inline (|>>) (t, m)   = Formlet.map     m t
  static member inline (<*>) (f, t)   = Formlet.apply   f t
  static member inline (<&>) (l, r)   = Formlet.andAlso l r


module Builder =
  module Details =
    open Formlet
    type Builder () =
      member inline x.Bind       (t, uf)   = bind t uf
      member inline x.Return     v         = value v
      member inline x.ReturnFrom t         = t         : Formlet<_>
      member inline x.Zero       ()        = value ()
  open Details
  let formlet = Builder ()

module Inputs =
  open FormletTrees

  let text placeholder initial : Formlet<string> =
    FL <| fun fc ffc fm ->
      let rv =
        match fm with
        | FormletModel.Value rv -> rv
        | _                     -> ref initial

      let ft =
        empty 
        |> element            "input" 
        |> withAttribute      "type"          "text" 
        |> withAttribute      "value"         !rv 
        |> withAttribute      "placeholder"   placeholder
        |> withClass          "form-control"
        |> withChangeBinding  rv

      FR (!rv, FormletFailureTree.Empty, FormletModel.Value rv, ft)


module Components =
  type [<AbstractClass>] FormletComponent () =
    inherit ComponentBase ()

  type [<AbstractClass>] FormletComponent<'T> () =
    inherit FormletComponent ()

    let mutable formletModel        = FormletModel.Empty
    let mutable formletFailureTree  = FormletFailureTree.Empty
    let mutable formletTree         = FormletTree.Empty

    abstract Formlet : Formlet<'T>

    override x.BuildRenderTree builder =
      let id = ref 0

      let evalContext : FormletContext =
        {
          CreateId        = fun () -> 
            let r = sprintf "fid_#%d" !id
            incr id
            r
        }

      let (FR (v, fft, fm, ft)) = Formlet.eval evalContext x.Formlet formletModel

      formletFailureTree  <- fft
      formletModel        <- fm
      formletTree         <- ft

      let renderContext : FormletRenderContext = 
        {
          EventReceiver   = x
          RequestRebuild  = fun () -> ()  // TODO:
        }
      let i = Formlet.render renderContext builder formletTree 0
      
      ()

module Validate =
  open Common
  open System.Text.RegularExpressions

  let validate (validator : 'T -> string maybe) (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc ft ->
      let tfr = invoke tf fc ffc ft
      let (FR (tv, tfft, tfm , tft)) = tfr

      match validator tv with
      | Just failure  ->
        FR (tv, FormletFailureTree.Join tfft (FormletFailureTree.Failure (ffc, failure)), tfm, tft)
      | Nothing       ->
        tfr

  let notEmpty (t : Formlet<string>) : Formlet<string> =
    let validator (v : string) =
      if v.Length > 0 then
        Nothing
      else
        Just "Must not be empty"
    validate validator t

  let regex (test : Regex) failWith (t : Formlet<string>) : Formlet<string> =
    let validator (v : string) =
      if test.IsMatch v then
        Nothing
      else
        Just failWith
    validate validator t

module Surround =
  open Common
  open FormletTrees

  let withDiv cls (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let tft = tft |> div |> withClass cls

      FR (tv, tfft, tfm , tft)

  let withBox (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let content = tft |> div |> withClass "card-body" 
      let tft     = content |> div |> withClass "card mb-3" 

      FR (tv, tfft, tfm , tft)

  let withNamedBox name (t : Formlet<'T>) : Formlet<'T> =
    let tf      = adapt t
    let header  = content name |> div |> withClass "card-header"

    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let content = tft |> div |> withClass "card-body" 
      let tft     = (header +++ content) |> div |> withClass "card mb-3" 

      FR (tv, tfft, tfm , tft)

  let withSubmit name (t : Formlet<'T>) : Formlet<'T> =
    let tf      = adapt t


    let submitButton  = button "btn-dark"     "Submit"
    let resetButton   = button "btn-warning"  "Reset"

    let legendHeader  = content name |> div |> withClass "card-header"
    let legendContent = (submitButton +++ resetButton) |> div |> withClass "card-body"
    let legend        = (legendHeader +++ legendContent) |> div |> withClass "card mb-3"

    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let tft = legend +++ tft

      FR (tv, tfft, tfm , tft)

module Enhance =
  open Common
  open FormletTrees

  let withForm (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let tft = element "form" tft

      FR (tv, tfft, tfm , tft)

  let withFormGroup (t : Formlet<'T>) : Formlet<'T> =
    Surround.withDiv "form-group" t

  let withLabel lbl (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let id = fc.CreateId ()

      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let lft = content lbl |> element "label" |> withAttribute "for" id

      let tft = lft +++ (withId id tft)

      FR (tv, tfft, tfm , tft)

  let withValidation (t : Formlet<'T>) : Formlet<'T> =
    let tf = adapt t
    FL <| fun fc ffc fm ->
      let (FR (tv, tfft, tfm, tft)) = invoke tf fc ffc fm

      let tft =
        match tfft with
        | FormletFailureTree.Empty  -> tft |> withClass "is-valid" 
        | _                         -> 
          let sb = StringBuilder 16
          for f in tfft.Failures () do
            if sb.Length > 0 then
              sb.Append "; " |> ignore
            sb.Append f |> ignore
          let msg = sb.ToString ()

          let lft = tft |> withClass "is-invalid"
          let rft = content msg |> div |> withClass "invalid-feedback"
          lft +++ rft

      FR (tv, tfft, tfm , tft)
