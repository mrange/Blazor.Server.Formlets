namespace Blazor.Formlet

module Bootstrap =
  open Core
  open Core.Details
  open Formlet
  open FormletViews

  open System.Text

  module Enhance =   
    let withFormGroup f =
      let lr = elementRange
      let fr, f = adaptft f
      ft (fr + lr) <| fun fc fp fs sno ->

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + lr)

        let ffv = 
          ffv
          |> element sno "div" 
          |> withClass "form-group"

        FR (fv, ffe, ffs, ffv)

    let inline withValidation f =
      let fr, f = adaptft f
      let vr = elementRange + 1
      ft (fr + vr) <| fun fc fp fs sno ->

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs sno

        let ffv =
          match ffe with
          | FormletError.Empty  -> ffv |> withClass "is-valid" 
          | _                   -> 
            let sb = StringBuilder 16
            let folder (sb : StringBuilder) _ msg =
              if sb.Length > 0 then
                sb.Append "; " |> ignore
              sb.Append (msg : string) |> ignore
              sb
            ffe.Fold folder sb |> ignore
            let msg = sb.ToString ()
            let lfv = 
              ffv 
              |> withClass "is-invalid"

            let sno = sno + fr

            let rfv = 
              content (sno + 0 + elementRange) msg 
              |> element sno "div"
              |> withClass "invalid-feedback"
            lfv +++ rfv

        FR (fv, ffe, ffs, ffv)

    let withBox f =
      let fr, f = adaptft f
      let cr = elementRange
      let br = elementRange

      ft (fr + cr + br) <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + cr + br)        

        let bsno = sno + cr

        let body    = ffv |> element bsno "div" |> withClass "card-body" 
        let ffv     = body |> element sno "div" |> withClass "card mb-3" 

        FR (fv, ffe, ffs, ffv)

    let withLabeledBox lbl f =
      let fr, f = adaptft f
      let cr = elementRange
      let hr = elementRange + 1
      let br = elementRange

      ft (fr + cr + hr + br) <| fun fc fp fs sno ->
        let fp = lbl::fp 

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + cr + hr + br)        

        let hsno = sno + cr
        let bsno = sno + cr + hr

        let header  = content (hsno + 0 + elementRange) lbl |> element hsno "div" |> withClass "card-header"
        let body    = ffv |> element bsno "div" |> withClass "card-body" 
        let ffv     = (header +++ body) |> element sno "div" |> withClass "card mb-3" 

        FR (fv, ffe, ffs, ffv)
