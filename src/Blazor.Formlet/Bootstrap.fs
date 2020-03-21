namespace Blazor.Formlet

module Bootstrap =
  open Core
  open Core.Details
  open Formlet
  open FormletViews

  open System.Text

  module Enhance =
    let withFormGroup f =
      let lr = divRange
      let fr, f = adaptft f
      ft (fr + lr) <| fun fc fp fs sno ->

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + lr)

        let ffv =
          ffv
          |> div sno
          |> withClass "form-group"

        FR (fv, ffe, ffs, ffv)

    let inline withValidation f =
      let fr, f = adaptft f
      let vr = divRange + 1
      ft (fr + vr) <| fun fc fp fs sno ->

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs sno

        let ffv =
          match ffe with
          | FormletError.Empty  -> ffv |> withClass "is-valid"
          | _                   ->
            let sb = StringBuilder 16
            let folder (sb : StringBuilder) _ _ msg =
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
              |> div sno
              |> withClass "invalid-feedback"
            lfv +++ rfv

        FR (fv, ffe, ffs, ffv)

    let withBox f =
      let fr, f = adaptft f
      let cr = divRange
      let br = divRange

      ft (fr + cr + br) <| fun fc fp fs sno ->
        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + cr + br)

        let bsno = sno + cr

        let body    = ffv |> div bsno |> withClass "card-body"
        let ffv     = body |> div sno |> withClass "card mb-3"

        FR (fv, ffe, ffs, ffv)

    let withLabeledBox lbl f =
      let fr, f = adaptft f
      let cr = divRange
      let hr = divRange + 1
      let br = divRange

      ft (fr + cr + hr + br) <| fun fc fp fs sno ->
        let fp = lbl::fp

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + cr + hr + br)

        let hsno = sno + cr
        let bsno = sno + cr + hr

        let header  = content (hsno + 0 + elementRange) lbl |> div hsno |> withClass "card-header"
        let body    = ffv |> div bsno |> withClass "card-body"
        let ffv     = (header +++ body) |> div sno |> withClass "card mb-3"

        FR (fv, ffe, ffs, ffv)

    let withSubmit f =
      let fr, f = adaptft f
      let hr = divRange + spanRange + 1 + 2*buttonRange
      let br = divRange + 2 + elementRange
      let lr = divRange
      ft (fr + lr + hr + br) <| fun fc fp fs sno ->

        // TODO: Getting the sequence numbers right for bigger controls is a bit messy.
        //  How to improve?

        let hsno = sno + lr
        let bsno = sno + lr + hr

        let (FR (fv, ffe, ffs, ffv)) = invokeft f fc fp fs (sno + lr + hr + br)

        let header, body =
          match ffe with
          | FormletError.Empty  -> "Ready to submit!", content (bsno + 0 + elementRange) "No problems detected"
          | _                   ->
            let trs = ResizeArray ()
            let sb = StringBuilder ()
            let folder () i (fp : FormletPath) (msg : string) =
              let tdr = elementRange + 1
              let trr = elementRange
              let tr  = tdr*3 + trr
              sb.Clear () |> ignore
              let ifolder (p : string) () = sb.Append '.' |> ignore; sb.Append p |> ignore
              List.foldBack ifolder fp ()
              let td0 = content (tr*i + 0 + elementRange + tdr*0 + trr) (sb.ToString ()) |> element (tr*i + 0 + tdr*0 + trr) "td"
              let td1 = content (tr*i + 0 + elementRange + tdr*1 + trr) "-" |> element (tr*i + 0 + tdr*1 + trr) "td"
              let td2 = content (tr*i + 0 + elementRange + tdr*2 + trr) msg |> element (tr*i + 0 + tdr*2 + trr) "td"
              let tds = td0 +++ td1 +++ td2
              let tr = tds |> element (tr*i) "tr"
              trs.Add tr
            ffe.Fold folder ()
            let table =
              group (bsno + 1 + 2*elementRange) (trs.ToArray())
              |> element (bsno + 1 + elementRange) "table"
            "Fix validation error(s)"  , table

        let submitButton = button (hsno + 0 + divRange + 0*buttonRange) "btn-dark"     "Submit"
        let resetButton  = button (hsno + 0 + divRange + 1*buttonRange) "btn-warning"  "Reset"

        let headerContent =
          content (hsno + 0 + spanRange + divRange + 2*buttonRange) header
          |> span (hsno + 0 + divRange + 2*buttonRange)

        let header  = (submitButton +++ resetButton +++ headerContent) |> div hsno |> withClass "card-header"
        let body    = body |> div bsno |> withClass "card-body"
        let legend  = (header +++ body) |> div sno |> withClass "card mb-3"

        FR (fv, ffe, ffs, legend +++ ffv)
