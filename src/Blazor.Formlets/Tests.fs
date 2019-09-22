namespace Blazor.Formlets.Tests

open Blazor.Formlets.Core
open Blazor.Formlets.Core.Builder
open Blazor.Formlets.Core.Inputs
open Blazor.Formlets.Core.Enhance
open Blazor.Formlets.Core.Components
open Blazor.Formlets.Core.Validate
open Blazor.Formlets.Core.Surround

type Example1FormletComponent () = 
  inherit FormletComponent<string*string> ()

  let input validator lbl placeholder = 
    text placeholder "" 
    |> validator
    |> withValidation 
    |> withLabel lbl 
    |> withFormGroup

  override x.Formlet = 
    let form = 
      formlet {
        let! firstName = input notEmpty "First Name" "Like 'John' or 'Jane'"
        let! lastName  = input notEmpty "Last Name"  "Like 'Doe'"
        return firstName, lastName
      } |> withLabeledBox "Person" |> withSubmit |> withForm
    form
