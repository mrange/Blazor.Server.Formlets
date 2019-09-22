namespace Blazor.Formlets.Tests

open Blazor.Formlets.Core
open Blazor.Formlets.Core.Builder
open Blazor.Formlets.Core.Inputs
open Blazor.Formlets.Core.Enhance
open Blazor.Formlets.Core.Components
open Blazor.Formlets.Core.Validate
open Blazor.Formlets.Core.Surround

type Person =
  {
    FirstName : string
    LastName  : string
    SocialNo  : string
  }
  static member New fn ln sno : Person = { FirstName = fn; LastName = ln; SocialNo = sno }

type Company =
  {
    Name      : string
    OrgNo     : string
  }
  static member New n ono : Company = { Name = n; OrgNo = ono }

type Entity =
  | Person  of Person
  | Company of Company

type Example1FormletComponent () = 
  inherit FormletComponent<Entity> ()

  static let input validator lbl placeholder = 
    text placeholder "" 
    |> validator
    |> withValidation 
    |> withLabel lbl 
    |> withFormGroup

  static let person =
    Formlet.value Person.New
    <*> input notEmpty "First name"           "Like 'John' or 'Jane'"
    <*> input notEmpty "Last name"            "Like 'Doe'"
    <*> input notEmpty "Social security no"   "Like 123456-12345"
    |> withLabeledBox "Personal information"
    |>> Entity.Person

  static let company =
    Formlet.value Company.New
    <*> input notEmpty "Name"       "Like 'Google' or 'Apple'"
    <*> input notEmpty "Org no"     "Like 'MVA123456'"
    |> withLabeledBox "Company information"
    |>> Entity.Company

  static let entity = select [|"Person", person; "Company", company|] |> withLabel "Select entity" |> Formlet.unwrap

  static let form = 
    entity 
    |> withSubmit 
    |> withForm

  override x.Formlet = 
    form
