namespace Blazor.Formlet.TestComponents

open Blazor.Formlet
open Core
open Bootstrap
open Components
open Validate

type Person =
  {
    FirstName : string
    LastName  : string
    BirthDate : string
  }
  static member New fn ln bd : Person = { FirstName = fn; LastName = ln; BirthDate = bd }

type Company =
  {
    Name      : string
    OrgNo     : string
  }
  static member New n ono : Company = { Name = n; OrgNo = ono }

type Entity =
  | Person  of Person
  | Company of Company

type TestComponent() =
  class
    inherit FormletComponent<unit> ()

    let input v l p = 
      Input.text p "" 
      |> v
      |> Enhance.withValidation
      |> Enhance.withLabel l 
      |> Enhance.withFormGroup

    let isBirthDate = regex """^\d{4}-\d{2}-\d{2}$""" "Input must be a valid birth date like '1980-01-01'"

    let person =
      Formlet.value Person.New
      <*> input notEmpty    "First name" "Like 'John' or 'Jane'"
      <*> input notEmpty    "Last name"  "Like 'Doe'"
      <*> input isBirthDate "Birth date" "Like '1980-01-01'"
      |> Enhance.withLabeledBox "Person"
      |>> Person

    let company =
      Formlet.value Company.New
      <*> input notEmpty    "Company name"    "Like 'Microsoft'"
      <*> input notEmpty    "Company Org No"  "Like '12345'"
      |> Enhance.withLabeledBox "Company"
      |>> Company

    let select = 
      input notEmpty "Selector" "P or C"
      |>> fun s -> if s = "C" then 1 else 0
      |> Formlet.choose [|person; company|]

    let form = select |> Enhance.withForm

    override x.Formlet = form >>. Formlet.value ()

  end

