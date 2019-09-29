# Blazor.Server.Formlets

Formlets for Blazer Server side. Goal is to allow creating forms created through code like below.

```fsharp
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
```

> No further development can be done with my current understanding of Blazor server side

So after extensive testing and experimenting I found that how blazor render builder works with sequences and sub components is from my understanding incompatible with the dynamic nature of Formlet trees. 

I think it could be possibly to salvage something but it would either have to rely on hacks or sacrificing much of what is nice with Foemlets.


