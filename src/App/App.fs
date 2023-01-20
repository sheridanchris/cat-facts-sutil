open Fable.SimpleHttp
open Sutil
open Sutil.Attr
open Sutil.DOM
open Thoth.Json

type Model = { Fact: string }

type HttpResponse<'a> =
    | OkResponse of 'a
    | FailureResponse

type Msg =
    | GetFact
    | GotResponse of HttpResponse<string>

let fact model = model.Fact

// TODO: Model failure states.
let getFact () = async {
    let! response = Http.request "https://catfact.ninja/fact" |> Http.method GET |> Http.send

    return
        if response.statusCode = 200 then
            let factResponse = Decode.Auto.fromString<{| fact: string |}> response.responseText

            match factResponse with
            | Ok fact -> OkResponse fact.fact
            | Result.Error _ -> FailureResponse
        else
            FailureResponse
}

let initialState () = { Fact = "" }, Cmd.none

let update msg model =
    match msg with
    | GetFact -> model, Cmd.OfAsync.perform getFact () GotResponse
    | GotResponse(OkResponse fact) -> { model with Fact = fact }, Cmd.none
    | GotResponse FailureResponse -> { model with Fact = "" }, Cmd.none

let view () =
    let dispose = fun _ -> ()
    let model, dispatch = () |> Store.makeElmish initialState update dispose

    Html.div [
        disposeOnUnmount [ model ]
        Bind.fragment (model |> Store.map fact) <| Html.p
        Html.button [
            text "Get cat fact"
            onClick (fun _ -> dispatch GetFact) []
        ]
    ]

// Start the app
view () |> Program.mountElement "sutil-app"
