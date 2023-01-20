open Fable.SimpleHttp
open Sutil
open Sutil.Attr
open Sutil.DOM
open Thoth.Json

type Model = { Fact: string; Error: string option }

type HttpResponse<'a> =
    | Response of 'a
    | Failure of reason: string

type Msg =
    | GetFact
    | GotResponse of HttpResponse<string>

let fact model = model.Fact
let error model = model.Error

let getFact () = async {
    let! response = Http.request "https://catfact.ninja/fact" |> Http.method GET |> Http.send

    return
        if response.statusCode = 200 then
            let factResponse = Decode.Auto.fromString<{| fact: string |}> response.responseText

            match factResponse with
            | Ok factResponse -> Response factResponse.fact
            | Result.Error reason -> Failure reason
        else
            Failure "Request wasn't successful."
}

let initialState () = { Fact = ""; Error = None }, Cmd.none

let update msg model =
    match msg with
    | GetFact -> model, Cmd.OfAsync.perform getFact () GotResponse
    | GotResponse(Response fact) -> { model with Fact = fact; Error = None }, Cmd.none
    | GotResponse(Failure error) ->
        { model with
            Fact = ""
            Error = Some error
        },
        Cmd.none

let renderError (error: string option) =
    match error with
    | Some error -> Html.p error
    | None -> Html.none

let view () =
    let dispose = fun _ -> ()
    let model, dispatch = () |> Store.makeElmish initialState update dispose

    Html.div [
        disposeOnUnmount [ model ]

        Bind.el (model |> Store.map error, renderError)
        Bind.el (model |> Store.map fact, Html.p)
        Html.button [ text "Get cat fact"; onClick (fun _ -> dispatch GetFact) [] ]
    ]

// Start the app
view () |> Program.mountElement "sutil-app"
