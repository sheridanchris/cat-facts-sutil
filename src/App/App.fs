open Sutil

let view () =
    text "Hello World!"

// Start the app
view() |> Program.mountElement "sutil-app"
