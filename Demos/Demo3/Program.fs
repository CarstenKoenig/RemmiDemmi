namespace DWX2016.EventSourcing

module Main =

    [<EntryPoint>]
    let main _ = 
        printfn "DEMO 3\n*****\n\n"

        let session = Beispiel.initializeStream ()
        session
        |> Projektionen.zusammenfassung
        |> printfn "%A"
        0

