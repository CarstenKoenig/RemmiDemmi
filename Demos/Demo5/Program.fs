namespace DWX2016.EventSourcing

module Main =

    [<EntryPoint>]
    let main _ = 
        printfn "DEMO 5\n*****\n\n"

        let session = Beispiel.initializeStream ()
        session.Read Projektionen.zusammenfassung NoBound
        |> printfn "%A"

        0

