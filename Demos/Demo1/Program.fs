namespace DWX2016.EventSourcing

module Main =

    [<EntryPoint>]
    let main _ = 
        printfn "DEMO 1\n*****\n\n"

        let session = Beispiel.initializeStream ()
        session.Enumerate VersionBound.NoBound
        |> Projektionen.zusammenfassung
        |> printfn "%A"
        0

