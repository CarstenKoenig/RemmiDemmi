namespace DWX2016.EventSourcing

module Main =

    [<EntryPoint>]
    let main _ = 
        printfn "DEMO 0\n*****\n\n"

        let session = Beispiel.initializeStream ()
        session.Enumerate VersionBound.NoBound
        |> Projektionen.zusammenfassung
        |> printfn "%A"
        0

