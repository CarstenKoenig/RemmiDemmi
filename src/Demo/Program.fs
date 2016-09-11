namespace DWX2016.EventSourcing.Demo

open System
open DWX2016.EventSourcing

module DWX =
    open Session

    let eventsourcingId =
        let id = System.Guid.NewGuid ()
        let stream = InMemoryPersistence.EventSource.GetStream id
        stream.AddEvent <| Angelegt (Sprecher "C.König", Titel "Eventsourcing mit F#")
        stream.AddEvent <| TitelAktuallisiert (Titel "Funktionales Eventsourcing mit F#")
        stream.MakeSnapshot Projektionen.zusammenfassung NoBound
        stream.AddEvent <| Gestartet (DateTime (2016, 6, 21, 9, 0, 0))
        stream.AddEvent <| Beendet (DateTime (2016, 6, 21, 10, 0, 0))
        stream.MakeSnapshot Projektionen.zeitraum NoBound
        stream.AddEvent <| Bewertet ("Max", EinStern)
        stream.AddEvent <| Bewertet ("Evi", ZweiSterne)
        stream.AddEvent <| Bewertet ("Tom", ZweiSterne)
        stream.AddEvent <| Bewertet ("Udo", DreiSterne)
        stream.AddEvent <| Bewertet ("Max'", EinStern)
        stream.MakeSnapshot Projektionen.bewertung NoBound
        id       
          
    let zusammenfassung id = 
        let stream = InMemoryPersistence.EventSource.GetStream id
        stream.Read Projektionen.zusammenfassung NoBound

module Main =

    [<EntryPoint>]
    let main argv = 
        let ergebnis = DWX.zusammenfassung DWX.eventsourcingId

        printfn "Zusammenfassung:\n%O" ergebnis
        0 // return an integer exit code
