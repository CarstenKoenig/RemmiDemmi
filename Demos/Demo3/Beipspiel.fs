namespace DWX2016.EventSourcing

open System
open Session

module Beispiel = 

    let sessionId : AggregateId = Guid.NewGuid ()
    let eventSource = EventStore.InMemory

    let initializeStream () =
        let stream = eventSource.GetStream sessionId
        stream.Add <| Angelegt (Sprecher "C.König", Titel "Eventsourcing mit F#")
        stream.Add <| TitelAktuallisiert (Titel "Funktionales Eventsourcing mit F#")
        stream.Add <| Gestartet (DateTime (2016, 6, 21, 11, 45, 0))
        stream.Add <| Beendet (DateTime (2016, 6, 21, 12, 45, 0))
        stream.Add <| Bewertet ("Max", EinStern)
        stream.Add <| Bewertet ("Evi", ZweiSterne)
        stream.Add <| Bewertet ("Tom", ZweiSterne)
        stream.Add <| Bewertet ("Udo", DreiSterne)
        stream.Add <| Bewertet ("Max'", EinStern)
        stream