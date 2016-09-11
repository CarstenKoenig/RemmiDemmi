// BEISPIEL

// #r "DemoStore.dll"
#load "DemoStore.fsx"


open System
open Definitionen

let eventStore = DemoStore.EventStore.InMemory

[<Measure>]
type Min

[<AutoOpen>]
module Filme =

    type Titel = Titel of string
    type Genre = Genre of string
    type Laufzeit = Laufzeit of int<Min>

    type Bewerter = string

    type Sterne = 
        | EinStern 
        | ZweiSterne 
        | DreiSterne
        override this.ToString() =
            match this with
            | EinStern -> "*"
            | ZweiSterne -> "**"
            | DreiSterne -> "***"
        member this.Int =
            match this with
            | EinStern -> 0
            | ZweiSterne -> 1
            | DreiSterne -> 2

    type Ereignisse =
        | FilmAngelegt of Titel * Genre
        | LaufzeitHinzugefügt of Laufzeit
        | Bewertet of Bewerter * Sterne

    type Film =
        {
            Titel : Titel
            Genre : Genre
            Laufzeit : Laufzeit
            Bewertung : decimal
            AnzahlBewertungen : int
        }
        override this.ToString() =
            let (Titel titel)= this.Titel
            let (Laufzeit laufzeit) = this.Laufzeit 
            sprintf "%s [%d min]\n%d Bewertungen mit %.1f Sterne(n)" 
                titel 
                laufzeit
                this.AnzahlBewertungen
                this.Bewertung

    let film titel genre laufzeit anzahl bewertung =
        {
            Titel = titel
            Genre = genre
            Laufzeit = laufzeit
            AnzahlBewertungen = anzahl
            Bewertung = bewertung
        }

let filmId : AggregateId = Guid.NewGuid ()

let beispielStream =
    let stream = eventStore.GetStream filmId
    stream.Add <| FilmAngelegt (Titel "RemmiDemmi in Hamburg", Genre "Thriller")
    stream.Add <| LaufzeitHinzugefügt (Laufzeit 60<Min>)
    stream.Add <| Bewertet ("Anne", EinStern)
    stream.Add <| Bewertet ("Jakob", DreiSterne)
    stream.Add <| Bewertet ("Max", ZweiSterne)
    stream.Add <| Bewertet ("Susi", DreiSterne)
    stream

let beispielEvents : Ereignisse seq = beispielStream.Events ()

beispielEvents
    |> Seq.choose (function Bewertet (_,b) -> Some (decimal b.Int) | _ -> None)
    |> Seq.average