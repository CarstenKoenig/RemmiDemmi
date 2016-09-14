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
        | LaufzeitHinzugefuegt of Laufzeit
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

    let emptyFilm =
        film (Titel "") (Genre "") (Laufzeit 0<Min>) 0 0m


let filmId : AggregateId = Guid.NewGuid ()


let beispielStream =
    let stream = eventStore.GetStream filmId
    stream.Add <| FilmAngelegt (Titel "Project X", Genre "Comedy")
    stream.Add <| LaufzeitHinzugefuegt (Laufzeit 88<Min>)
    stream.Add <| Bewertet ("Anne", EinStern)
    stream.Add <| Bewertet ("Jakob", DreiSterne)
    stream.Add <| Bewertet ("Max", ZweiSterne)
    stream.Add <| Bewertet ("Susi", DreiSterne)
    stream










//////////////////////////////////////////////////////////////////////
// OOPish

type FilmAggregate (state : Film, bewertungen : decimal list) =

    member __.State = state

    member __.Apply (ev : Ereignisse) =
        match ev with
        | FilmAngelegt (titel, genre) ->
            let state' = { state with Titel = titel
                                      Genre = genre }
            FilmAggregate (state', bewertungen)
        | LaufzeitHinzugefuegt laufzeit ->
            let state' = { state with Laufzeit = laufzeit }
            FilmAggregate (state', bewertungen)
        | Bewertet (_, sterne) ->
            let bewertungen' = decimal sterne.Int :: bewertungen
            let state' = { state with AnzahlBewertungen = bewertungen'.Length
                                      Bewertung = bewertungen' |> Seq.average }
            FilmAggregate (state', bewertungen')

    static member Initial =
        FilmAggregate (emptyFilm, [])

    static member FromEvents =
        Seq.fold
           (fun (agg : FilmAggregate) -> agg.Apply)
           FilmAggregate.Initial

            
    

let beispielEvents = beispielStream.Events ()


let bewertung =
    beispielEvents
    |> Seq.choose (function Bewertet (_,b) -> Some (decimal b.Int) | _ -> None)
    |> Seq.average

let titel =
    beispielEvents
    |> Seq.choose (function FilmAngelegt (titel, _) -> Some titel | _ -> None)
    |> Seq.last





//////////////////////////////////////////////////////////////////////'event
// Fold
    
let titelRec (evs : Ereignisse list) : Titel = 
   let rec letzterTitel aktTitel =
       function
        | []                        -> aktTitel
        | FilmAngelegt (t,_) :: evs -> letzterTitel t evs
        | _                  :: evs -> letzterTitel aktTitel evs
   letzterTitel (Titel "---") evs


let titelFold (evs : Ereignisse seq) : Titel = 
    let updateTitel aktTitel =
       function
        | FilmAngelegt (t,_) -> t
        | _                  -> aktTitel
    Seq.fold updateTitel (Titel "---") evs


let bewertungFold (evs : Ereignisse seq) =
    let zaehle (anz, sum) =
        function
        | Bewertet (_, b) -> (anz + 1, sum + decimal b.Int)
        | _               -> (anz, sum)
    Seq.fold zaehle (0, 0m) evs
    |> (fun (anz, sum) ->
           if anz > 0 then
              sum / decimal anz
           else
              0m)


//////////////////////////////////////////////////////////////////////
// Projektionen

type AnzahlBewertungen = private | AnzahlBewertungen
let anzahlBewertungen : Projection<_,Ereignisse,int> =
    let istBewertet =
        function
        | Bewertet _ -> true
        | _          -> false
    Projektionen.countByP AnzahlBewertungen istBewertet

type SummeBewertungen = private | SummeBewertungen
let summeBewertungen : Projection<_,Ereignisse,decimal> =
    let bewertung =
        function
        | Bewertet (_,b) -> Some (decimal b.Int)
        | _              -> None
    Projektionen.sumByP SummeBewertungen bewertung

let bewertungP : Projection<_, Ereignisse, decimal> =
    Projektionen.parallelP (anzahlBewertungen, summeBewertungen)
    |> Projektionen.fmapP
      (fun (anz,bew) ->
         if anz > 0
         then bew / decimal anz
         else 0m)
