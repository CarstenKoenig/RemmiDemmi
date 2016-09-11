namespace DWX2016.EventSourcing.Demo

open DWX2016.EventSourcing

module Bibilothek =

    type ErscheinungsJahr = int
    type Autor = string
    type Titel = string

    type Regal = string
    type Kunde = string

    type Events =
        | Katalogisiert  of ErscheinungsJahr * Autor * Titel
        | Einsortiert    of Regal
        | Ausgeliehen    of Kunde
        | Zurückgebracht

    module Projektionen =
        type Buch = { 
            Autor : Autor
            Titel : Titel
            ErscheinungsJahr : ErscheinungsJahr
            }

        let buch =
            Projections.latestPartial (function
                | Katalogisiert (ej,a,t) -> 
                    Some { 
                        Autor = a
                        Titel = t
                        ErscheinungsJahr = ej
                    }
                | _ -> None)
            |> Projections.fmap (function 
                | Some b -> b 
                | None -> failwith "keine Buchdetails vorhanden")

        type Ort =
            | ImRegal   of Regal
            | BeiKunde  of Kunde
            | ImService

        let ort =
            Projections.latest ImService
                (function
                | Einsortiert r  -> Some <| ImRegal r
                | Ausgeliehen k  -> Some <| BeiKunde k
                | Zurückgebracht -> Some <| ImService
                | _ -> None)

        type AnzahlAusgeliehen = AnzahlAusgeliehen

        let anzahlAusgeliehen =
            Projections.sumBy 
                AnzahlAusgeliehen
                (function
                | Ausgeliehen _ -> Some 1
                | _             -> None)

        type BuchStatus = { 
            Buch  : Buch
            Ort   : Ort
            AnzahlAusgeliehen : int
            }

        let buchStatis =
            Projections.constant (fun b o a -> { Buch = b; Ort = o; AnzahlAusgeliehen = a})
            <*> buch <*> ort <*> anzahlAusgeliehen
