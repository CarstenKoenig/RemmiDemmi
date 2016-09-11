namespace DWX2016.EventSourcing

open System
open Session

module Projektionen = 
    open Kombinatoren

    let sprecher : Projection<_,_,Sprecher> = 
        firstP 
            (function
            | Angelegt (s,_) -> Some s
            | _              -> None )
            (Sprecher "???")

    let titel : Projection<_,_,Titel> =
        lastP
            (function
            | Angelegt (_,t)       -> Some t
            | TitelAktuallisiert t -> Some t
            | _                    -> None)
            (Titel "---")

    let anzahlÄnderungen : Projection<_,_,int> = 
        countByP
            (function
            | TitelAktuallisiert _ -> true
            | _                    -> false)


    let zeitraum : Projection<_,_,Zeitraum> = 
        let von =
            lastP
                (function Gestartet t -> Some t | _ -> None)
                DateTime.MinValue
        let bis =
            lastP
                (function Beendet t -> Some t | _ -> None)
                DateTime.MinValue
        parallelP (von, bis)
        |> fmap (fun (v,b) -> { Von = v; Bis = b })

    let anzahlBewertungen : Projection<_,_,int> =
        countByP
            (function
                | Bewertet _ -> true
                | _          -> false)

    let bewertung : Projection<_,_,decimal> = 
        let summeBewertungen =
            sumByP
                (function
                    | Bewertet (_,EinStern)   -> Some 1
                    | Bewertet (_,ZweiSterne) -> Some 2
                    | Bewertet (_,DreiSterne) -> Some 3
                    | _                       -> None)
        parallelP (summeBewertungen, anzahlBewertungen)
        |> fmap (function
                | wert, anzahl when anzahl > 0 -> 
                    decimal wert / decimal anzahl
                | _ -> 0m)

    let zusammenfassung (stream : IEventStream) : Zusammenfassung =
        let zusammenfassung' =
            parallelP (sprecher,
                parallelP (titel, 
                    parallelP (anzahlÄnderungen, 
                        parallelP (zeitraum,
                            parallelP (bewertung, anzahlBewertungen)))))
            |> fmap (fun (s, (t, (anzÄ, (z, (b, anzB))))) ->
                {
                    Sprecher          = s
                    Titel             = t
                    AnzahlÄnderungen  = anzÄ
                    Zeitraum          = z
                    Bewertung         = b
                    AnzahlBewertungen = anzB
                })
        stream.Read zusammenfassung' NoBound