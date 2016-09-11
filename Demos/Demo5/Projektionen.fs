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

    type AnzahlÄnderungen = AnzahlÄnderungen
    let anzahlÄnderungen : Projection<_,_,int> = 
        countByP AnzahlÄnderungen
            (function
            | TitelAktuallisiert _ -> true
            | _                    -> false)


    type VonZeitraum = VonZeitraum
    type BisZeitraum = BisZeitraum
    let zeitraum : Projection<_,_,Zeitraum> = 
        let von =
            lastLabeledP VonZeitraum
                (function Gestartet t -> Some t | _ -> None)
                DateTime.MinValue
        let bis =
            lastLabeledP BisZeitraum
                (function Beendet t -> Some t | _ -> None)
                DateTime.MinValue
        fun v b -> { Von = v; Bis = b }
        <* von <*> bis

    type AnzahlBewertungen = AnzahlBewertungen
    let anzahlBewertungen : Projection<_,_,int> =
        countByP AnzahlBewertungen
            (function
                | Bewertet _ -> true
                | _          -> false)

    type SummeBewertungen = SummeBewertungen
    let bewertung : Projection<_,_,decimal> = 
        let summeBewertungen =
            sumByP SummeBewertungen
                (function
                    | Bewertet (_,EinStern)   -> Some 1
                    | Bewertet (_,ZweiSterne) -> Some 2
                    | Bewertet (_,DreiSterne) -> Some 3
                    | _                       -> None)
        fun wert anzahl ->
            if anzahl > 0 then
                decimal wert / decimal anzahl
            else 0m
        <* summeBewertungen <*> anzahlBewertungen

    let zusammenfassung : Projection<_,_,Zusammenfassung> =
        zusammenfassungConst
        <* sprecher <*> titel 
        <*> anzahlÄnderungen <*> zeitraum
        <*> bewertung <*> anzahlBewertungen
