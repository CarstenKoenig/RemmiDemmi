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
        fun v b -> { Von = v; Bis = b }
        <* von <*> bis

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

