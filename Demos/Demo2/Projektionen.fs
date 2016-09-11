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
        createP
            (fun (von,bis) ->
                function
                | Gestartet t -> (Some t, bis)
                | Beendet t   -> (von, Some t)
                | _           -> (von, bis))
            (None, None)
            (function 
                | Some von, Some bis -> { Von = von; Bis = bis }
                | _                  -> { Von = DateTime.MinValue; Bis = DateTime.MinValue })

    let bewertung : Projection<_,_,decimal> = 
        createP
            (fun (wert, anzahl) ->
                function
                | Bewertet (_,EinStern)   -> (wert+1, anzahl+1)
                | Bewertet (_,ZweiSterne) -> (wert+2, anzahl+1)
                | Bewertet (_,DreiSterne) -> (wert+3, anzahl+1)
                | _                       -> (wert, anzahl))
            (0,0)
            (function
                | wert, anzahl when anzahl > 0 -> 
                    decimal wert / decimal anzahl
                | _ -> 0m)

    let anzahlBewertungen : Projection<_,_,int> = 
        countByP
            (function
                | Bewertet (_,_) -> true
                | _              -> false)

    let zusammenfassung (stream : IEventStream) : Zusammenfassung =
        {
            Sprecher          = stream.Read sprecher NoBound
            Titel             = stream.Read titel NoBound
            AnzahlÄnderungen  = stream.Read anzahlÄnderungen NoBound
            Zeitraum          = stream.Read zeitraum NoBound
            Bewertung         = stream.Read bewertung NoBound
            AnzahlBewertungen = stream.Read anzahlBewertungen NoBound
        }
