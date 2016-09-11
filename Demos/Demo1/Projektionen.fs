namespace DWX2016.EventSourcing

open System
open Session

module Projektionen = 

    let rec sprecher : Ereignisse seq -> Sprecher = 
        Seq.fold 
            (fun sprecher ->
                function
                | Angelegt (s,_) -> s
                | _              -> sprecher)
             (Sprecher "???")

    let titel : Ereignisse list -> Titel = 
        Seq.fold
            (fun aktTitel ->
                function
                | Angelegt (_,t)       -> t
                | TitelAktuallisiert t -> t
                | _                    -> aktTitel)
             (Titel "---")

    let anzahlÄnderungen : Ereignisse seq -> int = 
        Seq.fold
            (fun aktAnzahl ->
                function
                | TitelAktuallisiert _ -> aktAnzahl + 1
                | _                    -> aktAnzahl)
            0

    let zeitraum : Ereignisse seq -> Zeitraum = 
        Seq.fold
            (fun (von,bis) ->
                function
                | Gestartet t -> (Some t, bis)
                | Beendet t   -> (von, Some t)
                | _           -> (von, bis))
             (None, None)
        >> function 
            | Some von, Some bis -> { Von = von; Bis = bis }
            | _                  -> { Von = DateTime.MinValue; Bis = DateTime.MinValue }

    let bewertung : Ereignisse seq -> decimal = 
        Seq.fold 
            (fun (wert, anzahl) ->
                function
                | Bewertet (_,EinStern)   -> (wert+1, anzahl+1)
                | Bewertet (_,ZweiSterne) -> (wert+2, anzahl+1)
                | Bewertet (_,DreiSterne) -> (wert+3, anzahl+1)
                | _                       -> (wert, anzahl))
            (0,0)
        >> function
            | wert, anzahl when anzahl > 0 -> 
                decimal wert / decimal anzahl
            | _ -> 0m

    let anzahlBewertungen : Ereignisse seq -> int = 
        Seq.fold
            (fun anzahl ->
                function
                | Bewertet (_,_) -> anzahl + 1
                | _              -> anzahl)
            0

    let zusammenfassung (evs : Ereignisse seq) : Zusammenfassung =
        let evs = List.ofSeq evs
        {
            Sprecher          = sprecher evs
            Titel             = titel evs
            AnzahlÄnderungen  = anzahlÄnderungen evs
            Zeitraum          = zeitraum evs
            Bewertung         = bewertung evs
            AnzahlBewertungen = anzahlBewertungen evs
        }
