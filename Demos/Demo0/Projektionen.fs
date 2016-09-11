namespace DWX2016.EventSourcing

open System
open Session

module Projektionen = 

    let rec sprecher (evs : Ereignisse list) : Sprecher = 
        match evs with
        | []                    -> Sprecher "???"
        | Angelegt (s,_) :: _   -> s
        | _              :: evs -> sprecher evs

    let titel (evs : Ereignisse list) : Titel = 
        let rec letzterTitel aktTitel =
            function
            | []                          -> aktTitel
            | Angelegt (_,t)       :: evs -> letzterTitel t evs
            | TitelAktuallisiert t :: evs -> letzterTitel t evs
            | _                    :: evs -> letzterTitel aktTitel evs
        letzterTitel (Titel "---") evs

    let anzahlÄnderungen (evs : Ereignisse list) : int = 
        let rec zählen aktAnzahl =
            function
            | []                          -> aktAnzahl
            | TitelAktuallisiert _ :: evs -> zählen (aktAnzahl+1) evs
            | _                    :: evs -> zählen aktAnzahl evs
        zählen 0 evs

    let zeitraum (evs : Ereignisse list) : Zeitraum = 
        let rec zeitraum' von bis =
            function
            | []                 -> (von, bis)
            | Gestartet t :: evs -> zeitraum' (Some t) bis evs
            | Beendet t   :: evs -> zeitraum' von (Some t) evs
            | _           :: evs -> zeitraum' von bis evs
        match zeitraum' None None evs with
        | Some von, Some bis -> { Von = von; Bis = bis }
        | _                  -> { Von = DateTime.MinValue; Bis = DateTime.MinValue }

    let bewertung (evs : Ereignisse list) : decimal = 
        let rec zählen wert anzahl =
            function
            | []                             -> (wert, anzahl)
            | Bewertet (_,EinStern)   :: evs -> zählen (wert+1) (anzahl+1) evs
            | Bewertet (_,ZweiSterne) :: evs -> zählen (wert+2) (anzahl+1) evs
            | Bewertet (_,DreiSterne) :: evs -> zählen (wert+3) (anzahl+1) evs
            | _                       :: evs -> zählen wert anzahl evs
        match zählen 0 0 evs with
        | wert, anzahl when anzahl > 0 -> 
            decimal wert / decimal anzahl
        | _ -> 0m

    let anzahlBewertungen (evs : Ereignisse list) : int = 
        let rec zählen anzahl =
            function
            | []                    -> anzahl
            | Bewertet (_,_) :: evs -> zählen (anzahl+1) evs
            | _              :: evs -> zählen anzahl evs
        zählen 0 evs

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
