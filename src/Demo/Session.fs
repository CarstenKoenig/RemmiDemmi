namespace DWX2016.EventSourcing.Demo

open System
open DWX2016.EventSourcing

module Session =

    type Zuhörer = string
    type Sprecher = Sprecher of string
    type Titel    = Titel of string

    type Regal = string
    type Kunde = string

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

    type Events =
        | Angelegt           of Sprecher * Titel
        | TitelAktuallisiert of Titel
        | Gestartet          of DateTime
        | Beendet            of DateTime
        | Bewertet           of Zuhörer * Sterne

    type Zeitraum = { Von : DateTime; Bis : DateTime }

    module Projektionen =

        let sprecher = 
            Projections.latest 
                (Sprecher "---")
                (function Angelegt (s,_) -> Some s | _ -> None)

        let titel =
            Projections.latest 
                (Titel "---")
                (function 
                    | Angelegt (_,t) -> Some t 
                    | TitelAktuallisiert t -> Some t
                    | _ -> None)
        
        let zeitraum =
            Projections.create
                { Von = DateTime.MinValue; Bis = DateTime.MinValue }
                (fun zeitraum ->
                    function
                    | Gestartet t -> 
                        #if trace
                        printfn "setze Zeitraum-Von auf %O" t
                        #endif
                        { zeitraum with Von = t }
                    | Beendet t   -> 
                        #if trace
                        printfn "setze Zeitruam-Bis auf %O" t
                        #endif
                        { zeitraum with Bis = t }
                    | _           -> 
                        #if trace
                        printfn "<Zeitraum-ignoriert>" t
                        #endif
                        zeitraum)

        type Bewertung = Bewertung
        let bewertung =
            Projections.averageBy 
                Bewertung
                (function
                | Bewertet (_, EinStern)   -> Some 1m
                | Bewertet (_, ZweiSterne) -> Some 2m
                | Bewertet (_, DreiSterne) -> Some 3m
                | _               -> None)

        let anzahlBewertungen = 
            Projections.sumBy
                Bewertung
                (function 
                | Bewertet _ -> Some 1
                | _          -> None)

        type Aenderungen = Aenderungen
        let anzahlAenderungen = 
            Projections.sumBy
                Aenderungen
                (function 
                | TitelAktuallisiert _ -> Some 1
                | _                    -> None)

        type Zusammenfassung =
            {
                Sprecher          : Sprecher
                Titel             : Titel
                AnzahlÄnderungen  : int
                Zeitraum          : Zeitraum
                Bewertung         : decimal
                AnzahlBewertungen : int
            }
            override this.ToString() =
                let (Sprecher sprecher)= this.Sprecher
                let (Titel titel) = this.Titel 
                sprintf "%s [%s]\n%d Änderungen\n%s-%s\n%d Bewertungen mit %.1f Sterne" 
                    titel sprecher
                    this.AnzahlÄnderungen
                    (this.Zeitraum.Von.ToString("HH:mm")) 
                    (this.Zeitraum.Bis.ToString("HH:mm")) 
                    this.AnzahlBewertungen
                    this.Bewertung

        let zusammenfassung =
            Projections.constant (fun s t aa z b ab -> 
                { Sprecher = s
                ; Titel = t
                ; AnzahlÄnderungen = aa
                ; Zeitraum = z
                ; Bewertung = b
                ; AnzahlBewertungen = ab})
            <*> sprecher <*> titel <*> anzahlAenderungen 
            <*> zeitraum <*> bewertung  <*> anzahlBewertungen