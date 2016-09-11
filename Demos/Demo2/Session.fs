namespace DWX2016.EventSourcing

open System
module Session =

    type Teilnehmer = string
    type Sprecher = Sprecher of string
    type Titel    = Titel of string

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
        | Angelegt           of Sprecher * Titel
        | TitelAktuallisiert of Titel
        | Gestartet          of DateTime
        | Beendet            of DateTime
        | Bewertet           of Teilnehmer * Sterne

    type Zeitraum = 
        { 
            Von : DateTime
            Bis : DateTime 
        }

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