namespace DWX2016.EventSourcing

open System

module internal Tracing =

    let rec describe (t : System.Type) =
        if not t.IsGenericType then describePrim t else
        if t.GetGenericTypeDefinition() <> GenericPair.genericType then describeGen t else
        let args = t.GetGenericArguments()
        sprintf "%s * %s" (describe args.[0]) (describe args.[1])
    and describePrim (t : Type) =
        if t = typeof<unit> then "()" else t.Name
    and describeGen (t : Type) =
        if t.GetGenericTypeDefinition() = typeof<Option<_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "%s option" args.[0].Name
        elif t.GetGenericTypeDefinition() = typeof<Projections.Sum<_,_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "%s Summe" args.[0].Name
        elif t.GetGenericTypeDefinition() = typeof<Projections.Average<_,_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "%s Mittelwert" args.[0].Name
        else
            t.Name