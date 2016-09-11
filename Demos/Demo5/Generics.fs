namespace DWX2016.EventSourcing

open System
open Kombinatoren

module internal Generics =


    let genericType = typeof<Pair<_,_>>.GetGenericTypeDefinition()

    let tryGetTypes (t : System.Type) =
        if not t.IsGenericType then None else
        if t.GetGenericTypeDefinition() <> genericType then 
            None 
        else
            let args = t.GetGenericArguments()
            Some (args.[0], args.[1])

    let tryGetValues (value : obj) =
        let t = value.GetType()
        if not t.IsGenericType then None else
        if t.GetGenericTypeDefinition() <> genericType then None else
        let args = t.GetGenericArguments()
        let a = t.GetProperty("First").GetGetMethod().Invoke(value, [||])
        let b = t.GetProperty("Second").GetGetMethod().Invoke(value, [||])
        Some (args.[0], args.[1], a, b)

    let make (ta : Type, tb : Type, oa : obj, ob : obj, va, vb) =
        genericType.MakeGenericType(ta, tb).GetConstructors().[0].Invoke([|oa; box va; ob; box vb|])

    let rec describe (t : System.Type) =
        if not t.IsGenericType then describePrim t else
        if t.GetGenericTypeDefinition() <> genericType then describeGen t else
        let args = t.GetGenericArguments()
        sprintf "%s * %s" (describe args.[0]) (describe args.[1])
    and describePrim (t : Type) =
        if t = typeof<unit> then "()" else t.Name
    and describeGen (t : Type) =
        if t.GetGenericTypeDefinition() = typeof<Option<_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "%s option" (describe args.[0])
        elif t.GetGenericTypeDefinition() = typeof<Labeled<_,_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "[%s] %s" args.[0].Name (describe args.[1])
        elif t.GetGenericTypeDefinition() = typeof<Summe<_,_>>.GetGenericTypeDefinition() then
            let args = t.GetGenericArguments()
            sprintf "%s Summe" args.[0].Name
        else
            t.Name