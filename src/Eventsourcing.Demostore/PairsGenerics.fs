namespace DWX2016.EventSourcing

open System

module internal GenericPair =

    let genericType = typeof<Projections.Pair<_,_>>.GetGenericTypeDefinition()

    let tryGetTypes (t : System.Type) =
        if not t.IsGenericType then None else
        if t.GetGenericTypeDefinition() <> genericType then None else
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