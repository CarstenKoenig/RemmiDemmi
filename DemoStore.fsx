#load "Definitionen.fsx"

open System
open Definitionen

module Generics =


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
      // Sadly this does not work in Mono right now
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

///////////////////////////////////////////////////////////////////////////////////

module EventStore =

    type Record<'event> = {
        AggregateId : AggregateId
        Version     : AggregateVersion
        Event       : 'event
        }


    let private events = System.Collections.Generic.List<Record<obj>>()

    let private inBounds (lower:VersionBound, upper:VersionBound) (v:AggregateVersion) =
        let lowerOk =
            match lower with
            | NoBound -> true
            | Limit l -> l <= v
        let upperOk =
            match upper with
            | NoBound -> true
            | Limit u -> v <= u
        lowerOk && upperOk

    let private aggregateEvents (lower, upper) (id : AggregateId) =
        events 
        |> Seq.filter (fun r -> r.AggregateId = id && inBounds (lower,upper) r.Version)

    let private latestVersion (id : AggregateId) =
        aggregateEvents (NoBound, NoBound) id
        |> Seq.tryLast
        |> function Some r -> r.Version | None -> 0


    let private enumEvents (id:AggregateId) (lower:VersionBound, upper:VersionBound) : WithMeta<'event> seq * AggregateVersion =
        let events = aggregateEvents (lower,upper) id |> Seq.toArray
        let version = if events.Length = 0 then 0 else (Seq.last events).Version
        // (filtered |> Seq.map (fun r -> printfn "reading %d" r.Version; unbox r.Event, r.Version), version)
        let withMeta (r : Record<'event>) = {
            Meta =
              {
                Id = r.AggregateId
                Version = r.Version
              }
            Event = unbox r.Event
            }
        (events |> Seq.map withMeta, version)


    let private addEvent (id : AggregateId) (ev : 'event) =
        let version = latestVersion id + 1
        { AggregateId = id; Version = version; Event = box ev }
        |> events.Add


    let private snapshots = System.Collections.Generic.Dictionary<System.Type*AggregateId, AggregateVersion*obj>()


    let rec private getSnapshotObj (id:AggregateId) (lower:VersionBound, upper:VersionBound) (snapshotType : System.Type) : (AggregateVersion*obj) option =
        if snapshotType = typeof<unit> then Some (upper.ToUpperValue, box ()) else
        match Generics.tryGetTypes snapshotType with
        | Some (ta, tb) ->
            getSnapshotObj id (lower, upper) ta
            |> Option.bind (fun (va, oa) ->
                getSnapshotObj id (lower, upper) tb
                |> Option.map (fun (vb, ob) ->
                    (min va vb, Generics.make (ta, tb, oa, ob, va, vb))))
        | None ->
            let key = snapshotType, id
            match snapshots.TryGetValue key with
            | (true, (ver, o)) when ver |> inBounds (NoBound, upper) -> 
                printfn "got Snapshot [%s] Ver %d" (Generics.describe snapshotType) ver
                Some (ver, o)
            | _ -> 
                None


    let private getSnapshot (id:AggregateId) (lower:VersionBound, upper:VersionBound) : Snapshot<'s> option =
        match getSnapshotObj id (lower,upper) typeof<'s> with
        | None          -> 
            None
        | Some (ver, o) -> 
            Some { AggregateId = id; Version = ver; Value = unbox o }


    let rec private addSnapshotObj (id:AggregateId, version : AggregateVersion) (snapshotType : System.Type, snapshotValue : obj) =
        if snapshotType = typeof<unit> then () else
        match Generics.tryGetValues snapshotValue with
        | Some (ta, tb, va, vb) ->
            addSnapshotObj (id, version) (ta, va)
            addSnapshotObj (id, version) (tb, vb)
        | None ->
            let key = snapshotType, id
            printfn "wrote Snapshot [%s] with Ver %d" (Generics.describe snapshotType) version
            snapshots.[key] <- (version, snapshotValue)


    let private addSnapshot (id : AggregateId) (version : AggregateVersion, snapshotValue : 's) =
        addSnapshotObj (id, version) (typeof<'s>, box snapshotValue)


    let private getEventsAndSnapshot p id upper = 
        let init, lower =
            match getSnapshot id (NoBound, upper) with
            | None   -> (p.Init, NoBound)
            | Some s -> (s.Value, Limit (s.Version+1))
        let (evs, ver) = enumEvents id (lower,upper)
        (evs, ver, init)


    let private foldSnapshot p id upper =
        let (evs, version, init) = getEventsAndSnapshot p id upper
        let snapshot = evs |> Seq.fold p.Fold init
        snapshot, version, init


    let private getStream (id:AggregateId) = 
        { new IEventStream<'event> with

            member __.Add (event:'event) =
                addEvent id event
            member __.Read(p: Projection<'s,'event,'r>) (upper: VersionBound): 'r = 
                let (snapshot, _, _) = foldSnapshot p id upper
                snapshot |> p.Proj
            member __.TakeSnapshot (p: Projection<_,_,_>) (upper: VersionBound) =
                let (snapshot, version, _) = foldSnapshot p id upper
                addSnapshot id (version, snapshot)
            member __.Events () =
                enumEvents id (NoBound, NoBound)
                |> fst
                |> Seq.map (fun withMeta -> withMeta.Event)
        }


    let InMemory =
        { new IEventSource with
            member __.GetStream(id) = getStream id
        }    
