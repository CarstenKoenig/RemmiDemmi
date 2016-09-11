namespace DWX2016.EventSourcing

open System
module EventStore =

    type Record<'event> = {
        AggregateId : AggregateId
        Version     : AggregateVersion
        Event       : 'event
        }

    let private events = System.Collections.Generic.List<Record<obj>>()
    let private latestVersion (id : AggregateId) =
        events 
        |> Seq.filter (fun r -> r.AggregateId = id)
        |> Seq.tryLast
        |> function Some r -> r.Version | None -> 0

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

    let private enumEvents (id:AggregateId) (lower:VersionBound, upper:VersionBound) : ('event * AggregateVersion) seq * AggregateVersion =
        let inBounds = inBounds (lower,upper)
        let filtered = events |> Seq.filter (fun r -> r.AggregateId  = id && inBounds r.Version) |> Seq.toArray
        let version = if filtered.Length = 0 then 0 else (Seq.last filtered).Version
        // (filtered |> Seq.map (fun r -> printfn "reading %d" r.Version; unbox r.Event, r.Version), version)
        (filtered |> Seq.map (fun r -> unbox r.Event, r.Version), version)

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

    let private getEvents p id upper = 
        let init, lower =
            match getSnapshot id (NoBound, upper) with
            | None   -> (p.Init, NoBound)
            | Some s -> (s.Value, Limit (s.Version+1))
        let (evs, ver) = enumEvents id (lower,upper)
        (evs, ver, init)

    let private readSnapshot p id upper =
        let (evs, version, init) = getEvents p id upper
        let snapshot = evs |> Seq.fold p.Fold init
        snapshot, version, init

    let private getStream (id:AggregateId) = 
        { new IEventStream with

            member __.Add (event:'event) =
                addEvent id event
            member __.Read(p: Projection<'s,'e,'r>) (upper: VersionBound): 'r = 
                let (snapshot, _, _) = readSnapshot p id upper
                snapshot |> p.Proj
            member __.TakeSnapshot (p: Projection<_,_,_>) (upper: VersionBound) =
                let (snapshot, version, _) = readSnapshot p id upper
                addSnapshot id (version, snapshot)
        }

    let InMemory =
        { new IEventSource with
            member __.GetStream(id) = getStream id
        }
