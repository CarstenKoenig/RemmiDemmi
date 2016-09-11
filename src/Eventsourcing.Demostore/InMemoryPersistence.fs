namespace DWX2016.EventSourcing

module InMemoryPersistence =
    open PersistenceContracts

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
        #if trace
        printfn "\t* Enumerating Events\n\t\t-> Bounds %O to %O" lower upper
        #endif
        let inBounds = inBounds (lower,upper)
        let filtered = events |> Seq.filter (fun r -> r.AggregateId  = id && inBounds r.Version) |> Seq.toArray
        let version = if filtered.Length = 0 then 0 else (Seq.last filtered).Version
        (filtered |> Seq.map (fun r -> unbox r.Event, r.Version), version)

    let private addEvent (id : AggregateId) (ev : 'event) =
        let version = latestVersion id + 1
        { AggregateId = id; Version = version; Event = box ev }
        |> events.Add

    let private eventsImpl = 
        { new IEvents with
            member __.AddEvent (id:AggregateId) (event:'event) =
                addEvent id event
            member __.EnumerateEvents(id: AggregateId) (lower: VersionBound, upper: VersionBound) = 
                enumEvents id (lower,upper)
        }

    let private snapshots = System.Collections.Generic.Dictionary<System.Type*AggregateId, AggregateVersion*obj>()

    let rec private getSnapshotObj (id:AggregateId) (lower:VersionBound, upper:VersionBound) (snapshotType : System.Type) : (AggregateVersion*obj) option =
        if snapshotType = typeof<unit> then Some (upper.ToUpperValue, box ()) else
        match GenericPair.tryGetTypes snapshotType with
        | Some (ta, tb) ->
            getSnapshotObj id (lower, upper) ta
            |> Option.bind (fun (va, oa) ->
                getSnapshotObj id (lower, upper) tb
                |> Option.map (fun (vb, ob) ->
                    (min va vb, GenericPair.make (ta, tb, oa, ob, va, vb))))
        | None ->
            let key = snapshotType, id
            match snapshots.TryGetValue key with
            | (true, (ver, o)) when ver |> inBounds (NoBound, upper) -> 
                #if trace
                printfn "\t\t= got Snapshot for %s - %O" (Tracing.describe snapshotType) ver
                #endif
                Some (ver, o)
            | _ -> 
                #if trace
                printfn "\t\t= no Snapshot for %s" (Tracing.describe snapshotType)
                #endif
                None

    let private getSnapshot (id:AggregateId) (lower:VersionBound, upper:VersionBound) : Snapshot<'s> option =
        #if trace
        printfn "\t* Fetching Snapshot for\n\t\t-> %s\n\t\t-> Bounds %O to %O" (Tracing.describe typeof<'s>) lower upper
        #endif
        match getSnapshotObj id (lower,upper) typeof<'s> with
        | None          -> 
            None
        | Some (ver, o) -> 
            Some { AggregateId = id; Version = ver; Value = unbox o }

    let rec private addSnapshotObj (id:AggregateId, version : AggregateVersion) (snapshotType : System.Type, snapshotValue : obj) =
        if snapshotType = typeof<unit> then () else
        match GenericPair.tryGetValues snapshotValue with
        | Some (ta, tb, va, vb) ->
            addSnapshotObj (id, version) (ta, va)
            addSnapshotObj (id, version) (tb, vb)
        | None ->
            let key = snapshotType, id
            #if trace
            printfn "\t* writing snapshot\n\t\t-> %s\n\t\t-> Version %O" (Tracing.describe snapshotType) version
            #endif
            snapshots.[key] <- (version, snapshotValue)

    let private addSnapshot (id : AggregateId) (version : AggregateVersion, snapshotValue : 's) =
        addSnapshotObj (id, version) (typeof<'s>, box snapshotValue)

    let internal snapshotsImpl = 
        { new ISnapshots with
              member __.AddSnapshot id version snapshot = 
                addSnapshot id (version, snapshot)
              
              member __.GetSnapshot id (lower, upper) =
                getSnapshot id (lower, upper)
        }

    let EventSource =
        createEventSourceFrom (eventsImpl, snapshotsImpl)