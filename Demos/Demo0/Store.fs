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

    let private getStream (id:AggregateId) = 
        { new IEventStream with
            member __.Add (event:'event) =
                addEvent id event
            member __.Enumerate (upper: VersionBound) = 
                enumEvents id (NoBound, upper)
                |> fst
                |> Seq.map fst
                |> Seq.toList
        }

    let InMemory =
        { new IEventSource with
            member __.GetStream(id) = getStream id
        }
