namespace DWX2016.EventSourcing

module PersistenceContracts =

    type Record<'event> = {
        AggregateId : AggregateId
        Version     : AggregateVersion
        Event       : 'event
        }

    type IEvents =
        abstract AddEvent        : id:AggregateId -> event:'event -> unit
        abstract EnumerateEvents : id:AggregateId -> lower:VersionBound * upper:VersionBound -> ('event * AggregateVersion) seq * AggregateVersion

    type ISnapshots =
        abstract GetSnapshot : id:AggregateId -> lower:VersionBound * upper:VersionBound -> Snapshot<'s> option
        abstract AddSnapshot : id:AggregateId -> version:AggregateVersion -> 's -> unit

    let createEventSourceFrom (events : IEvents, snapshots : ISnapshots) =
        let getEvents p id upper = 
            let init, lower =
                match snapshots.GetSnapshot id (NoBound, upper) with
                | None   -> (p.Init, NoBound)
                | Some s -> (s.Value, Limit (s.Version+1))
            let (evs, ver) = events.EnumerateEvents id (lower,upper)
            (evs, ver, init)
        let snapshot p id upper = 
            let fold s (ev,ver) =
                #if trace
                printf "\t* folding Ver %i:" ver
                #endif
                let res = p.Fold s (ev,ver)
                #if trace
                printfn ""
                #endif
                res

            let (evs, version, init) = getEvents p id upper
            let snapshot = evs |> Seq.fold fold init
            snapshot, version, init
        let getStream id = 
            { new IEventStream with
                member __.AddEvent(event) =
                    events.AddEvent id event
                member __.MakeSnapshot p upper =
                    #if trace
                    printfn "\n***\ncreating Snapshot for aggregate %A" id
                    #endif
                    let (snapshot, version, _) = snapshot p id upper
                    snapshots.AddSnapshot id version snapshot
                    #if trace
                    printfn "***\n"
                    #endif
                member __.Read p upper =
                    #if trace
                    printfn "\n***\nreading projection for aggregate %A" id
                    #endif
                    let (snapshot, _, _) = snapshot p id upper
                    #if trace
                    printfn "***\n"
                    #endif
                    snapshot |> p.Proj
            }                    
        { new IEventSource with
            member __.GetStream(id) = getStream id
        }