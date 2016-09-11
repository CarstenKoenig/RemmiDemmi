namespace DWX2016.EventSourcing

open System

type AggregateId = Guid
type AggregateVersion = int

type VersionBound =
    | NoBound
    | Limit of AggregateVersion
    member this.ToUpperValue =
        match this with
        | NoBound -> Int32.MaxValue
        | Limit v -> v
    override this.ToString() =
        match this with
        | NoBound -> "-"
        | Limit v -> string v

type Projection<'s,'event,'result> = {
    Fold : 's -> 'event * AggregateVersion -> 's
    Proj : 's -> 'result
    Init : 's
    }

type Snapshot<'s> = {
    AggregateId : AggregateId
    Version     : AggregateVersion
    Value : 's
    }

type IEventStream =
    abstract Add           : event:'event -> unit
    abstract TakeSnapshot  : p:Projection<'s,'e,'r> -> upper:VersionBound -> unit
    abstract Read          : p:Projection<'s,'e,'r> -> upper:VersionBound -> 'r

type IEventSource =
    abstract GetStream : id:AggregateId -> IEventStream
