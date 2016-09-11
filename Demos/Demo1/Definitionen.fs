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


type IEventStream =
    abstract Add       : event:'event -> unit
    abstract Enumerate : upper:VersionBound -> 'event list

type IEventSource =
    abstract GetStream : id:AggregateId -> IEventStream
