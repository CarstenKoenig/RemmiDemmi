// Definitionen

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


type MetaData = {
    Id        : AggregateId
    Version   : AggregateVersion
    }

type WithMeta<'event> = {
    Event : 'event
    Meta  : MetaData
    }


type Projection<'snap, 'event, 'result> = {
    Fold : 'snap -> WithMeta<'event> -> 'snap
    Proj : 'snap -> 'result
    Init : 'snap
    }


type Snapshot<'s> = {
    AggregateId : AggregateId
    Version     : AggregateVersion
    Value : 's
    }


type IEventStream<'event> =
    abstract Add           : event:'event -> unit
    abstract TakeSnapshot  : p:Projection<'snap,'event,'res> -> upper:VersionBound -> unit
    abstract Read          : p:Projection<'snap,'event,'res> -> upper:VersionBound -> 'res
    abstract Events        : unit -> 'event seq


type IEventSource =
    abstract GetStream : id:AggregateId -> IEventStream<'event>


type Pair<'a,'b> = { 
    First     : 'a
    FirstVer  : AggregateVersion
    Second    : 'b 
    SecondVer : AggregateVersion
    }


type Labeled<'lable,'a> = Labeled of 'a


type Summe<'label,'a> = Summe of 'a


module Projektionen =

    let createP f i p : Projection<_,_,_> = {
        Fold = fun snap withMeta -> f snap withMeta.Event
        Init = i
        Proj = p
        }


    let fmapP 
        (f : 'a -> 'b) 
        (pa : Projection<'s,'event,'a>) 
        : Projection<'s,'event,'b> =
        {
            Init = pa.Init
            Fold = pa.Fold
            Proj = pa.Proj >> f
        }


    let parallelP
        ( pa : Projection<'sa,'event,'ra>
        , pb : Projection<'sb,'event,'rb>) 
        : Projection<Pair<'sa,'sb>,'event,'ra*'rb> =
        { 
            Init = 
                { First  = pa.Init
                ; FirstVer = 0
                ; Second = pb.Init 
                ; SecondVer = 0} 
            Proj = function 
                | { First = sA; Second = sB } -> 
                    (pa.Proj sA, pb.Proj sB)
            Fold = fun pair ev ->
                match pair with
                | { First = sA; FirstVer = verA; Second = sB; SecondVer = verB } -> 
                    let fst = 
                        if ev.Meta.Version > verA
                        then pa.Fold sA ev
                        else sA
                    let snd = 
                        if ev.Meta.Version > verB
                        then pb.Fold sB ev
                        else sB
                    in { pair with First = fst; Second = snd }
        }


    let pureP value =
        {
            Init = ()
            Proj = fun _ -> value
            Fold = (fun _ _ -> ())
        }


    let applictiveMapP   
        (pf : Projection<'sf,'event,'a -> 'b>) 
        (pa : Projection<'sa,'event,'a>) 
        : Projection<Pair<'sf,'sa>,'event,'b> =
        parallelP (pf, pa)
        |> fmapP (fun (f,a) -> f a)


    let (<*>) = applictiveMapP


    let (<*) f a = (pureP f) <*> a


    let firstP (select : 'event -> 'result option) (defResult : 'result) =
        let orElse alt opt = 
            match opt with
            | Some _ -> opt
            | None   -> alt
        createP 
            (fun opt ev -> opt |> orElse (select ev))
            None
            (function Some r -> r | None -> defResult)


    let lastP (select : 'event -> 'result option) (defResult : 'result) =
        let orElse alt opt = 
            match alt with
            | Some _ -> alt
            | None   -> opt
        createP 
            (fun opt ev -> opt |> orElse (select ev))
            (Some defResult)
            Option.get


    let lastLabeledP 
        (label : 'label)
        (select : 'event -> 'result option) 
        (defResult : 'result) 
        : Projection<Labeled<'label,'result> option,_,_> =
        let orElse alt opt = 
            match alt with
            | Some alt -> Some (Labeled alt)
            | None     -> opt
        createP 
            (fun opt ev -> opt |> orElse (select ev))
            (Some (Labeled defResult))
            (Option.get >> (fun (Labeled v) -> v))


    let inline sumByP 
        (label : 'label) 
        (select : 'event -> 'num option) 
        : Projection<Summe<'label,'num>, 'event, 'num> =
        createP
            (fun (Summe sum) ev ->
                match select ev with
                | Some nr -> Summe (sum + nr)
                | None    -> Summe sum)
            (Summe LanguagePrimitives.GenericZero)
            (fun (Summe num) -> num)


    let countByP 
        (label : 'label)
        (select : 'event -> bool)
        : Projection<Summe<'label,int>, 'event, int> =
        let toNum = 
            function 
            | true -> Some 1 
            | false -> None
        sumByP label (select >> toNum)        
