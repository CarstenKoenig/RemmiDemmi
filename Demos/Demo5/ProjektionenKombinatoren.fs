namespace DWX2016.EventSourcing

open System

module Kombinatoren =

    let createP f i p : Projection<_,_,_> =
        { Fold = f
        ; Init = i
        ; Proj = p }

    let fmap 
        (f : 'a -> 'b) 
        (pa : Projection<'s,'event,'a>) 
        : Projection<'s,'event,'b> =
        {
            Init = pa.Init
            Fold = pa.Fold
            Proj = pa.Proj >> f
        }

    type Pair<'a,'b> = { 
        First     : 'a
        FirstVer  : AggregateVersion
        Second    : 'b 
        SecondVer : AggregateVersion
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
            Fold = fun pair (ev, ver) ->
                match pair with
                | { First = sA; FirstVer = verA; Second = sB; SecondVer = verB } -> 
                    let fst = 
                        if ver > verA
                        then pa.Fold sA (ev, ver)
                        else sA
                    let snd = 
                        if ver > verB
                        then pb.Fold sB (ev, ver)
                        else sB
                    in { pair with First = fst; Second = snd }
        }

    let pureP value =
        {
            Init = ()
            Proj = fun _ -> value
            Fold = (fun _ _ -> ())
        }

    let aMap   
        (pf : Projection<'sf,'event,'a -> 'b>) 
        (pa : Projection<'sa,'event,'a>) 
        : Projection<Pair<'sf,'sa>,'event,'b> =
        parallelP (pf, pa)
        |> fmap (fun (f,a) -> f a)

    let (<*>) = aMap
    let (<*) f a = (pureP f) <*> a


    let firstP (select : 'event -> 'result option) (defResult : 'result) =
        let orElse alt opt = 
            match opt with
            | Some _ -> opt
            | None   -> alt
        createP 
            (fun opt (ev,_) -> opt |> orElse (select ev))
            None
            (function Some r -> r | None -> defResult)

    let lastP (select : 'event -> 'result option) (defResult : 'result) =
        let orElse alt opt = 
            match alt with
            | Some _ -> alt
            | None   -> opt
        createP 
            (fun opt (ev,_) -> opt |> orElse (select ev))
            (Some defResult)
            Option.get

    type Labeled<'lable,'a> = Labeled of 'a
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
            (fun opt (ev,_) -> opt |> orElse (select ev))
            (Some (Labeled defResult))
            (Option.get >> (fun (Labeled v) -> v))

    type Summe<'label,'a> = Summe of 'a

    let inline sumByP 
        (label : 'label) 
        (select : 'event -> 'num option) 
        : Projection<Summe<'label,'num>,_,'num> =
        createP
            (fun (Summe sum) (ev,_) ->
                match select ev with
                | Some nr -> Summe (sum + nr)
                | None    -> Summe sum)
            (Summe LanguagePrimitives.GenericZero)
            (fun (Summe num) -> num)

    let countByP 
        (label : 'label)
        (select : 'event -> bool)
        : Projection<Summe<'label,int>,_,int> =
        let toNum = 
            function 
            | true -> Some 1 
            | false -> None
        sumByP label (select >> toNum)