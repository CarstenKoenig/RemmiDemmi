namespace DWX2016.EventSourcing

module Projections =
    
    /// used by the pair combinator to zip two projections
    type Pair<'a,'b> = { 
        First : 'a
        FirstVersion : int
        Second : 'b 
        SecondVersion : int
        }

    /// combines two projections into a single returing the pair of their results
    /// notice that the fold uses the event-version to only fold the components if they have newer versions
    /// that's because when using snapshots the current versions for the components might differ!
    let pair ( pa : Projection<'sa,'event,'ra>
             , pb: Projection<'sb,'event,'rb>) 
             : Projection<Pair<'sa,'sb>,'event,'ra*'rb> =
        { 
            Init = 
                { 
                    First = pa.Init
                    FirstVersion = 0
                    Second = pb.Init
                    SecondVersion = 0 
                } 
            Proj = fun pair -> pa.Proj pair.First, pb.Proj pair.Second
            Fold = fun pair (ev, ver) ->
                let fst = if ver > pair.FirstVersion 
                            then pa.Fold pair.First (ev,ver) 
                            else pair.First
                let snd = if ver > pair.SecondVersion 
                            then pb.Fold pair.Second (ev,ver) 
                            else pair.Second
                { pair with First = fst; Second = snd }
        }

    // ****************************************************************************************************************

    /// constant projection - always returns the given value
    let constant value =
        {
            Init = ()
            Proj = fun _ -> value
            Fold = (fun _ _ -> ())
        }

    let create init fold =
        {
            Init = init
            Proj = id
            Fold = (fun state (event,_) -> fold state event)
        }

    /// quasi-functor map
    let fmap (f : 'a -> 'b) (pa : Projection<'s,'event,'a>) : Projection<'s,'event,'b> =
        {
            Init = pa.Init
            Fold = pa.Fold
            Proj = pa.Proj >> f
        }

    /// quasi-applicative apply
    let aAppl   (pf : Projection<'sf,'event,'a -> 'b>) 
                (pa : Projection<'sa,'event,'a>) 
                : Projection<Pair<'sf,'sa>,'event,'b> =
        pair (pf, pa)
        |> fmap (fun (f,a) -> f a)

    // ****************************************************************************************************************

    let latestPartial (selector : 'event -> 'b option)
                      : Projection<'b option,'event,'b option> =
        let orElse d o =
            if Option.isSome o
                then 
                    #if trace
                    printf "\n\t\t%s option <- %A" (typeof<'b>.Name) o.Value 
                    #endif
                    o
                else 
                    #if trace
                    printf "\n\t\t%s option skipped" (typeof<'b>.Name)
                    #endif
                    d
        {
            Init = None
            Proj = id
            Fold = (fun s (ev,_) -> selector ev |> orElse s)
        }

    let latest (init : 'b) 
               (selector : 'event -> 'b option)
               : Projection<'b,'event,'b> =
        let orDefault d =
            function
            | Some o -> 
                #if trace
                printf "\n\t\t%s <- %A" (typeof<'b>.Name) o
                #endif
                o
            | None   -> 
                #if trace
                printf "\n\t\t%s skipped" (typeof<'b>.Name)
                #endif
                d
        {
            Init = init
            Proj = id
            Fold = (fun s (ev,_) -> selector ev |> orDefault s)
        }

    type Sum<'label,'b> = Sum of 'b

    /// projection to sum the Some-results of the value map
    let inline sumBy (label : 'label) (value : 'event -> 'b option) 
        : Projection<Sum<'label,'b>,'event,'b>=
        {
            Init = Sum LanguagePrimitives.GenericZero 
            Proj = fun (Sum sum) -> sum
            Fold = (fun (Sum sum) (ev, _) -> 
                match value ev with 
                | Some v -> 
                    #if trace
                    printf "\n\t\t+%O auf %s" v (typeof<'label>.Name) 
                    #endif
                    Sum (sum + v)
                | _      -> 
                    #if trace
                    printf "\n\t\tskipped %s" (typeof<'label>.Name) 
                    #endif
                    Sum sum)
        }

    // ****************************************************************************************************************
    // Average

    type Average<'label, 'a> =
        { 
            Sum   : 'a
            Count : int
        }

    /// average of the Some-results of the value map
    let inline averageBy (label : 'label) (value : 'event -> 'a option) 
        : Projection<Average<'label,'a>,'event,'a> =
        {
            Init = { Sum = LanguagePrimitives.GenericZero; Count = 0 }
            Proj = fun avg -> LanguagePrimitives.DivideByInt avg.Sum avg.Count
            Fold = (fun avg (ev, _) -> 
                match value ev with 
                | Some v -> 
                    #if trace
                    printf "\n\t\t+%O auf %s"  v (typeof<'label>.Name) 
                    #endif
                    { avg with Sum = avg.Sum + v; Count = avg.Count + 1 }
                | _      -> 
                    #if trace
                    printf "\n\t\tskipped %s" (typeof<'label>.Name) 
                    #endif
                    avg)
        }
[<AutoOpen>]
module ProjectionOperators =
    let (<*>) = Projections.aAppl