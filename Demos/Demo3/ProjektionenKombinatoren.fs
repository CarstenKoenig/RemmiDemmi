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
        First : 'a
        Second : 'b 
        }

    let parallelP
        ( pa : Projection<'sa,'event,'ra>
        , pb : Projection<'sb,'event,'rb>) 
        : Projection<Pair<'sa,'sb>,'event,'ra*'rb> =
        { 
            Init = { First = pa.Init; Second = pb.Init } 
            Proj = fun pair -> pa.Proj pair.First, pb.Proj pair.Second
            Fold = fun pair ev ->
                let fst = pa.Fold pair.First ev
                let snd = pb.Fold pair.Second ev
                { pair with First = fst; Second = snd }
        }

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

    let inline sumByP (select : 'event -> 'num option) =
        createP
            (fun sum ev ->
                match select ev with
                | Some nr -> sum + nr
                | None    -> sum)
            LanguagePrimitives.GenericZero
            id

    let countByP (select : 'event -> bool) =
        let toNum = 
            function 
            | true -> Some 1 
            | false -> None
        sumByP (select >> toNum)