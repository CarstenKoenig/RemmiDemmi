namespace DWX2016.EventSourcing

open System

module Kombinatoren =

    let createP f i p : Projection<_,_,_> =
        { Fold = f
        ; Init = i
        ; Proj = p }

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
