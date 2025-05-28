namespace GenBerk

open System.Threading
open System.Collections.Generic

module Resp =
    let mutable rdict = new Dictionary<string, string list>()
    let Setup(fil) = 
        RespCache.cache <- fil
        rdict <- RespCache.Load()
    let LiGet (fen:string) =
        let rec tryget ct =
            try
                let res = OpenExp.Results.Load(OpenExp.addr + fen)
                let tot = res.White + res.Draws + res.Black
                let mvs = res.Moves
                //only pick > 25%
                let sans = 
                    mvs
                    |>Array.map(fun m -> float(m.White + m.Draws + m.Black)/float(tot),m.San)
                    |>Array.filter(fun (p,s) -> p > 0.25)
                    |>Array.map snd
                sans|>List.ofArray
            with
                | ex ->
                    printfn "Fail probably 429, count: %i"  ct
                    if ct<4 then
                        Thread.Sleep(ct*100)
                        tryget (ct+1)
                    else failwith"too many tries"
        tryget 1
    let Get (fen:string) =
        if rdict.ContainsKey fen then rdict[fen]
        else
            let ans = LiGet fen
            rdict.Add(fen,ans)
            RespCache.Save(rdict)
            ans
