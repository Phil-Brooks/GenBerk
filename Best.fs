namespace GenBerk

open System.Collections.Generic

module Best =
    let mutable depth = 5
    let mutable bdict = new Dictionary<string, BestEntry>()

    let Setup(fil) = 
        BestCache.cache <- fil
        bdict <- BestCache.Load()

    let Get (fen:string) =
        if bdict.ContainsKey fen then bdict[fen]
        else
            let ans = Berserk.GetBestMove(fen,depth)
            bdict.Add(fen,ans)
            BestCache.Save(bdict)
            ans

