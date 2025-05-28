namespace GenBerk

open System.Collections.Generic

module Best =
    let mutable depth = 5
    let mutable bdict = new Dictionary<string, BestEntry>()

    let Setup(fil) = 
        BestCache.cache <- fil
        bdict <- BestCache.Load()


