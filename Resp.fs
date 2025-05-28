namespace GenBerk
open System.Collections.Generic

module Resp =
    let mutable rdict = new Dictionary<string, string list>()

    let Setup(fil) = 
        RespCache.cache <- fil
        rdict <- RespCache.Load()


