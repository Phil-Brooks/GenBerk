namespace GenBerk

open System.IO
open System.Collections.Generic

module BestCache =
    let mutable cache = ""
    let Save (bd:BestCacheDict) =
        let b2str (b:BestEntry) =
            "{Best:" + b.Best + ";Resp:" + b.Resp + ";Eval:" + b.Eval.ToString() + "}"
        let lines =
            bd
            |> Seq.map(fun (KeyValue(k,v)) -> "\"" + k + "\"," + (b2str v))
        if cache = "" then failwith "Cache file not defined"
        else File.WriteAllLines(cache,lines)
    let Load () =
        let ln2tuple (l:string) =
            let bits = l.Split(",")
            if bits.Length<>2 then failwith ("Invalid line in Cache:" + l)
            else
                let k = bits[0].Trim('\"')
                let v0:string = bits[1].Trim('{').Trim('}')
                let ms = v0.Split(";")
                if ms.Length<>3 then failwith ("Invalid line in Cache:" + l)
                let best = ms[0].Split(":")[1]
                let resp = ms[1].Split(":")[1]
                let eval = int(ms[2].Split(":")[1])
                let v = {Best=best;Resp=resp;Eval=eval}
                k,v
        if cache = "" then failwith "Cache file not defined"
        else 
            if File.Exists(cache) then
                let lines = File.ReadAllLines(cache)
                lines|>Array.map ln2tuple|>dict|>Dictionary
            else new Dictionary<string, BestEntry>()
