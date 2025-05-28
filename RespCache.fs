namespace GenBerk

open System.IO
open System.Collections.Generic

module RespCache =
    let mutable cache = ""
    let Save (rd:RespCacheDict) =
        let l2str (l:string list) =
            if l.IsEmpty then ""
            else
                l
                |>List.map (fun m -> "\"" + m + "\"")
                |>List.reduce (fun a b -> a + ";" + b)
        let lines =
            rd
            |> Seq.map(fun (KeyValue(k,v)) -> "\"" + k + "\",[" + (l2str v) + "]")
        if cache = "" then failwith "Cache file not defined"
        else File.WriteAllLines(cache,lines)
    let Load () =
        let ln2tuple (l:string) =
            let bits = l.Split(",")
            if bits.Length<>2 then failwith ("Invalid line in Cache:" + l)
            else
                let k = bits[0].Trim('\"')
                let v0:string = bits[1].Trim('[').Trim(']')
                if v0="" then 
                    k,[]
                else
                    let ms = v0.Split(";")
                    let v = ms|>Array.map(fun m -> m.Trim('\"'))|>Array.toList
                    k,v
        if cache = "" then failwith "Cache file not defined"
        else 
            if File.Exists(cache) then
                let lines = File.ReadAllLines(cache)
                lines|>Array.map ln2tuple|>dict|>Dictionary
            else new Dictionary<string, string list>()
