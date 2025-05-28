namespace GenBerk

module DateUtil = 
    
    let FromStr(dtstr:string) =
        let a = dtstr.Split([|'.'|])|>Array.map(fun s -> s.Trim())
        let y,m,d = if a.Length=3 then a.[0],a.[1],a.[2] else a.[0],"??","??"
        let yop = if y="????" then None else Some(int y)
        let mop = if m="??" then None else Some(int m)
        let dop = if d="??" then None else Some(int d)
        yop,mop,dop
