open GenBerk

[<EntryPoint>]
let main argv =
    (*
    1. Set cache
    2. Set depth
    3. Set number of moves to add
    *)
    Rep.setcache()
    Best.depth <- 40
    let mnum = 10
    let gm0 = RegParse.ReadGame "base.pgn"
    let gm1 = Rep.AddWhite1st gm0
    //let num = mnum - (gm1.MoveText.Length/2)
    //let rec addresps ct igm =
    //    if ct = num then igm
    //    else
    //        let ogm = Rep.AddWhiteResps igm
    //        addresps (ct+1) ogm
    //let gmr = addresps 0 gm1
    //let gm = Rep.AddWhiteLast gmr
    //Rep.SaveWhite ch gm



    0
