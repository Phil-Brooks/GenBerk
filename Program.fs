open GenBerk

[<EntryPoint>]
let main argv =
    (*
    1. Set cache
    2. Set depth
    3. Set number of moves to add
    4. Read game
    5. Check colour
    6. Add 1st additional moves
    *)
    Rep.setcache()
    Best.depth <- 10
    let mnum = 10
    let gmi = RegParse.ReadGame "base.pgn"
    let gm0 = Game.SetaMoves gmi
    let isw = gm0.BlackPlayer = ""||gm0.BlackPlayer = "?" 
    let gm1 = 
        if isw then
            Rep.AddWhite1st gm0
        else Rep.AddBlack1st gm0
    let num = mnum - (gm1.MoveText.Length/2)
    //let rec addresps ct igm =
    //    if ct = num then igm
    //    else
    //        let ogm = Rep.AddWhiteResps igm
    //        addresps (ct+1) ogm
    //let gmr = addresps 0 gm1
    //let gm = Rep.AddWhiteLast gmr
    Rep.Save "berk.pgn" gm1



    0
