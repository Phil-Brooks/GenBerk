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
    7. Expand specified number of moves
    8. Add last moves for each variation
    9. Save new pgn
    *)
    Rep.setcache()
    Best.depth <- 20
    let mnum = 11
    let gmi = RegParse.ReadGame "base.pgn"
    let gm0 = Game.SetaMoves gmi
    let isw = gm0.BlackPlayer = ""||gm0.BlackPlayer = "?" 
    let gm1 = 
        if isw then
            Rep.AddWhite1st gm0
        else Rep.AddBlack1st gm0
    let num = mnum - (gm1.MoveText.Length/2)
    let rec addresps ct igm =
        if ct = num then igm
        else
            let ogm = 
                if isw then
                    Rep.AddWhiteResps igm
                else Rep.AddBlackResps igm
            addresps (ct+1) ogm
    let gmr = addresps 0 gm1
    let gm = 
       if isw then Rep.AddWhiteLast gmr
       else Rep.AddBlackLast gmr
    Rep.Save "berk.pgn" gm
    0
