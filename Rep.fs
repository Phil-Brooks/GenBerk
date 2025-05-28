namespace GenBerk

module Rep =
    let setcache() = 
        Best.Setup("BestCache")
        Resp.Setup("RespCache")

    let Save (nm:string) (gm:Game) =
        PgnWriter.WriteGame nm gm
    
    let AddWhite1st (gm:Game) =
        //Just go to end of each var and add a set of Ravs using resp cache
        let rec add1st hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
            if imtel.IsEmpty then 
                failwith "last move should be a GameEndEntry"
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) -> 
                    let getrav brd (resp:string) =
                        let pmv = pMove.Parse resp
                        let amv = pMove.ToaMove brd (hmn/2) pmv
                        let mte = HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                        RAVEntry([mte])
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let resps = Resp.Get fen
                    if resps.Length=0 then failwith"got no resps"
                    else
                        let pmv = pMove.Parse resps.Head
                        let amv = pMove.ToaMove brd (hmn/2) pmv
                        let nmte = HalfMoveEntry(None,false,pmv,Some(amv))
                        if resps.Length=1 then
                            (mte::nmte::omtel)|>List.rev
                        else
                            let ravs = resps.Tail|>List.map(getrav brd)|>List.rev
                            (mte::(ravs@[nmte]@omtel))|>List.rev
                |HalfMoveEntry(_,_,_,amv) -> add1st (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> add1st hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = add1st 2 None gm.MoveText []
        {gm with MoveText=nmt}

    let AddBlack1st (gm:Game) =
        //Just go to end of each var and add a set of Ravs using resp cache
        let rec add1st hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
            if imtel.IsEmpty then 
                failwith "last move should be a GameEndEntry"
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) -> 
                    let getrav brd (resp:string) =
                        let pmv = pMove.Parse resp
                        let amv = pMove.ToaMove brd (hmn/2) pmv
                        let mte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                        RAVEntry([mte])
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let resps = Resp.Get fen
                    if resps.Length=0 then failwith"got no resps"
                    else
                        let pmv = pMove.Parse resps.Head
                        let amv = pMove.ToaMove brd (hmn/2) pmv
                        let nmte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                        if resps.Length=1 then
                            (mte::nmte::omtel)|>List.rev
                        else
                            let ravs = resps.Tail|>List.map(getrav brd)|>List.rev
                            (mte::(ravs@[nmte]@omtel))|>List.rev
                |HalfMoveEntry(_,_,_,amv) -> add1st (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> add1st hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = add1st 2 None gm.MoveText []
        {gm with MoveText=nmt}


