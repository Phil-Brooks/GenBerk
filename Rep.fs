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
    let AddWhiteResps (gm:Game) =
        //Just go to end of each var and add best move using best cache 
        //and then list of moves using resp cache with diags
        let rec addresps hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
           if imtel.IsEmpty then 
                //this is end of a RAV so can add here
                let brd = camv.Value.PostBrd
                let fen = FEN.FromBd brd|>FEN.ToStr
                let bm = Best.Get fen
                let pmv = pMove.Parse bm.Best
                let amv = pMove.ToaMove brd (hmn/2) pmv
                let nmte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                let nomtel = nmte::omtel
                let getrav brd (resp:string) =
                    let pmv = pMove.Parse resp
                    let amv = pMove.ToaMove brd (hmn/2) pmv
                    let mte = HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                    RAVEntry([mte])
                let nbrd = amv.PostBrd
                let fen = FEN.FromBd nbrd|>FEN.ToStr
                let iresps = Resp.Get fen
                let tresps = iresps|>List.filter(fun r -> r<>bm.Resp)
                let resps = bm.Resp::tresps
                let pmv = pMove.Parse resps.Head
                let amv = pMove.ToaMove nbrd (hmn/2) pmv
                let nmte = HalfMoveEntry(None,false,pmv,Some(amv))
                if resps.Length=1 then
                    (nmte::nomtel)|>List.rev
                else
                    let ravs = resps.Tail|>List.map(getrav nbrd)|>List.rev
                    (ravs@[nmte]@nomtel)|>List.rev
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) ->
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let bm = Best.Get fen
                    let pmv = pMove.Parse bm.Best
                    let amv = pMove.ToaMove brd (hmn/2) pmv
                    let nmte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                    let nomtel = nmte::omtel
                    let getrav brd (resp:string) =
                        let pmv = pMove.Parse resp
                        let amv = pMove.ToaMove brd (hmn/2) pmv
                        let mte = HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                        RAVEntry([mte])
                    let nbrd = amv.PostBrd
                    let fen = FEN.FromBd nbrd|>FEN.ToStr
                    let iresps = Resp.Get fen
                    let tresps = iresps|>List.filter(fun r -> r<>bm.Resp)
                    let resps = bm.Resp::tresps
                    let pmv = pMove.Parse resps.Head
                    let amv = pMove.ToaMove nbrd (hmn/2) pmv
                    let nmte = HalfMoveEntry(None,false,pmv,Some(amv))
                    if resps.Length=1 then
                        (mte::nmte::nomtel)|>List.rev
                    else
                        let ravs = resps.Tail|>List.map(getrav nbrd)|>List.rev
                        (mte::(ravs@[nmte]@nomtel))|>List.rev
                |RAVEntry(rml) -> 
                    let nrml = addresps (hmn-1) camv rml []
                    let nrav = RAVEntry(nrml)
                    addresps hmn camv imtel.Tail (nrav::omtel)
                |HalfMoveEntry(_,_,_,amv) -> addresps (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> addresps hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = addresps 2 None gm.MoveText []
        {gm with MoveText=nmt}
    let AddBlackResps (gm:Game) =
        //Just go to end of each var and add best move using best cache 
        //and then list of moves using resp cache with diags
        let rec addresps hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
           if imtel.IsEmpty then 
                //this is end of a RAV so can add here
                let brd = camv.Value.PostBrd
                let fen = FEN.FromBd brd|>FEN.ToStr
                let bm = Best.Get fen
                let pmv = pMove.Parse bm.Best
                let amv = pMove.ToaMove brd (hmn/2) pmv
                let nmte = 
                    match omtel.Head with
                    |RAVEntry(_) -> HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                    |_ -> HalfMoveEntry(None,false,pmv,Some(amv))
                let nomtel = nmte::omtel
                let getrav brd (resp:string) =
                    let pmv = pMove.Parse resp
                    let amv = pMove.ToaMove brd ((hmn+1)/2) pmv
                    let mte = HalfMoveEntry(Some((hmn+1)/2),false,pmv,Some(amv))
                    RAVEntry([mte])
                let nbrd = amv.PostBrd
                let fen = FEN.FromBd nbrd|>FEN.ToStr
                let iresps = Resp.Get fen
                let tresps = iresps|>List.filter(fun r -> r<>bm.Resp)
                let resps = bm.Resp::tresps
                let pmv = pMove.Parse resps.Head
                let amv = pMove.ToaMove nbrd ((hmn+1)/2) pmv
                let nmte = HalfMoveEntry(Some((hmn+1)/2),false,pmv,Some(amv))
                if resps.Length=1 then
                    (nmte::nomtel)|>List.rev
                else
                    let ravs = resps.Tail|>List.map(getrav nbrd)|>List.rev
                    (ravs@[nmte]@nomtel)|>List.rev
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) ->
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let bm = Best.Get fen
                    let pmv = pMove.Parse bm.Best
                    let amv = pMove.ToaMove brd (hmn/2) pmv
                    let nmte = 
                        match omtel.Head with
                        |RAVEntry(_) -> HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                        |_ -> HalfMoveEntry(None,false,pmv,Some(amv))
                    let nomtel = nmte::omtel
                    let getrav brd (resp:string) =
                        let pmv = pMove.Parse resp
                        let amv = pMove.ToaMove brd ((hmn+1)/2) pmv
                        let mte = HalfMoveEntry(Some((hmn+1)/2),true,pmv,Some(amv))
                        RAVEntry([mte])
                    let nbrd = amv.PostBrd
                    let fen = FEN.FromBd nbrd|>FEN.ToStr
                    let iresps = Resp.Get fen
                    let tresps = iresps|>List.filter(fun r -> r<>bm.Resp)
                    let resps = bm.Resp::tresps
                    let pmv = pMove.Parse resps.Head
                    let amv = pMove.ToaMove nbrd (hmn/2) pmv
                    let nmte = HalfMoveEntry(Some((hmn+1)/2),false,pmv,Some(amv))
                    if resps.Length=1 then
                        (mte::nmte::nomtel)|>List.rev
                    else
                        let ravs = resps.Tail|>List.map(getrav nbrd)|>List.rev
                        (mte::(ravs@[nmte]@nomtel))|>List.rev
                |RAVEntry(rml) -> 
                    let nrml = addresps (hmn-1) camv rml []
                    let nrav = RAVEntry(nrml)
                    addresps hmn camv imtel.Tail (nrav::omtel)
                |HalfMoveEntry(_,_,_,amv) -> addresps (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> addresps hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = addresps 2 None gm.MoveText []
        {gm with MoveText=nmt}
    let GetNag (eval:int) =
        if eval > -50 && eval < 50 then NAGEntry(NAG.Even)
        elif eval > 0 then
            if eval < 150 then NAGEntry(NAG.Wslight) 
            elif eval < 250 then NAGEntry(NAG.Wmoderate) 
            else NAGEntry(NAG.Wdecisive)
        else
            if eval > -150 then NAGEntry(NAG.Bslight) 
            elif eval > -250 then NAGEntry(NAG.Bmoderate) 
            else NAGEntry(NAG.Bdecisive)
    let AddWhiteLast (gm:Game) =
        //Just go to end of each var and add best move using best cache 
        //and then nag based on eval and a diagram
        let rec addlast hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
           if imtel.IsEmpty then 
                //this is end of a RAV so can add here
                let brd = camv.Value.PostBrd
                let fen = FEN.FromBd brd|>FEN.ToStr
                let bm = Best.Get fen
                let pmv = pMove.Parse bm.Best
                let amv = pMove.ToaMove brd (hmn/2) pmv
                let nmte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                let ng = GetNag(bm.Eval)
                let nomtel = ng::nmte::omtel
                nomtel|>List.rev
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) ->
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let bm = Best.Get fen
                    let pmv = pMove.Parse bm.Best
                    let amv = pMove.ToaMove brd (hmn/2) pmv
                    let nmte = HalfMoveEntry(Some(hmn/2),false,pmv,Some(amv))
                    let ng = GetNag(bm.Eval)
                    let nomtel = mte::ng::nmte::omtel
                    nomtel|>List.rev
                |RAVEntry(rml) -> 
                    let nrml = addlast (hmn-1) camv rml []
                    let nrav = RAVEntry(nrml)
                    addlast hmn camv imtel.Tail (nrav::omtel)
                |HalfMoveEntry(_,_,_,amv) -> addlast (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> addlast hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = addlast 2 None gm.MoveText []
        {gm with MoveText=nmt}
    let AddBlackLast (gm:Game) =
        //Just go to end of each var and add best move using best cache 
        //and then nag based on eval and a diagram
        let rec addlast hmn (camv:aMove option) (imtel:MoveTextEntry list) (omtel:MoveTextEntry list) =
           if imtel.IsEmpty then 
                //this is end of a RAV so can add here
                let brd = camv.Value.PostBrd
                let fen = FEN.FromBd brd|>FEN.ToStr
                let bm = Best.Get fen
                let pmv = pMove.Parse bm.Best
                let amv = pMove.ToaMove brd (hmn/2) pmv
                let nmte = 
                    match omtel.Head with
                    |RAVEntry(_) -> HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                    |_ -> HalfMoveEntry(None,false,pmv,Some(amv))
                let ng = GetNag(-bm.Eval)
                let nomtel = ng::nmte::omtel
                nomtel|>List.rev
            else
                let mte = imtel.Head
                match mte with
                |GameEndEntry(_) ->
                    let brd = camv.Value.PostBrd
                    let fen = FEN.FromBd brd|>FEN.ToStr
                    let bm = Best.Get fen
                    let pmv = pMove.Parse bm.Best
                    let amv = pMove.ToaMove brd (hmn/2) pmv
                    let nmte = 
                        match omtel.Head with
                        |RAVEntry(_) -> HalfMoveEntry(Some(hmn/2),true,pmv,Some(amv))
                        |_ -> HalfMoveEntry(None,false,pmv,Some(amv))
                    let ng = GetNag(-bm.Eval)
                    let nomtel = mte::ng::nmte::omtel
                    nomtel|>List.rev
                |RAVEntry(rml) -> 
                    let nrml = addlast (hmn-1) camv rml []
                    let nrav = RAVEntry(nrml)
                    addlast hmn camv imtel.Tail (nrav::omtel)
                |HalfMoveEntry(_,_,_,amv) -> addlast (hmn+1) amv imtel.Tail (imtel.Head::omtel)
                |_ -> addlast hmn camv imtel.Tail (imtel.Head::omtel)
        let nmt = addlast 2 None gm.MoveText []
        {gm with MoveText=nmt}
