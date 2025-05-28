namespace GenBerk

open System.Text.RegularExpressions

module pMove =

    let CreateAll(mt,tgs,pc,orf,orr,pp,ic,id,im) =
        {Mtype=mt 
         TargetSquare=tgs
         Piece=pc
         OriginFile=orf
         OriginRank=orr
         PromotedPiece=pp
         IsCheck=ic
         IsDoubleCheck=id
         IsCheckMate=im}

    let CreateOrig(mt,tgs,pc,orf,orr) = CreateAll(mt,tgs,pc,orf,orr,None,false,false,false)

    let Create(mt,tgs,pc) = CreateOrig(mt,tgs,pc,None,None)

    let CreateCastle(mt) = CreateOrig(mt,OUTOFBOUNDS,None,None,None)
    
    let Parse(s : string) =
        //Active pattern to parse move string
        let (|SimpleMove|Castle|PawnCapture|AmbiguousFile|AmbiguousRank|Promotion|PromCapture|) (s:string) =
            if Regex.IsMatch(s, "^[BNRQK][a-h][1-8]$") then 
                SimpleMove(s.[0]|>PieceType.Parse, s.[1..]|>Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8]$") then SimpleMove(PieceType.Pawn, s|>Square.Parse)
            elif s = "O-O" then Castle('K')
            elif s = "O-O-O" then Castle('Q')
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8]$") then 
                PawnCapture(s.[0]|>File.Parse, s.[1..]|>Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][a-h][a-h][1-8]$") then 
                AmbiguousFile(s.[0]|>PieceType.Parse, s.[1]|>File.Parse, s.[2..]|>Square.Parse)
            elif Regex.IsMatch(s, "^[BNRQK][1-8][a-h][1-8]$") then 
                AmbiguousRank(s.[0]|>PieceType.Parse, s.[1]|>Rank.Parse, s.[2..]|>Square.Parse)
            elif Regex.IsMatch(s, "^[a-h][1-8][BNRQ]$") then 
                Promotion(s.[0..1]|>Square.Parse, s.[2]|>PieceType.Parse)
            elif Regex.IsMatch(s, "^[a-h][a-h][1-8][BNRQ]$") then 
                PromCapture(s.[0]|>File.Parse, s.[1..2]|>Square.Parse, s.[3]|>PieceType.Parse)
            else failwith ("invalid move: " + s)

        //general failure message
        let fl() =
            failwith ("not done yet, mv: " + s)

        let strip chars =
            String.collect (fun c -> 
                if Seq.exists ((=) c) chars then ""
                else string c)
          
        let m = s |> strip "+x#="|>fun x ->x.Replace("e.p.", "")
        
        let mv0 =
            match m with
            | SimpleMove(p, sq) -> 
                Create((if s.Contains("x") then MoveType.Capture else MoveType.Simple),sq,Some(p))
            | Castle(c) ->
                CreateCastle(if c='K' then MoveType.CastleKingSide else MoveType.CastleQueenSide)
            | PawnCapture(f, sq) -> 
                CreateOrig(MoveType.Capture,sq,Some(PieceType.Pawn),Some(f),None)
            | AmbiguousFile(p, f, sq) -> 
                CreateOrig((if s.Contains("x") then MoveType.Capture else MoveType.Simple),sq,Some(p),Some(f),None)
            | AmbiguousRank(p, r, sq) -> 
                CreateOrig((if s.Contains("x") then MoveType.Capture else MoveType.Simple),sq,Some(p),None,Some(r))
            | Promotion(sq, p) -> 
                CreateAll(MoveType.Simple,sq,Some(PieceType.Pawn),None,None,Some(p),false,false,false)
            | PromCapture(f, sq, p) -> 
                CreateAll(MoveType.Capture,sq,Some(PieceType.Pawn),Some(f),None,Some(p),false,false,false)
      
        let mv1 =
            if s.Contains("++") then {mv0 with IsDoubleCheck=true} 
            elif s.Contains("+") then {mv0 with IsCheck=true}
            elif s.Contains("#") then {mv0 with IsCheckMate=true}
            else mv0
        
        mv1
    
