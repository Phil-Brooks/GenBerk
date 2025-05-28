namespace GenBerk

open System.IO

module PgnWrite =
    let ResultString = GameResult.ToStr
    let Piece(pieceType: PieceType option) =
        if pieceType.IsNone then ""
        else 
            match pieceType.Value with
            |PieceType.Pawn -> ""
            |PieceType.Knight -> "N"
            |PieceType.Bishop -> "B"
            |PieceType.Rook -> "R"
            |PieceType.Queen -> "Q"
            |PieceType.King -> "K"
            |_ -> ""
    let MoveTarget(move:pMove) =
        if move.TargetSquare <> OUTOFBOUNDS then
            SQUARE_NAMES.[int(move.TargetSquare)]
        else ""
    let MoveOrigin(move:pMove) =
        let piece = Piece(move.Piece)
        let origf = if move.OriginFile.IsSome then FILE_NAMES.[int(move.OriginFile.Value)] else ""
        let origr = if move.OriginRank.IsSome then RANK_NAMES.[int(move.OriginRank.Value)] else ""
        piece + origf + origr    
    let CheckAndMateAnnotation(move:pMove) =
        if move.IsCheckMate then "#"
        elif move.IsDoubleCheck then "++"
        elif move.IsCheck then "+"
        else ""
    let Move(mv:pMove, writer:TextWriter) =
        match mv.Mtype with
        | Simple -> 
            let origin = MoveOrigin(mv)
            let target = MoveTarget(mv)
            writer.Write(origin)
            writer.Write(target)
            if mv.PromotedPiece.IsSome then
                writer.Write("=")
                writer.Write(Piece(mv.PromotedPiece))
            writer.Write(CheckAndMateAnnotation(mv))
        | Capture -> 
            let origin = MoveOrigin(mv)
            let target = MoveTarget(mv)
            writer.Write(origin)
            writer.Write("x")
            writer.Write(target)
            if mv.PromotedPiece.IsSome then
                writer.Write("=")
                writer.Write(Piece(mv.PromotedPiece))
            writer.Write(CheckAndMateAnnotation(mv))
        | CastleKingSide -> 
            writer.Write("O-O")
            writer.Write(CheckAndMateAnnotation(mv))
        | CastleQueenSide ->
            writer.Write("O-O-O")
            writer.Write(CheckAndMateAnnotation(mv))
    let MoveStr(mv:pMove) =
        let writer = new StringWriter()
        Move(mv,writer)
        writer.ToString()
    let MoveText(ml:MoveTextEntry list, writer:TextWriter) =
        let rec domte (iml:MoveTextEntry list) (indent:string) isc donl =
            if not (List.isEmpty iml) then
                if isc then writer.Write(" ")
                let entry = iml.Head
                match entry with
                |HalfMoveEntry(mn,ic,mv,amv) -> 
                    if donl then 
                        writer.WriteLine()
                        writer.Write(indent)
                    if mn.IsSome then
                        writer.Write(mn.Value)
                        writer.Write(if ic then "... " else ". ")
                    Move(mv, writer)
                    domte iml.Tail indent true false
                |CommentEntry(str) -> 
                    if donl then 
                        writer.WriteLine()
                        writer.Write(indent)
                    writer.Write("{" + str + "}")
                    domte iml.Tail indent true false
                |GameEndEntry(gr) -> writer.Write(ResultString(gr))
                |NAGEntry(cd) -> 
                    if donl then 
                        writer.WriteLine()
                        writer.Write(indent)
                    writer.Write("$" + (cd|>int).ToString())
                    domte iml.Tail indent true false
                |RAVEntry(rml) -> 
                    writer.WriteLine()
                    writer.Write(indent + " ")
                    writer.Write("(")
                    domte rml (indent + "  ") false false
                    writer.Write(")")
                    domte iml.Tail indent true true
        domte ml "" false false
    let Tag(name:string, value:string, writer:TextWriter) =
        writer.Write("[")
        writer.Write(name + " \"")
        writer.Write(value)
        writer.WriteLine("\"]")
    let Game(game:Game, writer:TextWriter) =
        Tag("Event", game.Event, writer)
        Tag("Site", game.Site, writer)
        Tag("Date", game|>DateUtil.ToStr, writer)
        Tag("Round", game.Round, writer)
        Tag("White", game.WhitePlayer, writer)
        Tag("Black", game.BlackPlayer, writer)
        Tag("Result", ResultString(game.Result), writer)
        Tag("WhiteElo", game.WhiteElo, writer)
        Tag("BlackElo", game.BlackElo, writer)
        for info in game.AdditionalInfo do
            Tag(info.Key, info.Value, writer)
        writer.WriteLine();
        MoveText(game.MoveText, writer)
        writer.WriteLine();
