namespace GenBerk

module Piece = 
    
    let Parse(c : char) = 
        match c with
        | 'P' -> Piece.WPawn
        | 'N' -> Piece.WKnight
        | 'B' -> Piece.WBishop
        | 'R' -> Piece.WRook
        | 'Q' -> Piece.WQueen
        | 'K' -> Piece.WKing
        | 'p' -> Piece.BPawn
        | 'n' -> Piece.BKnight
        | 'b' -> Piece.BBishop
        | 'r' -> Piece.BRook
        | 'q' -> Piece.BQueen
        | 'k' -> Piece.BKing
        | _ -> failwith (c.ToString() + " is not a valid piece")

    let ToPieceType(piece : Piece) = (int (piece) &&& 7) |> PcTp

    let PieceToPlayer(piece : Piece) = (int (piece) >>> 3) |> Plyr

