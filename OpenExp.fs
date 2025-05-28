namespace GenBerk

open System.IO
open FSharp.Data

//getting data from Lichess Opening Explorer
module OpenExp =
    [<Literal>]
    let sample = "https://explorer.lichess.ovh/masters"
    
    let addr = "https://explorer.lichess.ovh/masters?fen="
    
    type Results = JsonProvider<sample>

    ///get moves for board
    let GetMoves(bd:Brd) = 
        let fen = bd|>Board.ToStr
        let res = Results.Load(addr + fen)
        let mvs = res.Moves
        let sans = mvs|>Array.map(fun m -> m.San)
        sans
    