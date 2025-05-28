namespace GenBerk

type Dirn = 
    | DirN = -8
    | DirE = 1
    | DirS = 8
    | DirW = -1
    | DirNE = -7
    | DirSE = 9
    | DirSW = 7
    | DirNW = -9
    | DirNNE = -15
    | DirEEN = -6
    | DirEES = 10
    | DirSSE = 17
    | DirSSW = 15
    | DirWWS = 6
    | DirWWN = -10
    | DirNNW = -17

module Direction =
    let AllDirectionsKnight = 
        [| Dirn.DirNNE; Dirn.DirEEN; Dirn.DirEES; Dirn.DirSSE; Dirn.DirSSW; Dirn.DirWWS; 
           Dirn.DirWWN; Dirn.DirNNW |]
    let AllDirectionsRook = [| Dirn.DirN; Dirn.DirE; Dirn.DirS; Dirn.DirW |]
    let AllDirectionsBishop = [| Dirn.DirNE; Dirn.DirSE; Dirn.DirSW; Dirn.DirNW |]
    let AllDirectionsQueen = 
        [| Dirn.DirN; Dirn.DirE; Dirn.DirS; Dirn.DirW; Dirn.DirNE; Dirn.DirSE; 
           Dirn.DirSW; Dirn.DirNW |]
    let Opposite(dir : Dirn) :Dirn = -int (dir)|>enum<Dirn>
    let MyNorth(player : Player) = 
        if player = Player.White then Dirn.DirN
        else Dirn.DirS