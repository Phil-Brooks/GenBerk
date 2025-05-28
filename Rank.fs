namespace GenBerk

module Rank = 
    
    let Parse(c : char) :Rank = 
        let Rankdesclookup = RANK_NAMES|>List.reduce(+)
        let idx = Rankdesclookup.IndexOf(c.ToString().ToLower())
        if idx < 0 then failwith (c.ToString() + " is not a valid rank")
        else int16(idx) 
    
    let RankToString(rank : Rank) = RANK_NAMES.[int(rank)]
    let IsInBounds(rank : Rank) = rank >= 0s && rank <= 7s

