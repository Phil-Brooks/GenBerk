namespace GenBerk

module Berserk =
    let mutable prc = new System.Diagnostics.Process()
    ///send message to engine
    let Send(command:string) = 
        prc.StandardInput.WriteLine(command)
    ///set up engine
    let ComputeAnswer(fen, depth) = 
        Send("ucinewgame")
        Send("setoption name Threads value " + (System.Environment.ProcessorCount - 1).ToString())
        Send("position startpos")
        Send("position fen " + fen + " ")
        Send("go depth " + depth.ToString())
        prc.WaitForExit()
    ///set up process
    let SetUpPrc () = 
        prc.StartInfo.CreateNoWindow <- true
        prc.StartInfo.FileName <- "berserk.exe"
        prc.StartInfo.RedirectStandardOutput <- true
        prc.StartInfo.UseShellExecute <- false
        prc.StartInfo.RedirectStandardInput <- true
        prc.StartInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
        prc.Start() |> ignore
        prc.BeginOutputReadLine()
    ///Gets the Best Entry from a message
    let GetBest(msg:string,bd:Brd) =
        if msg.StartsWith("info") then
            let mv,resp = 
                let st = msg.LastIndexOf("pv")
                let ucis = msg.Substring(st+2)
                //need to change to SAN format
                let bits = ucis.Trim().Split([|' '|])
                let mv0 = bits[0]|>MoveUtil.fromUci bd|>MoveUtil.toPgn bd
                let pmv0 = pMove.Parse mv0
                let move0 = pMove.ToMove bd pmv0
                let newbd = Board.MoveApply move0 bd
                let resp0 = 
                    if bits.Length>1 then bits[1]|>MoveUtil.fromUci newbd|>MoveUtil.toPgn newbd
                    else ""
                mv0, resp0
            let scr =
                let st = msg.LastIndexOf("cp")
                let ss = msg.Substring(st+2,10).Trim()
                let bits = ss.Split([|' '|])
                let cp = int(bits.[0])
                cp
            {Best=mv;Resp=resp;Eval=scr}
        else
            {Best=msg;Resp="";Eval=0}
    let Stop() = 
        if prc <> null then prc.Kill()
    let GetBestMove(fen:string,dpth:int) = 
        prc <- new System.Diagnostics.Process()
        let cbd = fen|>Board.FromStr
        let mutable bestlst = []
        //p_out
        let pOut (e : System.Diagnostics.DataReceivedEventArgs) = 
            if not (e.Data = null || e.Data = "") then 
                let msg = e.Data.ToString().Trim()
                if not (msg.StartsWith("info") && not (msg.Contains(" cp "))) then 
                    System.Console.WriteLine(msg)
                    if msg.StartsWith("bestmove") then Stop()
                    else
                        bestlst <- ((msg,cbd)|>GetBest)::bestlst
        prc.OutputDataReceived.Add(pOut)
        //Start process
        SetUpPrc()
        // call calcs
        // need to send game position moves as UCI
        ComputeAnswer(fen, dpth)
        bestlst.Head
