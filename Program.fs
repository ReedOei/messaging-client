// From: https://gist.github.com/t0yv0/1346379/2ec15e36be83201897faedfe95f51f0d8543cfa8
open System.Linq
open System.Net.Sockets

type SocketArgs = SocketAsyncEventArgs
type Bytes = System.ArraySegment<byte>

exception SocketIssue of SocketError with
    override this.ToString() =
        string this.Data0

/// Wraps the Socket.xxxAsync logic into F# async logic.
let inline asyncDo (op : SocketArgs-> bool) (prepare : SocketArgs-> unit) (select : SocketArgs-> 'T) =
    Async.FromContinuations <| fun (ok, error, _) ->
        let args = new SocketArgs()
        prepare args
        let k (args : SocketArgs) =
            match args.SocketError with
            | SocketError.Success ->
                let result = select args
                args.Dispose()
                ok result
            | e ->
                args.Dispose()
                error (SocketIssue e)
        args.add_Completed(System.EventHandler<_>(fun _ -> k))
        if not (op args) then
            k args

/// Prepares the arguments by setting the buffer.
let inline setBuffer (buf : Bytes) (args : SocketArgs) =
    args.SetBuffer(buf.Array, buf.Offset, buf.Count)

let Accept (socket : Socket) =
    asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket)

let Receive (socket : Socket) (buf : Bytes) =
    asyncDo socket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred)

let Send (socket : Socket) (buf : Bytes) =
    asyncDo socket.SendAsync (setBuffer buf) ignore
    
let ReceiveString (socket : Socket) = async {
    let buffer = System.ArraySegment<_>(Array.zeroCreate 1024)
    let! received = Receive socket buffer
    return buffer.Take(received).ToArray() |> System.Text.Encoding.ASCII.GetString
}
let ReceiveLine (socket : Socket) (buffer : string) = async {
    let mutable str = buffer
    while not (str.Contains("\n")) do
        let! newStr = ReceiveString socket
        str <- str + newStr
        
    let idx = str.IndexOf("\n")
    return (str.Take(idx).ToString(), str.Skip(idx).ToString())
}
    
let ReceiveLines (socket : Socket) = seq {
    let mutable buffer = ""
    
    while true do
        let (received, newBuf) = ReceiveLine socket buffer |> Async.RunSynchronously
        match received.Split("\n") |> List.ofArray with
        | [] -> ()
        | head :: tail ->
            yield head
            buffer <- buffer + System.String.Join("\n", tail)
        buffer <- buffer + newBuf
}

let ReceiveInfos (socket : Socket) = seq {
    let mutable linesIt = (ReceiveLines socket).GetEnumerator()
    while true do
        if linesIt.Current = "text" then
            linesIt.MoveNext()
            linesIt.Current |> printfn "%s"
            linesIt.MoveNext()
            linesIt.Current |> printfn "%s"
            linesIt.MoveNext()
            linesIt.Current |> printfn "%s"
            linesIt.MoveNext()
            linesIt.Current |> printfn "%s"
}

let debugProgram (filename : string) = 
    match Parser.program (System.IO.File.ReadAllText filename) with
    | Some (leftover, program) when leftover = "" ->
//        printfn "Program: %A" program
        let state = Interpreter.State()
        Interpreter.interpret state program
        printfn $"State: %A{state.ToString()}"
    | x -> printfn "Error"
    
let runProgram (filename : string) = 
    match Parser.program (System.IO.File.ReadAllText filename) with
    | Some (leftover, program) when leftover = "" ->
        let state = Interpreter.State()
        Interpreter.interpret state program
    | x -> printfn "Error"
    
debugProgram "test.psa"
debugProgram "unify-test.psa"

runProgram "hello-world.psa"
