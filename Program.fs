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

//printfn "%A" (Parser.program "5 --> var x\n\
//                              \"te\\\"st\" --> var s")
//printfn "%A" (Parser.program "event KeyPress on { stdin --> var x } do { \"te\\\"st\" --> var s }")
//printfn "%A" (Parser.program "[1,2] --> var list")
//printfn "%A" (Parser.program "[stdout] --> var list")
//printfn "%A" (Parser.program "[] --> var list")
//printfn "%A" (Parser.program "copy(list) --> this")
//printfn "%A" (Parser.program "format(s, x, y) --> this")
//printfn "%A" (Parser.program "format(s) --> this")
//printfn "%A" (Parser.program "event ReceiveArgs on { input --> [var time, var msgText] } do { 5 --> var x }")
//printfn "%A" (Parser.actor "actor Formatter { var outStream : stream }")
printfn "%A" (Parser.program
"actor Formatter receives list any {\n\
    var outStream : stream\n\
    var formatStr : string\n\
    
    event ReceiveArgs on {\n\
        input --> [var time, var msgText]\n\
    } do {\n\
        format(formatStr, time, msgText) --> this.outStream\n\
        this --> consume\n\
    }\n\
}")

match Parser.program "5 --> var x" with
| Some (leftover, program) when leftover = "" ->
    let state = Interpreter.State()
    Interpreter.evaluate state program
    printfn "%A" (state.ToString())
| x -> printfn "Error"
