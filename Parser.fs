module rec Parser

let charSeqToString (charSeq : seq<char>) : string = 
    Seq.fold (fun s c -> s + c.ToString()) "" charSeq

type Parser<'a> = string -> Option<string * 'a>

type ParserBuilder() =
    member this.Bind(x : Parser<'a>, f : 'a -> Parser<'b>) : Parser<'b> = fun s ->
        match x s with
        | None -> None
        | Some(rest, a) -> f a rest
        
    member this.Return(x : 'a) : Parser<'a> = fun s -> Some(s, x)

let parser = ParserBuilder()

let char (c : char) : Parser<char> = fun s ->
    if s.StartsWith(c) then
        Some(s.Substring(1), s[0])
    else
        None

let string (pat : string) : Parser<string> = fun s ->
    if s.StartsWith(pat) then
        Some(s.Substring(pat.Length), pat)
    else
        None

let optional (p : Parser<'a>) : Parser<Option<'a>> = fun s ->
    match p s with
    | None -> Some(s, None)
    | Some(rest, value) -> Some(rest, Some(value))

let rec many (p : Parser<'a>) : Parser<seq<'a>> = parser {
    let! r = optional p
    match r with
    | None -> return Seq.empty
    | Some(v) -> 
        let! rest = many p
        return Seq.append (Seq.singleton v) rest
}

let before (c1 : char) (c2 : char) : Parser<unit> = fun s ->
    if s.IndexOf(c1) < s.IndexOf(c2) then
        Some(s, ())
    else
        None

let rec manyTill (p : Parser<'a>) (fail : Parser<'b>) : Parser<seq<'a>> = fun s ->
    match fail s with
    | None -> match p s with
        | None -> Some (s, Seq.empty)
        | Some (rest, value) ->
            match manyTill p fail rest with
            | None -> Some (rest, Seq.singleton value)
            | Some (final, vals) -> Some (final, Seq.append (Seq.singleton value) vals)
    | Some _ -> Some (s, Seq.empty)

let some (p : Parser<'a>) : Parser<seq<'a>> = parser {
    let! head = p
    let! rest = many p
    return Seq.append (Seq.singleton head) rest
}

let (<|>) (pa : Parser<'a>) (pb : Parser<'a>) : Parser<'a> = fun s ->
    match pa s with
    | None -> pb s
    | Some(rest, value) -> Some(rest, value)

let (|>>) (p : Parser<'a>) (x : 'b) : Parser<'b> = fun s ->
    match p s with
    | None -> None
    | Some (rest, _) -> Some (rest, x)
    
let (|>>=) (p : Parser<'a>) (f : 'a -> 'b) : Parser<'b> = fun s ->
    match p s with
    | None -> None
    | Some (rest, value) -> Some (rest, f value)
    
let ws = char ' ' <|> char '\t'
let symbol (p : Parser<'a>) : Parser<'a> = parser {
    let! _ = many ws
    let! value = p
    let! _ = many ws
    return value
}
let wsNewline = ws <|> char '\n' <|> char '\r'
let newlineSymbol (p : Parser<'a>) : Parser<'a> = parser {
    let! _ = many wsNewline
    let! value = p
    let! _ = many wsNewline
    return value
}

let failNow : Parser<'a> = fun _ -> None

let oneOf (chars : string) : Parser<char> = fun s ->
    let mutable res = None
    for c in chars do
        if s.StartsWith(c) then
            res <- Some (s.Substring(1), c)
    res
    
let noneOf (chars : string) : Parser<char> = fun s ->
    let mutable res = Some (s.Substring(1), s[0])
    let mutable loopDone = false
    for c in chars do
        if not loopDone && s.StartsWith(c) then
            res <- None
            loopDone <- true     
    res

let sepBy (p : Parser<'a>) (sep : Parser<'b>) : Parser<seq<'a>> = parser {
    let! head = optional p
    match head with
    | None -> return Seq.empty
    | Some value ->
        let! _ = optional sep
        let! values = sepBy p sep
        return Seq.append (Seq.singleton value) values
}

let sepBy1 (p : Parser<'a>) (sep : Parser<'b>) : Parser<seq<'a>> = parser {
    let! head = p
    let! _ = optional sep
    let! values = sepBy p sep
    return Seq.append (Seq.singleton head) values
}

// Modified from: https://stackoverflow.com/a/24106749
let escape : Parser<string> = parser {
    let! _ = char '\\'
    let! c = oneOf "\\\"rnt"
    if c = '\\' then
        return "\\"
    else if c = '"' then
        return "\""
    else if c = 'n' then
        return "\n"
    else if c = '\r' then
        return "\r"
    else 
        return "\t"
}
let nonescape : Parser<string> = noneOf "\\\"" |>>= fun c -> c.ToString()

let escapeStrChar = escape <|> nonescape
let escapedStr = parser {
    let! _ = char '"'
    let! raw = many escapeStrChar 
    let! _ = char '"'
    return Seq.fold (+) "" raw
}

let newlines = (char '\n' <|> char '\r') |> symbol |> many

let withNewlines (p : Parser<'a>) : Parser<'a> = parser {
    let! value = p
    let! _ = newlines
    return value
}

let anyChar : Parser<char> = fun s ->
    if s.Length = 0 then
        None
    else
        Some (s.Substring(1), s[0])

type PsaType =
    | PsaInt
    | PsaNat
    | PsaStr
    | PsaBool
    | PsaStream
    | PsaList of PsaType
    | PsaAny
    | ActorType of string
    
type TypeQuant =
    | Nonempty
    | AnyQuant
    | Exactly of Locator
    | Every

type Locator =
    | Uninitialized
    | Nothing
    | Consume
    | Stdin
    | Stdout
    | This
    | ActorRef of int
    | VarDef of string
    | VarDefTyped of string * PsaType
    | VarRef of string
    | IntLit of int64
    | NatLit of uint64
    | StrLit of string
    | BoolLit of bool
    | Copy of Locator
    | Unify of Locator
    | Combine of Locator * Locator
    | Format of Locator * list<Locator>
    | LocatorList of list<Locator>
    | LocatorStream of list<Locator>
    | FieldAccess of Locator * string
    | Select of Locator * Locator
    | SelectQuant of Locator * TypeQuant
    
type Transformer =
    | Start of string
    | TransformerCall of string * Locator list
    | ChainedLoc of Locator
    
type Flow =
    | SimpleFlow of Locator * Locator
    | TransformerFlow of Locator * Transformer * Locator

type Statement =
    | Actor of string * PsaType * PsaType * list<Statement>
    | Transformer of string * list<Locator> * list<Statement>
    | FlowStmt of Flow
    | FieldDefTyped of string * PsaType
    | FieldDef of string 
    | SimpleEvent of string * list<Statement> * list<Statement>
    | ChainedEvent of string * list<Statement> * list<Statement>

let symbolName : Parser<string> =
    some (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_") |>>= charSeqToString

let varDefWithType : Parser<Locator> = parser {
    let! _ = symbol (string "var")
    let! name = symbolName
    let! _ = symbol (char ':')
    let! typeName = psaType
    return VarDefTyped (name, typeName)
}

let varDefNoType : Parser<Locator> = parser {
    let! _ = symbol (string "var")
    let! name = symbolName
    return VarDef name
}

let varRef : Parser<Locator> = symbol symbolName |>>= VarRef

let integer : Parser<int> = some (oneOf "1234567890") |>>= charSeqToString |>>= int

let locatorList : Parser<Locator> = parser {
    let! _ = symbol (char '[')
    let! content = sepBy locator (newlineSymbol (char ','))
    let! _ = symbol (char ']')
    return LocatorList (List.ofSeq content)
}

let psaList : Parser<PsaType> = parser {
    let! _ = symbol (string "list")
    let! elemType = psaType
    return PsaList elemType
}

let psaType : Parser<PsaType> =
    (symbol (string "int") |>> PsaInt)
    <|> (symbol (string "string") |>> PsaStr)
    <|> (symbol (string "nat") |>> PsaNat)
    <|> (symbol (string "bool") |>> PsaBool)
    <|> (symbol (string "stream") |>> PsaStream)
    <|> (symbol (string "any") |>> PsaAny)
    <|> psaList
    <|> (symbolName |>>= ActorType)
    
let copyLoc = parser {
    let! _ = symbol (string "copy")
    let! _ = symbol (char '(')
    let! arg = locator
    let! _ = symbol (char ')')
    return Copy arg
}

let unifyLoc = parser {
    let! _ = symbol (string "unify")
    let! _ = symbol (char '(')
    let! arg = locator
    let! _ = symbol (char ')')
    return Unify arg
}
    
let formatLoc = parser {
    let! _ = symbol (string "format")
    let! _ = symbol (char '(')
    let! args = sepBy1 locator (symbol (char ',')) |>>= List.ofSeq
    let! _ = symbol (char ')')
    return Format (args.Head, args.Tail) // Safe because of the use of sepBy1
}

let parenLoc : Parser<Locator> = parser {
    let! _ = symbol (char '(')
    let! inner = locator
    let! _ = symbol (char ')')
    return inner
}

let numLit : Parser<Locator> = parser {
    let! n = symbol integer
    if n < 0 then
        return IntLit n
    else
        return NatLit (uint64 n)
}

let startActor : Parser<Transformer> = parser {
    let! _ = symbol (string "start")
    let! actorName = symbolName
    return Start actorName
}

let transformerCall : Parser<Transformer> = parser {
    let! name = symbolName
    let! _ = symbol (char '(')
    let! args = sepBy1 locator (symbol (char ',')) |>>= List.ofSeq
    let! _ = symbol (char ')')
    return TransformerCall (name, args)
}

let chainedLoc : Parser<Transformer> = parser {
    let! loc = locator
    return ChainedLoc loc
}

let transformer : Parser<Transformer> =
    startActor
    <|> transformerCall
    <|> chainedLoc

let basicLocator : Parser<Locator> =
    (symbol (string "consume") |>> Consume)
    <|> (symbol (string "stdin") |>> Stdin)
    <|> (symbol (string "stdout") |>> Stdout)
    <|> (symbol (string "this") |>> This)
    <|> formatLoc
    <|> copyLoc
    <|> unifyLoc
    <|> varDefWithType
    <|> varDefNoType
    <|> numLit
    <|> (symbol escapedStr |>>= StrLit)
    <|> locatorList
    <|> formatLoc
    <|> varRef
    <|> parenLoc

let exactlyQuant : Parser<TypeQuant> = parser {
    let! _ = symbol (char '|')
    let! n = integer
    let! _ = symbol (char '|')
    return Exactly (NatLit (uint64 n))
}
    
let typeQuant : Parser<TypeQuant> =
    (symbol (string "nonempty") |>> Nonempty)
    <|> (symbol (string "+") |>> Nonempty)
    <|> (symbol (string "any") |>> AnyQuant)
    <|> (symbol (string "*") |>> AnyQuant)
    <|> (symbol (string "every") |>> Every)
    <|> (symbol (string "empty") |>> Exactly (NatLit 0UL))
    <|> (symbol (string "!") |>> Exactly (NatLit 1UL))
    <|> exactlyQuant

let selectLoc : Parser<Locator> = parser {
    let! loc = basicLocator
    let! _ = symbol (char '[')
    let! filter = locator
    let! _ = symbol (char ']')
    return Select (loc, filter)
}

let selectQuant : Parser<Locator> = parser {
    let! loc = basicLocator
    let! _ = symbol (char '[')
    let! quant = typeQuant
    let! _ = symbol (char ']')
    return SelectQuant (loc, quant)
}
    
let fieldAccess : Parser<Locator> = parser {
    let! receiver = basicLocator
    let! _ = symbol (char '.')
    let! fieldName = symbolName
    return FieldAccess (receiver, fieldName)
}

let combineLoc : Parser<Locator> = parser {
    let! a = basicLocator
    let! _ = symbol (char '+')
    let! b = locator
    return Combine (a, b)
}
    
let locator : Parser<Locator> =
    fieldAccess
    <|> basicLocator
    <|> combineLoc
    <|> selectLoc
    <|> selectQuant
    
let simpleFlow : Parser<Flow> = parser {
    let! src = locator
    let! _ = symbol (string "-->")
    let! dst = locator
    let! _ = newlines
    return SimpleFlow (src, dst)
}

let selectLocFlow : Parser<Flow> = parser {
    let! src = locator
    let! _ = symbol (string "--[")
    let! filter = locator
    let! _ = symbol (string "]->" <|> string "]-->")
    let! dst = locator
    let! _ = newlines
    return SimpleFlow (Select (src, filter), dst)
}

let selectQuantFlow : Parser<Flow> = parser {
    let! src = locator
    let! _ = symbol (string "--[")
    let! quant = typeQuant
    let! _ = symbol (string "]->" <|> string "]-->")
    let! dst = locator
    let! _ = newlines
    return SimpleFlow (SelectQuant (src, quant), dst)
}

let basicFlow : Parser<Flow> =
    simpleFlow
    <|> selectLocFlow
    <|> selectQuantFlow
    
let transformerFlow : Parser<Flow> = parser {
    let! src = locator
    let! _ = symbol (string "-->")
    let! trans = transformer
    let! _ = symbol (string "-->")
    let! dst = locator
    let! _ = newlines
    return TransformerFlow (src, trans, dst)
}
    
let flow : Parser<Flow> =
    transformerFlow
    <|> basicFlow

let simpleEvent : Parser<Statement> = parser {
    let! _ = symbol (string "event")
    let! eventName = optional symbolName
    let! eventName = parser {
                 if eventName = Some "on" then
                    return None
                else
                    let! _ = symbol (string "on")
                    return eventName
            }
    let! _ = newlineSymbol (char '{') 
    let! guard = many statement
    let! _ = newlineSymbol (char '}') 
    let! _ = symbol (string "do") 
    let! _ = newlineSymbol (char '{') 
    let! body = many statement
    let! _ = newlineSymbol (char '}') 
    return SimpleEvent (Option.defaultValue "" eventName, List.ofSeq guard, List.ofSeq body)
}

let chainedEvent : Parser<Statement> = parser {
    let! _ = symbol (string "event")
    let! eventName = optional symbolName
    let! eventName = parser {
                 if eventName = Some "on" then
                    return None
                else
                    let! _ = symbol (string "on")
                    return eventName
            }
    let! _ = newlineSymbol (char '{') 
    let! guard = many statement
    let! _ = newlineSymbol (char '}') 
    let! _ = symbol (string "and")
    let! _ = symbol (string "then")
    let! _ = newlineSymbol (char '{') 
    let! childEvents = many event
    let! _ = newlineSymbol (char '}') 
    return ChainedEvent (Option.defaultValue "" eventName, List.ofSeq guard, List.ofSeq childEvents)
}

let event = simpleEvent <|> chainedEvent

let actor : Parser<Statement> = parser {
    let! _ = symbol (string "actor")
    let! actorName = symbolName
    let! _ = newlineSymbol (char '{') 
    let! members = many statement
    let! _ = newlineSymbol (char '}') 
    return Actor (actorName, PsaList PsaAny, PsaList PsaAny, List.ofSeq members)
}

let receivingActor : Parser<Statement> = parser {
    let! _ = symbol (string "actor")
    let! actorName = symbolName
    let! _ = symbol (string "receives")
    let! recvType = psaType
    let! _ = newlineSymbol (char '{') 
    let! members = many statement
    let! _ = newlineSymbol (char '}') 
    return Actor (actorName, recvType, PsaList PsaAny, List.ofSeq members)
}

let receiveEmitActor : Parser<Statement> = parser {
    let! _ = symbol (string "actor")
    let! actorName = symbolName
    let! _ = symbol (string "receives")
    let! recvType = psaType
    let! _ = symbol (string "emits")
    let! emitType = psaType
    let! _ = newlineSymbol (char '{') 
    let! members = many statement
    let! _ = newlineSymbol (char '}') 
    return Actor (actorName, recvType, emitType, List.ofSeq members)
}

let toFieldDef (vdef : Locator) : Statement =
    match vdef with
    | VarDef name -> FieldDef name
    | VarDefTyped (name, typ) -> FieldDefTyped (name, typ)
    | _ -> FieldDef "" // This can't happen because of how the function is called (c.f., `statement`), so this can be whatever.

let statement : Parser<Statement> =
    (flow |>>= FlowStmt)
    <|> (withNewlines varDefWithType |>>= toFieldDef)
    <|> (withNewlines varDefNoType |>>= toFieldDef)
    <|> event
    <|> actor
    <|> receivingActor
    <|> receiveEmitActor

let program = parser {
    let! _ = newlines
    let! stmts = many statement
    return List.ofSeq stmts
}
