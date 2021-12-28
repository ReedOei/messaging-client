module rec Interpreter

open System
open Microsoft.FSharp.Collections
open Parser

let defaultVal (typ : PsaType) : Locator =
    match typ with
    | PsaInt -> IntLit 0
    | PsaNat -> NatLit 0
    | PsaStr -> StrLit ""
    | PsaBool -> BoolLit false
    | PsaStream -> LocatorStream []
    | PsaList _ -> LocatorList []
    | PsaAny -> Uninitialized
    
let emptyVal (loc : Locator) : Locator =
    match loc with
    | Uninitialized -> Uninitialized
    | IntLit _ -> IntLit 0
    | NatLit _ -> NatLit 0
    | StrLit _ -> StrLit ""
    | BoolLit _ -> BoolLit false
    | LocatorList _ -> LocatorList []
    | LocatorStream _ -> LocatorList []
    | _ -> Uninitialized

type State() =
    let mutable store : Map<string, Locator> = Map.empty
    let mutable actors : Map<string, Map<string, Locator>> = Map.empty
    let mutable actorTypes : Map<string, Statement> = Map.empty
    
    override this.ToString() : string =
        $"State({store}, {actors}, {actorTypes})"
    
    member this.newActorType(name : string, actorType : Statement) =
        actorTypes <- actorTypes.Add(name, actorType)
        
    member this.newVar(name : string, typ : PsaType) =
        store <- store.Add(name, defaultVal typ)
        
    member this.newVar(name : string) =
        store <- store.Add(name, Uninitialized)
        
    member this.lookup(name : string) = store[name]
    
    member this.take(name : string) =
        let value = store[name]
        store <- store.Change(name, Option.map emptyVal)
        value
        
    member this.receive(name : string, value : Locator) =
        store <- store.Change(name, Option.map (combine value))

let rec evaluate (s : State) (program : list<Statement>) =
    let mutable currentState = s
    
    for stmt in program do
        evaluateStmt currentState stmt

let evaluateStmt (s : State) (stmt : Statement) =
    match stmt with
    | Actor (name, members) -> s.newActorType(name, Actor (name, members))
    | ReceivingActor (name, recvType, members) -> s.newActorType(name, ReceivingActor (name, recvType, members))
    | Transformer (name, args, body) -> ()
    | FlowStmt flow -> evaluateFlow s flow
    | FieldDefTyped (name, typ) -> ()
    | FieldDef name -> ()
    | SimpleEvent (name, trigger, body) -> ()
    | ChainedEvent (name, trigger, children) -> ()
    
let evaluateFlow (s : State) (flow : Flow) =
    match flow with
    | SimpleFlow (src, dst) -> sendToDst s (fun _ -> true) dst (resolveSrc s (fun _ -> true) src)
    
let resolveSrc (s : State) (f : Locator -> bool) (loc : Locator) : Locator =
    match loc with
    | Uninitialized -> raise (Exception("Expected value, got `uninitialized`"))
    | Consume -> raise (Exception("Can't use `consume` as a source!"))
    | Stdout -> raise (Exception("Can't use `stdout` as a source!"))
    | This -> s.take("this")
    | VarDef name ->
        s.newVar(name)
        s.take(name)
    | VarDefTyped (name, typ) ->
        s.newVar(name, typ)
        s.take(name)
    | VarRef name -> s.take(name)
    | IntLit n -> IntLit n
    | NatLit n -> NatLit n
    | StrLit s -> StrLit s
    | BoolLit b -> BoolLit b
    | Unify _ -> raise (Exception("Can't use `unify(...)` as a source!"))
    | Combine (a, b) ->
        let aVal = resolveSrc s f a
        let bVal = resolveSrc s f b
        combine aVal bVal
    | Format (format, args) ->
        match resolveSrc s f format with
        | StrLit formatStr ->
            let argVals = List.map (resolveSrc s f) args
            StrLit (String.Format(formatStr, argVals))
        | x -> raise (Exception($"Expected {format} to resolve to a string, but got: {x}"))
    | LocatorList xs -> LocatorList (List.map (resolveSrc s f) xs)
    | x -> raise (Exception($"Unsupported source locator: {x}"))
    
let sendToDst (s : State) (f : Locator -> bool) (loc : Locator) (value : Locator) =
    match loc with
    | Stdin -> raise (Exception("Can't use `stdin` as a destination!"))
    | This -> s.receive("this", value)
    | VarDef name ->
        s.newVar(name)
        s.receive(name, value)
    | VarDefTyped (name, typ) ->
        s.newVar(name, typ)
        s.receive(name, value)
    | VarRef name -> s.receive(name, value)
    | IntLit _ -> raise (Exception("Can't use an integer as a destination!"))
    | NatLit _ -> raise (Exception("Can't use a natural number as a destination!"))
    | StrLit _ -> raise (Exception("Can't use a string as a destination!"))
    | BoolLit _ -> raise (Exception("Can't use a boolean as a destination!"))
    | LocatorList xs ->
        match value with
        | LocatorList ys ->
            for x, y in List.zip xs ys do
                sendToDst s f x y
        | LocatorStream ys ->
            for x, y in List.zip xs (List.rev ys) do
                sendToDst s f x y
        | x -> raise (Exception($"Can only use a list as a destination if the value being sent is a list or stream, but got: {x}"))
    | x -> raise (Exception($"Unsupported destination locator: {x}"))
    
let combine (a : Locator) (b : Locator) : Locator =
    match a, b with
    | IntLit n, IntLit m -> IntLit (n + m)
    | NatLit n, NatLit m -> NatLit (n + m)
    | StrLit s, StrLit t -> StrLit (s + t)
    | BoolLit b, BoolLit c -> BoolLit (b || c)
    | LocatorList xs, LocatorList ys -> LocatorList (xs @ ys)
    | LocatorList xs, LocatorStream ys -> LocatorStream (List.rev xs @ xs)
    | LocatorStream xs, LocatorList ys -> LocatorStream (xs @ List.rev ys)
    | LocatorStream xs, LocatorStream ys -> LocatorStream (ys @ xs)
    | x, LocatorList xs -> LocatorList (xs @ [x])
    | x, LocatorStream xs -> LocatorStream (x :: xs)
    | x, Uninitialized -> x
    | _ -> Uninitialized
