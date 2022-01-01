module rec Interpreter

open System
open Microsoft.FSharp.Collections
open Parser

let concatMap f xs = List.concat (List.map f xs)
    
let flowSrc (flow : Flow) =
    match flow with
    | SimpleFlow (src, dst) -> src
    | TransformerFlow (src, transformer, dst) -> src

let defaultVal (typ : PsaType) : Locator =
    match typ with
    | PsaInt -> IntLit 0
    | PsaNat -> NatLit 0
    | PsaStr -> StrLit ""
    | PsaBool -> BoolLit false
    | PsaStream -> LocatorStream []
    | PsaList _ -> LocatorList []
    | PsaAny -> Uninitialized
    | ActorType _ -> Uninitialized
    
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
    
let isField (stmt : Statement) : bool =
    match stmt with
    | FieldDef _ -> true
    | FieldDefTyped _ -> true
    | _ -> false
    
let initField (field : Statement, value : Locator) : string * Locator =
    match field with
    | FieldDef name -> (name, value)
    | FieldDefTyped (name, _) -> (name, value)
    | _ -> ("", Uninitialized) // Impossible because we only call this function on FieldDefs

let initEvents (stmt : Statement) : (string * Statement) list =
    match stmt with
    | SimpleEvent (name, _, _) -> [(name, stmt)]
    | ChainedEvent (name, _, _) -> [(name, stmt)]
    | _ -> []
    
let vars (loc : Locator) : string list =
    match loc with
    | Uninitialized -> []
    | Nothing -> []
    | Consume -> []
    | Stdin -> []
    | Stdout -> []
    | This -> []
    | ActorVal _ -> []
    | VarDef _ -> []
    | VarDefTyped _ -> []
    | VarRef name -> [name]
    | IntLit _ -> []
    | NatLit _ -> []
    | StrLit _ -> []
    | BoolLit _ -> []
    | Copy loc -> vars loc
    | Unify loc -> vars loc
    | Combine (a,b) -> vars a @ vars b
    | Format (fmtStr, args) -> vars fmtStr @ concatMap vars args
    | LocatorList vals -> concatMap vars vals
    | LocatorStream vals -> concatMap vars vals
    | FieldAccess (loc, _) -> vars loc
    | Select (src, filter) -> vars src @ vars filter
    
let toFlow (stmt : Statement) : Flow list =
    match stmt with
    | FlowStmt flow -> [flow]
    | _ -> []
    
let eventSources (stmt : Statement) : string list =
    let triggerFlows = match stmt with
                       | SimpleEvent (_, trigger, _) -> concatMap toFlow trigger
                       | ChainedEvent (_, trigger, _) -> concatMap toFlow trigger
                       | _ -> []
                       
    concatMap (vars << flowSrc) triggerFlows 

exception FlowException of string

type ActorType(actorType : Statement) =
    let events : Map<string, Statement> =
        match actorType with
        | Actor (name, recvType, sendType, members) -> Map (concatMap initEvents members)
        | x -> failwith $"Expected actor type, got: {x}"
        
    let sourceToEvents : Map<string, Statement list> =
        let mutable res = Map.empty
        for e in events.Values do
            for src in eventSources e do
                if res.ContainsKey(src) then
                    res <- res.Change(src, Option.map (fun es -> e :: es))
                else
                    res <- res.Add(src, [e])
        res
        
    override this.ToString() : string =
        $"ActorType({events}, {sourceToEvents}, {actorType})"
        
    member this.initFields(fieldValues : Locator list) : Map<string, Locator> =
        let actorState =
            match actorType with
            | Actor (name, recvTyp, sendTyp, members) ->
                [("input", defaultVal recvTyp); ("output", defaultVal sendTyp); ("thisType", StrLit name)]
                @ List.map initField (List.zip (List.filter isField members) fieldValues)
            | x -> raise (Exception($"Expected '{actorType}' to map to an actor type, but got: {x}"))
            
        Map actorState
        
    member this.trigger(s : State, fieldName : string) =
        for e in Option.defaultValue [] (sourceToEvents.TryFind(fieldName)) do
            match e with
            | SimpleEvent (_, trigger, body) ->
                try
                    evaluate s trigger
                    evaluate s body
                with
                | FlowException _ -> ()
            | x -> failwith $"Unsupported: {x}"
        
    member this.trigger(s : State, fieldName : string, newStore : Map<string, Locator>) : Map<string, Locator> =
        let mutable resultStore = newStore
        for e in Option.defaultValue [] (sourceToEvents.TryFind(fieldName)) do
            match e with
            | SimpleEvent (_, trigger, body) ->
                let oldStore = s.getStore()
                s.setStore(resultStore)
                
                try
                    evaluate s trigger
                    evaluate s body
                with
                | FlowException _ -> ()
                
                resultStore <- s.getStore()
                s.setStore(oldStore)
            | x -> failwith $"Unsupported: {x}"
        resultStore

type State() =
    let mutable store : Map<string, Locator> = Map.empty
    let mutable actorTypes : Map<string, ActorType> = Map.empty
    
    override this.ToString() : string =
        $"State({store}, {actorTypes})"
        
    member this.getStore() : Map<string, Locator> = store
    member this.setStore(newStore : Map<string, Locator>) = store <- newStore
    
    member this.newActorType(name : string, actorType : Statement) =
        actorTypes <- actorTypes.Add(name, ActorType(actorType))
        
    member this.newActor(actorType : string, fieldValues : Locator list) =
        ActorVal (actorType, actorTypes[actorType].initFields(fieldValues))
        
    member this.newVar(name : string, typ : PsaType) =
        store <- store.Add(name, defaultVal typ)
        
    member this.newVar(name : string) =
        store <- store.Add(name, Uninitialized)
        
    member this.lookup(name : string) =
        match store[name] with
        | ActorVal (_, fields) -> fields["output"]
        | value -> value
    
    member this.take(name : string, f : Locator -> Locator) =
        let result = match store[name] with
                     | ActorVal (actorType, fields) ->
                         let value = fields["output"]
                         let newFields = fields.Change("output", Option.map (fun cur -> remove cur (f cur)))
                         let newState = actorTypes[actorType].trigger(this, "output", newFields)
                         store <- store.Change(name, Option.map (fun _ -> ActorVal (actorType, newState)))
                         f value
                     | value ->
                         store <- store.Change(name, Option.map (fun cur -> remove cur (f cur)))
                         f value
        if store.ContainsKey("thisType") then
            match store["thisType"] with
            | StrLit actorType -> actorTypes[actorType].trigger(this, name)
            | x -> failwith $"thisType should always map to a string literal, but got: {x}"
        result
        
    member this.receive(name : string, value : Locator) =
        let receiveValue (loc : Locator) =
            match loc with
            | ActorVal (actorType, fields) ->
                let newThis = fields.Change("input", Option.map (combine value))
                let newState = actorTypes[actorType].trigger(this, "input", newThis)
                ActorVal (actorType, newState)
            | curVal -> combine value curVal
        if store.ContainsKey("thisType") then
            match store["thisType"] with
            | StrLit actorType -> actorTypes[actorType].trigger(this, name)
            | x -> failwith $"thisType should always map to a string literal, but got: {x}"
        store <- store.Change(name, Option.map receiveValue)

let evaluate (s : State) (stmts : list<Statement>) =
    let mutable currentState = s
    for stmt in stmts do
        evaluateStmt currentState stmt

let evaluateStmt (s : State) (stmt : Statement) =
    match stmt with
    | Actor (name, _, _, _) -> s.newActorType(name, stmt)
    | Transformer (name, args, body) -> ()
    | FlowStmt flow -> evaluateFlow s flow
    | FieldDefTyped (name, typ) -> ()
    | FieldDef name -> ()
    | SimpleEvent (name, trigger, body) -> ()
    | ChainedEvent (name, trigger, children) -> ()
    
let evaluateFlow (s : State) (flow : Flow) =
    match flow with
    | SimpleFlow (src, dst) -> sendToDst s dst (resolveSrc s id src)
    | TransformerFlow (src, tr, dst) ->
        sendToDst s dst (transform s tr (resolveSrc s id src))
        
let transform (s : State) (tr : Transformer) (value : Locator) : Locator =
    match tr with
    | Start name ->
        match value with
        | LocatorList vals -> s.newActor(name, vals)
        | LocatorStream vals -> s.newActor(name, List.rev vals)
        | _ -> failwith "unsupported"
    | ChainedLoc loc ->
        sendToDst s loc value
        resolveSrc s id loc
    | _ -> failwith "todo"
    
let resolveSrc (s : State) (f : Locator -> Locator) (loc : Locator) : Locator =
    match loc with
    | Uninitialized -> raise (Exception("Expected value, got `uninitialized`"))
    | Consume -> raise (Exception("Can't use `consume` as a source!"))
    | Stdout -> raise (Exception("Can't use `stdout` as a source!"))
    | This -> s.take("this", f)
    | ActorVal (actorType, vals) -> ActorVal (actorType, vals)
    | VarDef name ->
        s.newVar(name)
        s.take(name, f)
    | VarDefTyped (name, typ) ->
        s.newVar(name, typ)
        s.take(name, f)
    | VarRef name -> s.take(name, f)
    | Select (loc, sel) ->
        let toTake = resolveSrc s (fun _ -> Nothing) sel
        resolveSrc s (fun _ -> toTake) loc
    | Copy (VarRef name) -> s.lookup(name)
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
    
let sendToDst (s : State) (loc : Locator) (value : Locator) =
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
//    | ActorRef id -> s.receiveInActor(id, "input", value)
    | IntLit _ -> raise (Exception("Can't use an integer as a destination!"))
    | NatLit _ -> raise (Exception("Can't use a natural number as a destination!"))
    | StrLit _ -> raise (Exception("Can't use a string as a destination!"))
    | BoolLit _ -> raise (Exception("Can't use a boolean as a destination!"))
    | LocatorList xs ->
        match value with
        | LocatorList ys ->
            for x, y in List.zip xs ys do
                sendToDst s x y
        | LocatorStream ys ->
            for x, y in List.zip xs (List.rev ys) do
                sendToDst s x y
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
    
let rec removeElem (xs : 'a list) (x : 'a) : 'a list =
    match xs with
    | [] -> raise (FlowException($"Could not remove {x}"))
    | y :: xs -> if x = y then xs else y :: removeElem xs x
    
let removeAll (xs : 'a list) (ys : 'a list) : 'a list = List.fold removeElem xs ys

let remove (a : Locator) (b : Locator) : Locator =
    match a, b with
    | x, Nothing -> x
    | IntLit n, IntLit m -> IntLit (n - m)
    | NatLit n, NatLit m ->
        if n >= m then
            NatLit (n - m)
        else
            raise (FlowException($"Could not take {m} from {n}"))
    | StrLit s, StrLit t ->
        if s.EndsWith(t) then
            StrLit (s.Substring(0, s.Length - t.Length))
        else
            raise (FlowException($"Could not remove {t} from {s}"))
    | BoolLit b, BoolLit c -> BoolLit c
    | LocatorList xs, LocatorList ys -> LocatorList (removeAll xs ys)
    | LocatorList xs, LocatorStream ys -> LocatorList (removeAll xs ys)
    | LocatorStream xs, LocatorList ys -> LocatorStream (removeAll xs ys)
    | LocatorStream xs, LocatorStream ys -> LocatorStream (removeAll xs ys)
    | LocatorList xs, x -> LocatorList (removeElem xs x)
    | LocatorStream xs, x -> LocatorStream (removeElem xs x)
    | _ -> Uninitialized
