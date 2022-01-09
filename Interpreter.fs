module rec Interpreter

open System
open System.Threading
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
    | ActorRef _ -> []
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
    | SelectQuant (src, quant) -> vars src @ varsQuant quant
    
let varsQuant (q : TypeQuant) : string list =
    match q with
    | Exactly loc -> vars loc
    | _ -> []
    
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

type TriggeredEvent = int * string
    
type ActorVal = string * Map<string, Locator>

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
        
    member this.initFields(ref : int, fieldValues : Locator list) : Map<string, Locator> =
        let actorState =
            match actorType with
            | Actor (name, recvTyp, sendTyp, members) ->
                [("input", defaultVal recvTyp); ("output", defaultVal sendTyp); ("thisType", StrLit name); ("thisRef", ActorRef ref)]
                @ List.map initField (List.zip (List.filter isField members) fieldValues)
            | x -> raise (Exception($"Expected '{actorType}' to map to an actor type, but got: {x}"))
            
        Map actorState
        
    member this.trigger(s : State, fieldName : string) =
        for e in Option.defaultValue [] (sourceToEvents.TryFind(fieldName)) do
            evaluateEvent s e |> ignore
        
    member this.trigger(s : State, fieldName : string, newStore : Map<string, Locator>) : Map<string, Locator> =
        let mutable resultStore = newStore
        for e in Option.defaultValue [] (sourceToEvents.TryFind(fieldName)) do
            let oldStore = s.getStore()
            s.setStore(resultStore)
            
            evaluateEvent s e |> ignore
                
            resultStore <- s.getStore()
            s.setStore(oldStore)
        resultStore

type State() =
    let mutable store : Map<string, Locator> = Map.empty
    let mutable actorStore : Map<int, ActorVal> = Map.empty
    let mutable actorTypes : Map<string, ActorType> = Map.empty
    let mutable eventQueue : TriggeredEvent list = []
    let mutable counter = 0
    let mutable backgroundTasks : Map<int, (int * string * Locator) option Async> = Map.empty
    let mutable taskGenerators : Map<int, string * (Locator -> Locator Async)> = Map.empty
    
    override this.ToString() : string =
        $"State(%A{store}, %A{actorStore})"
        
    // Returns true iff there are more events to process
    member this.triggerEvent() : bool =
        if eventQueue.IsEmpty then
            false
        else
            let nextEvent = eventQueue.Head
            eventQueue <- eventQueue.Tail
            
            let ref, fieldName = nextEvent
            let actorType, fields = actorStore[ref]
            let newFields = actorTypes[actorType].trigger(this, fieldName, fields)
            actorStore <- actorStore.Change(ref, Option.map (fun _ -> ActorVal (actorType, newFields)))
            
            not eventQueue.IsEmpty
        
    member this.getStore() : Map<string, Locator> = store
    member this.setStore(newStore : Map<string, Locator>) = store <- newStore
    
    member this.newActorType(name : string, actorType : Statement) =
        actorTypes <- actorTypes.Add(name, ActorType(actorType))
        
    member this.newActor(actorType : string, fieldValues : Locator list) =
        let ref = counter
        counter <- counter + 1
        actorStore <- actorStore.Add(ref, ActorVal (actorType, actorTypes[actorType].initFields(ref, fieldValues)))
        ActorRef ref
        
    member this.newVar(name : string, typ : PsaType) =
        store <- store.Add(name, defaultVal typ)
        
    member this.newVar(name : string) =
        store <- store.Add(name, Uninitialized)
        
    member this.lookup(name : string, f : Locator -> Locator) =
        match store[name] with
        | ActorRef ref ->
            let _, fields = actorStore[ref]
            f fields["output"]
        | value -> f value
        
    member this.queueEvent(e : TriggeredEvent) =
        eventQueue <- eventQueue @ [e]
    
    member this.take(name : string, f : Locator -> Locator) =
        let result = match store[name] with
                     | ActorRef ref ->
                         let actorType, fields = actorStore[ref]
                         let value = fields["output"]
                         let newFields = fields.Change("output", Option.map (fun cur -> remove cur (f cur)))
                         actorStore <- actorStore.Change(ref, Option.map (fun _ -> ActorVal (actorType, newFields)))
                         
                         this.queueEvent((ref, "output"))
                         
                         f value
                     | value ->
                         store <- store.Change(name, Option.map (fun cur -> remove cur (f cur)))
                         f value
        if store.ContainsKey("thisRef") then
            match store["thisRef"] with
            | ActorRef ref -> this.queueEvent((ref, name))
            | x -> failwith $"thisRef should always map to an ActorRef, but got: {x}"
        result
        
    member this.receive(name : string, value : Locator) =
        let receiveValue (loc : Locator) =
            match loc with
            | ActorRef ref ->
                this.receiveInField(ref, "input", value)
                ActorRef ref
            | curVal -> combine curVal value
        if store.ContainsKey("thisRef") then
            match store["thisRef"] with
            | ActorRef ref -> this.queueEvent((ref, name))
            | x -> failwith $"thisRef should always map to an ActorRef, but got: {x}"
        store <- store.Change(name, Option.map receiveValue)
        
    member this.receiveInField(ref : int, fieldName : string, value : Locator) =
        let actorType, fields = actorStore[ref]
        let newFields = fields.Change(fieldName, Option.map (fun cur -> combine cur value))
        actorStore <- actorStore.Change(ref, Option.map (fun _ -> ActorVal (actorType, newFields)))
        
        this.queueEvent((ref, "input"))
        
    member this.generateBackgroundTask(generatorId : int, prevValue : Locator) =
        backgroundTasks <- backgroundTasks.Add(counter, async {
            let dst, gen = taskGenerators[generatorId] 
            let! value = gen prevValue
            return Some (generatorId, dst, value)
        })
        counter <- counter + 1
        
    member this.addBackgroundTask(dst : string, taskGenerator : Locator -> Locator Async) =
        taskGenerators <- taskGenerators.Add(counter, (dst, taskGenerator))
        counter <- counter + 1
        this.generateBackgroundTask(counter - 1, Nothing)
        
    member this.isRunning() : bool =
        match backgroundTasks.Values |> Async.Choice |> Async.RunSynchronously with
        | None -> ()
        | Some (generatorId, dst, generatedValue) ->
            this.generateBackgroundTask(generatorId, generatedValue)
            evaluateFlow this (SimpleFlow (generatedValue, VarRef dst))
        not eventQueue.IsEmpty // || Map.exists (fun _ -> isActorVal) store
        
let interpret (s : State) (stmts : Statement list) =
    if evaluate s stmts then
        while s.isRunning() do
            while s.triggerEvent() do
                ()

let evaluate (s : State) (stmts : list<Statement>) : bool =
    let oldStore = s.getStore()
    try
        for stmt in stmts do
            evaluateStmt s stmt
            
        true
    with
    | FlowException _ ->
        s.setStore(oldStore)
        false
        
// Returns true iff the body of the event was evaluated
let evaluateEvent (s : State) (e : Statement) : bool =
    match e with
    | SimpleEvent (_, trigger, body) ->
        if evaluate s trigger then
            evaluate s body |> ignore
            true
        else
            false
    | ChainedEvent (_, trigger, children) ->
        if evaluate s trigger then
            let mutable evaluated = false
            for child in children do
                if not evaluated then
                    if evaluateEvent s child then
                        evaluated <- true
            evaluated
        else
            false
    | x -> failwith $"Unsupported: {x}"

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
    
let resolveQuantSel (s : State) (quant : TypeQuant) (f : Locator -> Locator) : Locator -> Locator =
    match quant with
    | Every -> id
    | Nonempty -> fun value ->
        match value with
        | LocatorList (_ :: _) -> value
        | LocatorStream (_ :: _) -> value
        | IntLit n when n > 0 -> value
        | NatLit n when n > 0 -> value
        | StrLit s when s.Length > 0 -> value
        | BoolLit b when b -> value
        | _ -> raise (FlowException($"Wanted to select `nonempty` but got: {value}"))
    | AnyQuant -> f
    | Exactly nLoc ->
        match evaluateExpr s id nLoc with
        | NatLit n -> fun value ->
            match value with
            | LocatorList xs when xs.Length >= n -> LocatorList (List.take n xs)
            | LocatorStream xs when xs.Length >= n -> LocatorStream (List.take n xs)
            | IntLit m when m >= n -> IntLit n
            | NatLit m when m >= n -> NatLit n
            | StrLit s when s.Length >= n -> StrLit (s.Substring(0, n))
            | BoolLit b when (b && n > 0) || n = 0 -> value
            | _ -> raise (FlowException($"Wanted to select `{n}` but got: {value}"))
        | x -> failwith $"Expected {nLoc} to evaluate to an integer, but got {x} instead."
    
let evaluateExpr (s : State) (f : Locator -> Locator) (loc : Locator) : Locator =
    match loc with
    | Uninitialized -> raise (Exception("Expected value, got `uninitialized`"))
    | Consume -> raise (Exception("Can't use `consume` as a source!"))
    | Stdout -> raise (Exception("Can't use `stdout` as a source!"))
    | This -> s.lookup("this", f)
    | ActorRef ref -> ActorRef ref
    | VarDef name ->
        s.newVar(name)
        s.lookup(name, f)
    | VarDefTyped (name, typ) ->
        s.newVar(name, typ)
        s.lookup(name, f)
    | VarRef name -> s.lookup(name, f)
    | Select (loc, sel) ->
        let toTake = evaluateExpr s id sel
        evaluateExpr s (fun _ -> toTake) loc
    | SelectQuant (loc, quant) ->
        evaluateExpr s (resolveQuantSel s quant f) loc
    | Copy (VarRef name) -> s.lookup(name, id)
    | IntLit n -> IntLit n
    | NatLit n -> NatLit n
    | StrLit s -> StrLit s
    | BoolLit b -> BoolLit b
    | Unify _ -> raise (Exception("Can't use `unify(...)` as a source!"))
    | Combine (a, b) ->
        let aVal = evaluateExpr s f a
        let bVal = evaluateExpr s f b
        combine aVal bVal
    | Format (format, args) ->
        match resolveSrc s f format with
        | StrLit formatStr ->
            let argVals = List.map (evaluateExpr s f) args
            StrLit (String.Format(formatStr, argVals))
        | x -> raise (Exception($"Expected {format} to resolve to a string, but got: {x}"))
    | LocatorList xs -> LocatorList (List.map (evaluateExpr s f) xs)
    | x -> raise (Exception($"Unsupported expression locator: {x}"))
    
let resolveSrc (s : State) (f : Locator -> Locator) (loc : Locator) : Locator =
    let resolvedVal =
        match loc with
        | Uninitialized -> raise (Exception("Expected value, got `uninitialized`"))
        | Consume -> raise (Exception("Can't use `consume` as a source!"))
        | Stdout -> raise (Exception("Can't use `stdout` as a source!"))
        | This -> s.take("this", f)
        | ActorRef ref -> ActorRef ref
        | VarDef name ->
            s.newVar(name)
            s.take(name, f)
        | VarDefTyped (name, typ) ->
            s.newVar(name, typ)
            s.take(name, f)
        | VarRef name -> s.take(name, f)
        | Select (loc, sel) ->
            let toTake = evaluateExpr s id sel
            resolveSrc s (fun _ -> toTake) loc
        | SelectQuant (loc, quant) ->
            resolveSrc s (resolveQuantSel s quant f) loc
        | Copy (VarRef name) -> s.lookup(name, id)
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
    resolvedVal
    
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
    | ActorRef ref -> s.receiveInField(ref, "input", value)
    | FieldAccess (loc, fieldName) ->
        match evaluateExpr s id loc with
        | ActorRef ref -> s.receiveInField(ref, fieldName, value)
        | x -> failwith $"Expected to get an ActorRef from {loc} but instead got: {x}"
    | IntLit _ -> raise (Exception("Can't use an integer as a destination!"))
    | NatLit _ -> raise (Exception("Can't use a natural number as a destination!"))
    | StrLit _ -> raise (Exception("Can't use a string as a destination!"))
    | BoolLit _ -> raise (Exception("Can't use a boolean as a destination!"))
    | Unify loc -> tryUnify loc value
    | LocatorList xs ->
        match value with
        | LocatorList ys ->
            if xs.Length <> ys.Length then
                raise (FlowException($"Can't zip {xs} and {ys} together because they have different lengths!"))
            for x, y in List.zip xs ys do
                sendToDst s x y
        | LocatorStream ys ->
            if xs.Length <> ys.Length then
                raise (FlowException($"Can't zip {xs} and {ys} together because they have different lengths!"))
            for x, y in List.zip xs ys do
                sendToDst s x y
        | x -> raise (Exception($"Can only use a list as a destination if the value being sent is a list or stream, but got: {x}"))
    | x -> raise (Exception($"Unsupported destination locator: {x}"))
    
let tryUnify (a : Locator) (b : Locator) =
    match a, b with
    | IntLit n, IntLit m when n = m -> ()
    | NatLit n, NatLit m when n = m -> ()
    | StrLit s, StrLit t when s = t -> ()
    | BoolLit b, BoolLit c when b = c -> ()
    | LocatorList xs, LocatorList ys -> List.iter2 tryUnify xs ys
    | LocatorList xs, LocatorStream ys -> List.iter2 tryUnify xs ys
    | LocatorStream xs, LocatorList ys -> List.iter2 tryUnify xs ys
    | LocatorStream xs, LocatorStream ys -> List.iter2 tryUnify xs ys
    | _ -> raise (FlowException($"Could not unify {a} and {b}"))
    
let combine (a : Locator) (b : Locator) : Locator =
    match a, b with
    | IntLit n, IntLit m -> IntLit (n + m)
    | NatLit n, NatLit m -> NatLit (n + m)
    | StrLit s, StrLit t -> StrLit (s + t)
    | BoolLit b, BoolLit c -> BoolLit (b || c)
    | LocatorList xs, LocatorList ys -> LocatorList (xs @ ys)
    | LocatorList xs, LocatorStream ys -> LocatorStream (xs @ ys)
    | LocatorStream xs, LocatorList ys -> LocatorStream (xs @ ys)
    | LocatorStream xs, LocatorStream ys -> LocatorStream (xs @ ys)
    | LocatorList xs, x -> LocatorList (xs @ [x])
    | LocatorStream xs, x -> LocatorStream (xs @ [x])
    | x, Uninitialized -> x
    | x, Nothing -> x
    | Uninitialized, x -> x
    | Nothing, x -> x
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
        if s.StartsWith(t) then
            StrLit (s.Substring(t.Length))
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
