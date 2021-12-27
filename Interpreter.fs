module rec Interpreter

open Microsoft.FSharp.Collections
open Parser

type State() =
    let mutable store : Map<string, Locator> = Map.empty

let rec evaluate (s : State) (stmt : Statement) : State = State()
