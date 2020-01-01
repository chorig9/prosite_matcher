module List =
    let forAny predicate list =
        not (List.forall (predicate >> not) list)

type OptionBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x

    member this.Combine(a, b) =
        match a, b with
        | Some ua, Some ub -> Some (ua, ub)
        | _ -> None

    member this.Zero() =
        None

    member this.Delay(f) =
        f()

let traverseOptionM f list =
    let (>>=) x f = Option.bind f x

    let initState = Some []
    let folder head tail = 
        f head >>= (fun h -> 
        tail >>= (fun t ->
        Some (h::t) ))
 
    List.foldBack folder list initState

let flip f x y z = f x z y
 
type Pattern =
    | AnyLetter
    | Letter of char
    | OneOf of char list
    | OneNotOf of char list

type RepeatedPattern =
    | Repeated of Pattern * int
    | OptionalRepeated of Pattern * int

type State = 
    | EndState
    | State of Transition list
and Transition =
    | EpsilonTransition of State
    | SymbolTransition of Pattern * State

type ProcessedState = {state: State; index: int}

let ProcessedStateGetState x =
    match x with {state = s ; index = cnt} -> s

let ProcessedStateGetIndex x =
    match x with {state = s ; index = cnt} -> cnt

let getState transition =
    match transition with
    | EpsilonTransition state -> state
    | SymbolTransition (_, state) -> state

let isEndState state =
    match state with
    | EndState -> true
    | _ -> false

let isEpsilon transition =
    match transition with
    | EpsilonTransition _ -> true
    | _ -> false

let extractTransitions state =
    match state with
    | EndState -> []
    | State transitions -> transitions

let split delimiter sequence = 
    let folder element state =
        match element, state with
            | d, _ when d.Equals delimiter -> []::state
            | _, [] -> [element]::state
            | _, head::tail -> (element::head)::tail

    List.foldBack folder sequence [[]]

let toInt (input : char list) =
    match System.Int32.TryParse (input |> List.map string |> List.reduce (+)) with
    | true, num -> Some num
    | _ -> None

let parseMultiplication sequence =
    let option = OptionBuilder()

    match sequence with
    | [] -> Some (1, 1)
    | '('::rst ->
        match (rst |> split ')') with
        | head::[[]] ->
            match (head |> split ',') with
            | [num1] ->
                option {
                    let! int1 = num1 |> toInt
                    return int1
                    return int1
                }
            | num1::[num2] -> 
                option {
                    let! int1 = toInt num1
                    let! int2 = toInt num2

                    if int1 <= int2 && int2 > 0 then
                        return int1
                        return int2
                }
            | _ -> None  
        | _ -> None
    | _ -> None

let toPatterns sequence =
    let withMuplitplication pattern sequence =
        parseMultiplication sequence
        |> Option.bind (fun mult -> 
            let min = fst mult
            let max = snd mult
            
            let obligatoryPattern = Repeated (pattern, min)
            if min = 0 then
                Some [OptionalRepeated (pattern, max)]
            elif max - min > 0 then
                Some [obligatoryPattern ; OptionalRepeated (pattern, max - min)]
            else
                Some [obligatoryPattern]
        )

    match sequence with
    | 'x'::rst ->
        withMuplitplication AnyLetter rst
    | '['::rst -> 
        match (sequence |> split ']') with
        | extracted::[tail] -> withMuplitplication (OneOf extracted) tail
        | _ -> None
    | '{'::rst ->
        match (sequence |> split '}') with
        | extracted::[tail] ->  withMuplitplication (OneNotOf extracted) tail
        | _ -> None
    | letter::rst -> 
        withMuplitplication (Letter letter) rst
    | _ -> None

let patternMatches pattern letter =
   match pattern with
   | AnyLetter -> true
   | Letter l when l.Equals letter -> true
   | OneOf ls when List.contains letter ls -> true
   | OneNotOf ls when not (List.contains letter ls) -> true
   | _ -> false

let makeOptionalNode pattern count nextNode =
    let folder pattern nextState =
        let symbolState = State [SymbolTransition(pattern, nextState)]
        State [EpsilonTransition(nextNode) ; EpsilonTransition(symbolState)]

    pattern
    |> List.replicate count
    |> flip List.foldBack folder nextNode

let makeNode pattern count nextNode =
    let folder pattern nextState =
        State [SymbolTransition(pattern, nextState)]

    pattern
    |> List.replicate count
    |> flip List.foldBack folder nextNode

let buildGraph patternList =
    let initialState = EndState
    let folder pattern state =
        match pattern with
        | Repeated (p, num) -> makeNode p num state
        | OptionalRepeated (p, num) -> makeOptionalNode p num state

    patternList
    |> flip List.foldBack folder initialState

let rec addNextState nextStates state =
    let retn = (fun cnt s-> {state = s ; index = cnt})

    match state with
    | {state = State transitions ; index = cnt } when List.forAny isEpsilon transitions ->
        transitions
        |> List.collect (getState >> (retn cnt) >> (addNextState nextStates))
    | _ ->
        state::nextStates

let transitionMatchesPattern symbol transition =
    match transition with
    | SymbolTransition (pattern, _) when patternMatches pattern symbol -> true
    | _ -> false

let getNextStates symbol state =
    match state with
    | {state = EndState ; index = _} -> [state]
    | {state = State transitions ; index = cnt } ->
        transitions
        |> List.filter (transitionMatchesPattern symbol)
        |> List.map (fun s -> {state = getState s; index = cnt + 1})

let matchPattern input patternGraph =
    let startState = 
        {state = State[EpsilonTransition (patternGraph)] ; index = -1}
        |> addNextState []

    let folder states letter =
        states
        |> List.collect (getNextStates letter)
        |> List.collect (addNextState [])

    input 
    |> List.fold folder startState
    |> List.filter (ProcessedStateGetState >> isEndState)
    |> List.map ProcessedStateGetIndex

let buildMatcher pattern =
    pattern
    |> Seq.toList
    |> split '-'
    |> traverseOptionM toPatterns
    |> Option.bind (List.concat >> buildGraph >> Some)

let matchExpression pattern sequence =
    buildMatcher pattern
    |> Option.bind ((sequence |> Seq.toList |> matchPattern) >> Some)

let pattern = "[abcd](3)-x(0,2)-b"
let input = "abdbbb"

let test = matchExpression pattern input
