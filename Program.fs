open System.Text.RegularExpressions
open System

type Word =
    | Number of int
    | Operator of string
    | PrintCommand
    
let push n l = n::l

let pop l =
    match l with
    | [] -> printfn "Error: can't pop an empty stack"
            None
    | x::xs -> Some(x, xs)
    
let peek l =
    match l with
    | [] -> printfn "Error: can't peek on an empty stack"
            None
    | x::_ -> Some(x,l)
    
let duplicate (result: (int * int list) option) =
    match result with
    | None -> None
    | Some (n, stack) -> Some(n, n::stack)
    
let unaryOperation (result: (int * int list) option) (operation: int->int) =
    match result with
    | None -> None
    | Some (n, stack) ->
        let r = (operation n)
        Some (r, r::stack)
    
let binaryOperation (result: (int * int list) option) (operation: int->int->int) =
    match result with
    | None -> None
    | Some (second, stack) ->
        match pop stack with
        | None -> None
        | Some (first, stack) ->
            let n = (operation first second)
            Some (n, n::stack)

let tokenize input =
    let regex = Regex("(\\s*[()\\[\\]{}'\`~,^@\\s]\\s*|;.*|[^\\s,()\\[\\]{}\"'`~@^;]+)")

    regex.Matches(input)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value.Trim())
    |> Seq.filter (fun s -> s <> "")
    |> Seq.toList



let matchWord word =
    match word with
    | "+" | "-" | "/" | "*" | "%"
    | "."
    | "add" | "sub" | "div" | "mul" | "mod"
    | "negate" | "abs" | "max" | "min"
    | "dup" | "swap" | "rot" | "drop" | "nip" -> Operator word
    | ".s" -> PrintCommand
    | _ -> Number (int word) 

let rec readTokens tokens =
    match tokens with
    | [] -> []
    | x::xs -> matchWord(x) :: readTokens xs
    
let read input =
    input |> tokenize |> readTokens

let rec eval words stack =
    match words with
    | [] -> printfn "ok"
            stack
    | word::xs ->
        match word with
        | PrintCommand ->
                          let stackStr = stack |> List.rev |> List.map string |> String.concat " " 
                          printf $"<{List.length(stack)}> {stackStr} "
                          eval xs stack
        | Number n -> eval xs (push n stack)
        | Operator op ->
            let newStack = 
                match op with
                | "+" | "add" -> binaryOperation (pop stack) (+)
                | "-" | "sub" -> binaryOperation (pop stack) (-)
                | "*" | "mul" -> binaryOperation (pop stack) (*)
                | "/" | "div" -> binaryOperation (pop stack) (/)
                | "%" | "mod" -> binaryOperation (pop stack) (%)
                | "min" -> binaryOperation (pop stack) (fun a b -> if a < b then a else b)
                | "max" -> binaryOperation (pop stack) (fun a b -> if a > b then a else b)
                | "negate" -> unaryOperation (pop stack) (fun n -> n * -1)
                | "abs" -> unaryOperation (pop stack) (fun n -> if n < 0 then -n else n)
                | "dup" -> duplicate (peek stack)
                | "." ->
                    match pop stack with
                    | None -> None
                    | Some (p, stack) -> printf $"{p} "; Some (p, stack)
                | _ -> failwith "not implemented"
            match newStack with
            | None -> eval xs stack
            | Some (_, stack) -> eval xs stack
                     
        
    
    

[<EntryPoint>]
let main argv =
    printfn "Welcome to Fifth, type \"bye\" to exit"
    printfn $"{argv}"
    let rec loop stack =
        printf "fifth> "
        let input = Console.ReadLine()
        match input with
        | "bye" -> ()
        | _ ->
               let words = input |> read
               loop(eval words stack)
    loop(List.Empty)
    0 // return an integer exit code