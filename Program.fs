open System.Text.RegularExpressions
open System

type Word =
    | Number of int
    | Operator of string
    | PrintCommand
    
let push n l = n::l

let pop l =
    match l with
    | [] -> failwith "can't pop an empty stack"
    | x::xs -> x, xs

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
    | "negate" | "abs" | "max" | "min" -> Operator word
    | ".s" -> PrintCommand
    | _ -> Number (int word) 

let rec readTokens tokens =
    match tokens with
    | [] -> []
    | x::xs -> matchWord(x) :: readTokens xs
    
let read input =
    input |> tokenize |> readTokens

let rec eval (words:Word list, stack:int list) =
    match words with
    | [] -> printfn "ok"
            stack
    | word::xs ->
        match word with
        | PrintCommand ->
                          let stackStr = stack |> List.rev |> List.map string |> String.concat " " 
                          printf $"<{List.length(stack)}> {stackStr} "
                          eval(xs, stack)
        | Number n -> eval(xs, push n stack)
        | Operator op ->
            match op with
            | "+" | "add" ->
                let second, stack = pop stack
                let first, stack = pop stack
                eval(xs, push (first + second) stack)
            | "-" | "sub" ->
                let second, stack = pop stack
                let first, stack = pop stack
                eval(xs, push (first-second) stack)
            | "*" | "mul" ->
                let second, stack = pop stack
                let first, stack = pop stack
                eval(xs, push (first*second) stack)
            | "/" | "div" ->
                let second, stack = pop stack
                let first, stack = pop stack
                eval(xs, push (first/second) stack)
            | "%" | "mod" ->
                let second, stack = pop stack
                let first, stack = pop stack
                eval(xs, push (first%second) stack)
            | "." ->
                let p, stack = pop stack
                printf $"{p} "
                eval(xs, stack)
            | _ -> failwith "not implemented"
                     
        
    
    

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
               loop(eval (words, stack))
    loop(List.Empty)
    0 // return an integer exit code