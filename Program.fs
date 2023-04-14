open System.Text.RegularExpressions
open System

type Word =
    | Number of int
    | Operator of string
    | RelationalOperator of string
    | StartCompile
    | EndCompile
    | WordName of string
    | PrintCommand
    | PrintCompiledWords
    | SeeCompiledWord
    | If
    | Else
    | EndIf
    | String of string
    | PrintTopOfStack
    | NewLine
    | ForLoop
    | EndForLoop
    
type CompiledWord = (Word * Word list)
    
let mutable compiledWords: CompiledWord list = []

let push n l = n :: l

let pop l =
    match l with
    | [] ->
        printfn "Error: can't pop an empty stack"
        None
    | x :: xs -> Some(x, xs)

let peek l =
    match l with
    | [] ->
        printfn "Error: can't peek on an empty stack"
        None
    | x :: _ -> Some(x, l)

let duplicate (result: (int * int list) option) =
    match result with
    | None -> None
    | Some (n, stack) -> Some(n :: stack)

let unaryOperation (result: (int * int list) option) (operation: int -> int) =
    match result with
    | None -> None
    | Some (n, stack) ->
        let r = (operation n)
        Some(r :: stack)

let rec rollHelper index bottomStack (topStack: int list) =
    match index with
    | 0 -> List.head bottomStack, (topStack @ (List.tail bottomStack))
    | _ -> rollHelper (index - 1) (List.tail bottomStack) (topStack @ [ List.head bottomStack ])

let roll index stack =
    let newHead, newTail = rollHelper index stack []
    newHead::newTail
    
let rec pickHelper index (stack: int list) =
    match index with
    | 0 -> List.head stack
    | _ -> pickHelper (index - 1) (List.tail stack)
             
let pick index stack =
    (pickHelper index stack)::stack

let swap (result: (int * int list) option) =
    match result with
    | None -> None
    | Some (second, stack) ->
        match pop stack with
        | None -> None
        | Some (first, stack) -> Some([ first; second ] @ stack)

let rotate (result: (int * int list) option) =
    match result with
    | None -> None
    | Some (second, stack) ->
        match pop stack with
        | None -> None
        | Some (third, stack) ->
            match pop stack with
            | None -> None
            | Some (first, stack) -> Some([ first; second; third ] @ stack)

let binaryOperation (result: (int * int list) option) (operation: int -> int -> int) =
    match result with
    | None -> None
    | Some (second, stack) ->
        match pop stack with
        | None -> None
        | Some (first, stack) ->
            let n = (operation first second)
            Some(n :: stack)

let tokenize input =
//    let regex = Regex("(\\s*[()\\[\\]{}'\`~,^@\\s]\\s*|;.*|[^\\s,()\\[\\]{}\"'`~@^;]+)")
    let regex = Regex("(\\s*[()\\[\\]{}'\`~,^@\\s]\\s*|;.*|\"([^\"]*)\"|[^\\s,()\\[\\]{}\"'`~@^;]+)")
    regex.Matches(input)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value.Trim())
    |> Seq.filter (fun s -> s <> "")
    |> Seq.toList
    
let removeComments input =
    let regex = Regex("(\(.*?\))|#.*")
    regex.Replace(input, "")

let matchWord word =
    let mutable num = 0
    match word with
    | "+"
    | "-"
    | "/"
    | "*"
    | "%"
    | "."
    | "add"
    | "sub"
    | "div"
    | "mul"
    | "mod"
    | "negate"
    | "abs"
    | "max"
    | "min"
    | "dup"
    | "swap"
    | "rot"
    | "drop"
    | "nip"
    | "tuck"
    | "over"
    | "roll"
    | "pick" -> Operator word
    | "=" | "<" | "<=" | ">" | ">=" -> RelationalOperator word
    | "if" -> If
    | "else" -> Else
    | "endif" -> EndIf
    | ".s" -> PrintCommand
    | ".c" -> PrintCompiledWords
    | "printindex" -> PrintTopOfStack
    | "newline" -> NewLine
    | "see" -> SeeCompiledWord
    | ":" -> StartCompile
    | ";" -> EndCompile
    | "do" -> ForLoop
    | "loop" -> EndForLoop
    | _ when Int32.TryParse(word, &num) -> Number(int word)
    | _ when word[0] = '"' -> String word
    | _ -> WordName word
    
let rec readTokens tokens =
    match tokens with
    | [] -> []
    | x :: xs -> matchWord x :: readTokens xs

let read input = input |> removeComments |> tokenize |> readTokens

let rec makeNewWordHelper name body words =
    match words with
    | EndCompile::tail -> ((name, (List.rev body)), tail)
    | word::tail -> makeNewWordHelper name (word::body) tail
    
let makeNewWord words =
    // handle list stuff
    match (makeNewWordHelper (List.head words) [] (List.tail words)) with
    | newWord, xs -> compiledWords <- newWord::compiledWords
                     xs
                     
let rec callWordList name (l:CompiledWord list) =
    let n, w = List.head l
    let n =
        match n with
        | WordName wname -> wname
    if name = n then
        w
    else
        callWordList name (List.tail l)
        
//let rec compileWordList wordList stack 
        
let rec printWordsOfString wordList =
    match wordList with
    | [] -> printf ""
    | word::tail ->
        match word with
        | Operator n ->
            printf $"{n} "
            printWordsOfString tail
        | WordName n ->
            printf $"{n} "
            printWordsOfString tail
        | If ->
            printf "if "
            printWordsOfString tail
        | Else ->
            printf "else "
            printWordsOfString tail
        | EndIf ->
            printf "endif "
            printWordsOfString tail
            
let rec moveToElseOrEnd (l:Word list) =
    match l with
    | [] -> []
    | Else::xs -> xs
    | EndIf::xs -> xs
    | _::xs -> moveToElseOrEnd xs            
    

let rec eval words stack =
    match words with
    | [] ->
        printfn "ok"
        stack
    | word :: xs ->
        match word with
        | PrintCommand ->
            let stackStr = stack |> List.rev |> List.map string |> String.concat " "
            printf $"<{List.length stack}> {stackStr} "
            eval xs stack
        | PrintCompiledWords ->
            printfn $"{compiledWords}"
            eval xs stack
        | SeeCompiledWord ->
            let wordName =
                match (List.head xs) with
                | WordName n -> n
            let wordContent = callWordList wordName compiledWords
            printf $": {wordName} "
            printWordsOfString wordContent
            printf "; "
            eval [] stack
        | Number n -> eval xs (push n stack)
        | Operator op ->
            let newStack =
                match op with
                | "+"
                | "add" -> binaryOperation (pop stack) (+)
                | "-"
                | "sub" -> binaryOperation (pop stack) (-)
                | "*"
                | "mul" -> binaryOperation (pop stack) (*)
                | "/"
                | "div" -> binaryOperation (pop stack) (/)
                | "%"
                | "mod" -> binaryOperation (pop stack) (%)
                | "min" -> binaryOperation (pop stack) (fun a b -> if a < b then a else b)
                | "max" -> binaryOperation (pop stack) (fun a b -> if a > b then a else b)
                | "negate" -> unaryOperation (pop stack) (fun n -> n * -1)
                | "abs" -> unaryOperation (pop stack) (fun n -> if n < 0 then -n else n)
                | "dup" -> duplicate (peek stack)
                | "swap" -> swap (pop stack)
                | "rot" -> rotate (pop stack)
                | "drop" ->
                    match pop stack with
                    | None -> None
                    | Some (_, stack) -> Some stack
                | "nip" ->
                    match pop stack with
                    | None -> None
                    | Some (first, stack) ->
                        match pop stack with
                        | None -> None
                        | Some (_, stack) -> Some(first :: stack)
                | "tuck" ->
                    match pop stack with
                    | None -> None
                    | Some (top, stack) ->
                        match pop stack with
                        | None -> None
                        | Some (second, stack) -> Some([ top; second; top ] @ stack)
                | "over" ->
                    match pop stack with
                    | None -> None
                    | Some (top, stack) ->
                        match pop stack with
                        | None -> None
                        | Some (second, stack) -> Some([ second; top; second ] @ stack)
                | "roll" ->
                    match pop stack with
                    | None -> None
                    | Some (index, stack) ->
                        if index < 0 then
                            printfn "Error: roll given a negative index"
                            Some stack
                        elif index > (List.length stack - 1) then
                            printfn "Error: stack too small for roll"
                            Some stack
                        else
                            Some (roll index stack)
                | "pick" ->
                    match pop stack with
                    | None -> None
                    | Some (index, stack) ->
                        if index < 0 then
                            printfn "Error1: pick given a negative index"
                            Some stack
                        elif index > (List.length stack - 1) then
                            printfn "Error1: stack too small for pick"
                            Some stack
                        else
                            Some (pick index stack)
                | "." ->
                    match pop stack with
                    | None -> None
                    | Some (p, stack) ->
                        printf $"{p} "
                        Some stack
                | _ -> failwith "not implemented"
            match newStack with
            | None -> eval xs stack
            | Some stack -> eval xs stack
        | RelationalOperator op ->
            let newStack =
                match op with
                | "=" ->
                    match pop stack with
                    | None -> None
                    | Some (second, stack) ->
                        match pop stack with
                        | None -> None
                        | Some (first, stack) ->
                            if first = second then
                                Some (1::stack)
                            else
                                Some (0::stack)
                | "<" -> binaryOperation (pop stack) (fun a b -> if a < b then 1 else 0)
                | "<=" -> binaryOperation (pop stack) (fun a b -> if a <= b then 1 else 0)
                | ">" -> binaryOperation (pop stack) (fun a b -> if a > b then 1 else 0)
                | ">=" -> binaryOperation (pop stack) (fun a b -> if a >= b then 1 else 0)
                | _ -> failwith "not implemented"
            match newStack with
            | None -> eval xs stack
            | Some stack -> eval xs stack
        | StartCompile -> eval (makeNewWord xs) stack
        | WordName s -> eval ((callWordList s compiledWords) @ xs) stack
        | String s ->
            printf $"{s} "
            eval xs stack
        | If ->
            match pop stack with
            | None -> eval xs stack //undefined?
            | Some (result, stack) ->
                if (result = 1) then
                    eval xs stack
                else
                    eval (moveToElseOrEnd xs) stack
        | Else -> eval (moveToElseOrEnd xs) stack
        | EndIf -> eval xs stack
        | NewLine ->
            printfn ""
            eval xs stack
        | PrintTopOfStack ->
            printf $"{List.head stack} "
            eval xs stack
            
        

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
            loop (eval words stack)

    loop List.Empty
    0 // return an integer exit code
