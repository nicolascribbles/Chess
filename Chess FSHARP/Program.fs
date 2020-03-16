open System
open System.Text
open System.Text.RegularExpressions

type Color = 
    | Black
    | White
    
type PawnState = 
    | NotMoved
    | Moved

type Rank = 
    | King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn of PawnState

type Piece = {
    Player : Color;
    Rank   : Rank;
    }

type Column =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    with static member List = [A; B; C; D; E; F; G; H]

type Row =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    with static member List = [One; Two; Three; Four; Five; Six; Seven; Eight]


type Square = (Row * Column)
type Board = Map<Square, Piece option>

let player = White

let rowlist = Row.List

let columnlist = Column.List

let blackPawn = 
    Some { 
    Player    = Black;
    Rank      = Pawn NotMoved
    }
let whitePawn = 
    Some { 
    Player    = White;
    Rank      = Pawn NotMoved
    }
let black rank = 
    Some {
    Player     = Black;
    Rank       = rank
    }
let white rank = 
    Some {
    Player     = White;
    Rank       = rank
    }

let matchRow ( row : string ) : Row =
    match row with
    | "1" | "One"   | "one"   -> One
    | "2" | "Two"   | "two"   -> Two
    | "3" | "Three" | "three" -> Three
    | "4" | "Four"  | "four"  -> Four
    | "5" | "Five"  | "five"  -> Five
    | "6" | "Six"   | "six"   -> Six
    | "7" | "Seven" | "seven" -> Seven
    | "8" | "Eight" | "eight" -> Eight
    | _ -> One

let matchColumn ( col : string ) : Column =
    match col with
    | "A" | "a" -> A
    | "B" | "b" -> B
    | "C" | "c" -> C
    | "D" | "d" -> D
    | "E" | "e" -> E
    | "F" | "f" -> F
    | "G" | "g" -> G
    | "H" | "h" -> H
    | _ -> A

let makeRow row pieces =
    let cells = Column.List |> List.map(fun col -> (row, col))
    List.zip cells pieces

let board =
    Map (   (makeRow Eight [black Rook; black Knight; black Bishop; black King; black Queen; black Bishop; black Knight; black Rook]) @
            (makeRow Seven [blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn]) @
            (makeRow Six [None;None;None;None;None;None;None;None]) @
            (makeRow Five [None;None;None;None;None;None;None;None]) @
            (makeRow Four [None;None;None;None;None;None;None;None]) @
            (makeRow Three [None;None;None;None;None;None;None;None;]) @
            (makeRow Two [whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn]) @
            (makeRow One [white Rook; white Knight; white Bishop; white King; white Queen; white Bishop; white Knight; white Rook]) )

let chessBoard () =
    printfn " "
    printfn " "
    printfn " "
    printf "     "
    for column in columnlist do 
        printf " %A  " column
    printfn "\n\r     _______________________________"
    for i in 8 .. -1 .. 1 do
        printf "\n\r %i | " i
    
        let row = i.ToString()
        let matchedRow = matchRow row
    
        for column in columnlist do
            let matchedKey : Square = (matchedRow, column)
            match board.[matchedKey] with
            | Some piece -> 
                match piece.Rank with
                | King ->
                    match piece.Player with
                    | White -> printf " \u2654 "
                    | Black -> printf " \u265A "
                | Queen ->
                    match piece.Player with
                    | White -> printf " \u2655 "
                    | Black -> printf " \u265B "
                | Bishop ->
                    match piece.Player with
                    | White -> printf " \u2657 "
                    | Black -> printf " \u265D "
                | Knight ->
                    match piece.Player with
                    | White -> printf " \u2658 "
                    | Black -> printf " \u265E "
                | Rook ->
                    match piece.Player with
                    | White -> printf " \u2656 "
                    | Black -> printf " \u265C "
                | Pawn Moved ->
                    match piece.Player with
                    | White -> printf " \u2659 "
                    | Black -> printf " \u265F "
                | Pawn NotMoved ->
                    match piece.Player with
                    | White -> printf " \u2659 "
                    | Black -> printf " \u265F "
            | None -> printf "  "

    printfn " "
    printfn " "
    printfn " "   
    
let rec queryfunc key (map : Board) =
    if (board.ContainsKey key) then
        match board.[key] with
        | Some piece -> 
            printfn "\n\r%A has a %A %A there!" key piece.Player piece.Rank
            chessBoard()
        | None -> printf "%A is vacant.\n\r" key 


// IF queryfunc contains some piece = printf "\n\rWould you like to move it?\n\r"

// WELCOME!
printfn "\n\rWelcome to this ordinary game of chess\n\r
where you can only find out what lives in each square!"

chessBoard()

// The access key format is Row * Column

let mutable inpRowValidation = false
let mutable inpColumnValidation = false

let rec queryBoard() = 

    // Get the row
    printf "Please select the row ( 1 - 8 ) : "
    let mutable rowInput : string = 
        System.Console.ReadLine()
    
    //Validate the row
    let rec rowCheck(rowInput : string) =

        // Any of the type names, not case sensitive, or integers 1-8 in string format
        inpRowValidation <-
            Regex.IsMatch(rowInput, @"[1-8]|One|Two|Three|Four|Five|Six|Seven|Eight", RegexOptions.IgnoreCase)
    
        if inpRowValidation = false then

            printfn "\n\rI'm sorry, this input is invalid."


            // Ask again
            printf "\n\rPlease select the row ( 1 - 8 ) : "
            let mutable rowInput = System.Console.ReadLine()
            rowCheck(rowInput : string)

        else if inpRowValidation = true then
        
            let row = 
                matchRow (rowInput : string)

            printfn "\n\r(%A, __)\n\r" row
    rowCheck(rowInput : string)       


    // Get the column
    printf "Please select the column ( A - H ) : "
    let mutable columnInput : string = System.Console.ReadLine()

    // Validate the column
    let rec columnCheck (columnInput : string) =
        // if the row doesn't have letters a-h it will print an error
        inpColumnValidation <- Regex.IsMatch(columnInput, @"[A-H]", RegexOptions.IgnoreCase)

        if inpColumnValidation = false then
            printfn "\n\rI'm sorry, this input is invalid.\n\r"

            
            // Ask again
            printf "Please select the column ( A - H ) : "
            let mutable columnInput = System.Console.ReadLine()
            columnCheck(columnInput)

      
    
    columnCheck(columnInput)

    // If it passes all checks, continue with the query
    if inpColumnValidation = true && inpRowValidation = true then
        let row =
            matchRow rowInput
            
        let column =
            matchColumn columnInput

        let tupleInput =  (row, column) : Square


        printfn " "
        queryfunc tupleInput board
        printfn " "
    

    queryBoard()


queryBoard()
        

System.Console.ReadKey() |> ignore