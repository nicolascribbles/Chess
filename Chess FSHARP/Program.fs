open System
open System.Text
open System.Text.RegularExpressions


// Pieces & Players
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


// Board
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

type Status =
    | InProgress

type State = {
    Board : Board ;
    Status : Status ;
    ThisPlayer : Color ;
    Message : string ;
    }

// Access Helpers
let rowlist = Row.List
let columnlist = Column.List

// Move types
type moveQuery = { From : Square ; To: Square}
type movePieceQuery = { ThisPiece : Piece ; From : Square ; To: Square}


let mutable inpRowValidation = false
let mutable inpColumnValidation = false

// Functions
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
    
// Starting board
let mutable board =
    Map (   (makeRow Eight [black Rook; black Knight; black Bishop; black King; black Queen; black Bishop; black Knight; black Rook]) @
            (makeRow Seven [blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn]) @
            (makeRow Six [None;None;None;None;None;None;None;None]) @
            (makeRow Five [None;None;None;None;None;None;None;None]) @
            (makeRow Four [None;None;None;None;None;None;None;None]) @
            (makeRow Three [None;None;None;None;None;None;None;None;]) @
            (makeRow Two [whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn]) @
            (makeRow One [white Rook; white Knight; white Bishop; white King; white Queen; white Bishop; white Knight; white Rook]) )
    
let gameInit ()=
    let player = White
    {
        Board = board ;
        Status = InProgress ;
        ThisPlayer = player ;
        Message = sprintf "%A to move" player ;
    }

gameInit ()

// Printed Board
let chessBoard (board: Board) =
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

// Query Function 
let rec queryfunc key (map : Board) =
    if (board.ContainsKey key) then
        match board.[key] with
        | Some piece -> 
            printfn "\n\r%A has a %A %A there!" key piece.Player piece.Rank
            chessBoard(board)
        | None -> printfn "%A is vacant.\n\r\n\r" key 

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


// WELCOME!
printfn "\n\rWelcome to this ordinary game of chess\n\r
where you can only find out what lives in each square!"

chessBoard board

// The access key format is Row * Column





// Get the key function (returns columnInput & rowInput)

        //key : Square <- 
        //    (row, column)


let rec queryBoard() = 
    // Get the row
    printf "Please select the row ( 1 - 8 ) : "
    let mutable rowInput : string = 
        System.Console.ReadLine()
    // Validate it
    rowCheck(rowInput : string)

    // Get the column
    printf "Please select the column ( A - H ) : "
    let mutable columnInput : string = System.Console.ReadLine()      
    // Validate it
    columnCheck(columnInput)
    
    if inpColumnValidation = true && inpRowValidation = true then
        let mutable row : Row =
            matchRow rowInput
        
        let mutable column : Column =
            matchColumn columnInput

        let mutable key : Square = (row, column)
        
        printfn " "
        // Queryfunc returns piece information, tuple input is my key, and it's querying the board
        queryfunc key board
        printfn " "

        let square = board.[key]
        if (board.ContainsKey key) then
            match square with
            | Some piece ->  
                let myPiece : Piece option = Some {Player = piece.Player ; Rank = piece.Rank}
                printf "Would you like to move it ? (y/n) "
                let mutable input = System.Console.ReadLine()
                match input with
                | "y" -> 
                    
                    

                    printfn "Where would you like to move your %A %A ?\n\r" piece.Player piece.Rank
                    // Get the row
                    

                    // Get the row
                    printf "Please select the row ( 1 - 8 ) : "
                    let mutable toRowInput : string = 
                        System.Console.ReadLine()
                    // Validate it
                    rowCheck(toRowInput : string)
                    
                    // Get the column
                    printf "Please select the column ( A - H ) : "
                    let mutable toColumnInput : string = System.Console.ReadLine()      
                    // Validate it
                    columnCheck(toColumnInput)

                    let mutable toRow = matchRow toRowInput

                    let mutable toColumn = matchColumn toColumnInput
                     

                    let mutable toKey : Square = (toRow, toColumn)

                    match piece.Rank with
                    | Pawn NotMoved -> { Player = piece.Player ; Rank = Pawn Moved }
                    | _ -> { Player = piece.Player ; Rank = piece.Rank }
                    |> ignore

                    match board.[toKey] with
                    | Some piece -> 
                        printfn "\n\rYou have captured a %A %A!\n\r" piece.Player piece.Rank
                        
                        board <- board.Add(toKey, None).Add(key, None).Add(toKey, myPiece)

                    | None ->
                        board <- board.Add(key, None).Add(toKey, myPiece)

                    chessBoard board
                    queryBoard()

                | "n" -> queryBoard()
                | _ -> 
                    printfn "Select y or n: \n\r" 
                    queryBoard()
            | None -> 
                printf "There is no piece there"
                queryBoard()

   
    
    // If it passes all checks, continue with the query

queryBoard() |> ignore

System.Console.ReadKey() |> ignore