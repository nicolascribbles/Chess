open System.Text.RegularExpressions
open System.Drawing

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

type Square = (Column * Row)

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

let columnlist = Column.List
let rowlist = Row.List

// Move types
type moveQuery = { From : Square ; To: Square}
type movePieceQuery = { ThisPiece : Piece ; From : Square ; To: Square}

type distance = { Horizontal: int; Vertical: int}

let mutable inpRowValidation = false
let mutable inpColumnValidation = false


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

let makeRow row pieces =
    let cells = Column.List |> List.map(fun col -> (col, row))
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
    
// Printed Board
let chessBoard (board: Board) =
    printfn " "
    printf "     "
    for column in columnlist do 
        printf " %A  " column
    printfn "\n\r     ____________________________"
    for i in 8 .. -1 .. 1 do
        printf "\n\r %i  | " i 
    
        let row = i.ToString()
        let matchedRow = matchRow row
    
        for column in columnlist do
            let matchedKey : Square = (column, matchedRow)
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
            | None -> printf "    "
    printfn " "   

// Query Function 
let rec queryfunc key (board : Board) =
    if (board.ContainsKey key) then
        match board.[key] with
        | Some piece -> 
            printfn "\n\r%A has a %A %A there!" key piece.Player piece.Rank
            chessBoard(board)
        | None -> printfn "%A is vacant.\n\r\n\r" key 


// Validate the column
let rec columnCheck (columnInput : string) =
    // if the row doesn't have letters a-h it will print an error
    inpColumnValidation <- Regex.IsMatch(columnInput, @"[A-H]", RegexOptions.IgnoreCase)

    if inpColumnValidation = false then
        printfn "\n\rWow, that's embarrassing. Try again, lightbulb.\n\r"

        
        // Ask again
        printf "Please select the column ( A - H ) : "
        let mutable columnInput = System.Console.ReadLine()
        columnCheck(columnInput)

    else if inpColumnValidation = true then

        let column =
            matchColumn(columnInput)
        printfn "\n\r(%A, __)\n\r" column


//Validate the row
let rec rowCheck(rowInput : string) =

    // Any of the type names, not case sensitive, or integers 1-8 in string format
    inpRowValidation <-
        Regex.IsMatch(rowInput, @"[1-8]|One|Two|Three|Four|Five|Six|Seven|Eight", RegexOptions.IgnoreCase)

    if inpRowValidation = false then

        printfn "\n\rIt's wildly impressive how wrong you just were. It's spelled out in front of you! Pick a number 1 through 8."

        // Ask again
        printf "\n\rPlease select the row ( 1 - 8 ) : "
        let mutable rowInput = System.Console.ReadLine()
        rowCheck(rowInput : string)

// Starting board
let mutable p = White
let mutable b =
    Map (   (makeRow Eight [black Rook; black Knight; black Bishop; black King; black Queen; black Bishop; black Knight; black Rook]) @
            (makeRow Seven [blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn;blackPawn]) @
            (makeRow Six [None;None;None;None;None;None;None;None]) @
            (makeRow Five [None;None;None;None;None;None;None;None]) @
            (makeRow Four [None;None;None;None;None;None;None;None]) @
            (makeRow Three [None;None;None;None;None;None;None;None;]) @
            (makeRow Two [whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn;whitePawn]) @
            (makeRow One [white Rook; white Knight; white Bishop; white King; white Queen; white Bishop; white Knight; white Rook]) )
let mutable s =
    {
        Board = b ;
        Status = InProgress ;
        ThisPlayer = p ;
        Message = sprintf "%A to move" p ;
    }
// : :
// : :
// : :
// : :_______    :  :
// :  _____  |   :  :
// : :     : :       
// : :     : :   :  :
// : :     : :   :  :
// : :     : :   :  :
// : :     : :   :  :
// : :     : :   :  :
// : :     : :   :  :

// WELCOME!
[<EntryPoint>]
let Game args =
    printfn "\n\rWelcome to this lawless game of chess\n\r
    where you can move whatever the hell you want!"
    let mutable board : Board = b
    let mutable state : State = s
    // Chessboard prints the board
    chessBoard board

    let mutable player : Color = p
    
    printfn "%A goes first." player


    // The access key format is Column * Row
    let rec queryBoard ( ) = 
  

        // Starting information
        printfn "\n\rThis game is %A." state.Status
        printfn "%s \n\r" state.Message

        // Get the column
        printf "Please select the column ( A - H ) : "
        let mutable columnInput : string = System.Console.ReadLine()
        // Validate it
        columnCheck(columnInput)
    
    
        // Get the row
        printf "Please select the row ( 1 - 8 ) : "
        let mutable rowInput : string = 
            System.Console.ReadLine()
        // Validate it
        rowCheck(rowInput : string)

        // If it passes both tests, then tuple it into a key and return the query then query a move
        if inpColumnValidation = true && inpRowValidation = true then

            let mutable column : Column =
                matchColumn columnInput
        
            let mutable row : Row =
                matchRow rowInput

            let mutable key : Square = (column, row)

            // Queryfunc returns piece information,
            // my key is querying the board for piece or vacancy
            printfn " "
            queryfunc key board
            printfn " "

            let square = board.[key]

            let mutable player = state.ThisPlayer

            let rec moves ( ) =
                if (board.ContainsKey key) then
                    match square with
                    | Some piece -> 
                        let pieceColor = piece.Player
                        
                        if ( player = pieceColor ) then
                            let mutable myPiece : Piece option = Some {Player = piece.Player ; Rank = piece.Rank}
                            printf "\n\r\n\r Would you like to move your %A %A ? (y/n) " piece.Player piece.Rank
                            let mutable input = System.Console.ReadLine()
                            match input with
                            | "y" | "Y" -> 
                                printfn "\n\rWhere would you like to move your %A %A ?\n\r" piece.Player piece.Rank
                    
                                // Get the column
                                printf "Please select the column ( A - H ) : "
                                let mutable toColumnInput : string = System.Console.ReadLine()      

                                // Validate it
                                columnCheck(toColumnInput)
                        
                                // Get the row
                                printf "Please select the row ( 1 - 8 ) : "
                                let mutable toRowInput : string = 
                                    System.Console.ReadLine()

                                // Validate it
                                rowCheck(toRowInput : string)

                    
                                if inpColumnValidation = true && inpRowValidation = true then

                                    let mutable toColumn = matchColumn toColumnInput
                     
                                    let mutable toRow = matchRow toRowInput

                                    let mutable toKey : Square = (toColumn, toRow)



                                    // Validate the move - Maybe set up for boolean
                                    let moveValidation (key) (toKey) = 
                                    (*
                                        I need the distance first so I can count the number of squares it jumps from one to the other
                                        I need to say that columns are vertical (up and down)
                                        I need to say that rows are horizontal (left and right)
                                        I need a record for this too so i gotta make it a type
                                    *)

                                        let mutable toKey : Square = (toColumn, toRow)

                                        let distance ( move : movePieceQuery ) =
                                            let (fx, fy) = move.From
                                            let (tx, ty) = move.To

                                            let xA = 
                                                ((Column.List |> List.findIndex (fun c -> c = tx)) - 
                                                    (Column.List |> List.findIndex(fun c -> c = fx)))

                                            let yA = 
                                                ((Row.List |> List.findIndex (fun  c -> c = ty)) - 
                                                    (Row.List |> List.findIndex (fun c -> c = fy)))

                                            { Horizontal = xA ; Vertical = yA ; }

                                        let validatePieceandRank (board : Board) player (move: movePieceQuery) =

                                            let mutable d = distance move
                                            let dCoords = ((abs d.Horizontal), (abs d.Vertical))
                                            
                                            // Pawn move rules
                                            let pawnMove pawnState move =
                                            (* 
                                                Pawn    :
                                                    - Can only move forward unless it's capturing (landing in square diagonally from other player)
                                                    - On first turn, can move 2 spaces
                                                    - On every turn after, they can only move forward 1 space (remember to capture diagonally)
                                                    - You can also capture by landing on their square
                                            *)
                                                let mutable d = distance move
                                                let direction =
                                                   // Can only move forward
                                                    match player with
                                                    | White -> 1
                                                    | Black -> -1
                                                // I need this to determine if it can catch players
                                                let newSquare = board.[move.To]

                                                match (dCoords, newSquare, pawnState) with
                                                // On first turn, can move 2 spaces
                                                | ((x, y), None, NotMoved) when x = 0 && y = (2 * direction) -> Ok move 
                                                // On first turn and any turn, can move 1 space
                                                | ((x, y), None, _) when x = 0 && y = (1 * direction) -> Ok move
                                                | ((x, y), Some _, _) when x = 1 && y = (1 * direction) -> Ok move
                                                | _ -> Error "Error: This is not a valid move"
                                            
                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )

                                            let straight move =
                                             // Rook: May only move straight - forward backward left and right

                                                match dCoords with
                                                | (x, y) when x > 0 && y = 0 -> Ok move
                                                | (x, y) when x = 0 && y > 0 -> Ok move
                                                | _ -> Error "Error: This is not a valid move" 
                                                
                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )

                                            let diagonal move =
                                            // Bishop: may move diagonally
                                                match dCoords with
                                                | (x, y) when x > 0 && y = x -> Ok move
                                                | _ -> Error "Error: This is not a valid move" 

                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )



                                            let lshape move =
                                            // Knight: move in L shape & won't collide

                                                match dCoords with
                                                | (2, 1) | (1, 2) -> Ok move
                                                | _ -> Error "Error: This is not a valid move" 

                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )


                                            let queenMove move =
                                            // Queen: can move straight or diagonal
                                                match dCoords with
                                                | (x, y) when x > 0 && y = 0 -> Ok move
                                                | (x, y) when x = 0 && y > 0 -> Ok move
                                                | (x, y) when x > 0 && y = x -> Ok move
                                                | _ -> Error "Error: This is not a valid move" 

                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )


                                            let kingMove move =
                                            (*
                                                King    :
                                                    - Restricted to one move per turn
                                                    - Can move in any direction
                                                        - Straight / Diagonal
                                                    - May capture in any direction that's within legal move range
                                            *)
                                                match dCoords with
                                                | (x, y) when x = 1 && y = 0 -> Ok move
                                                | (x, y) when x = 0 && y = 1 -> Ok move
                                                | (x, y) when x = 1 && y = 1 -> Ok move
                                                | _ -> Error "Error: This is not a valid move" 

                                                if (this.Result = Error) then
                                                    printfn "%s" Error
                                                    moves ( )

                                            // sending to correct function of movement
                                            match move.ThisPiece.Rank with
                                            | King          -> kingMove
                                            | Queen         -> queenMove
                                            | Bishop        -> diagonal
                                            | Knight        -> lshape
                                            | Rook          -> straight
                                            | Pawn NotMoved -> pawnMove NotMoved
                                            | Pawn Moved    -> pawnMove Moved
                                        validatePieceandRank board player

                                    if (myPiece = Some piece) then
                                        match piece with
                                        | { Player = _ ; Rank = Pawn NotMoved } -> 
                                            myPiece <- Some { Player = piece.Player ; Rank = Pawn Moved }
                                        | { Player = _ ; Rank = _ } -> 
                                            myPiece <- Some { Player = piece.Player ; Rank = piece.Rank }
                                    else
                                        printf "I am not even sure how you got here."

                                    match board.[toKey] with
                                    | Some piece -> 
                                        printfn "%A %A got capped!\n\r" piece.Player piece.Rank
                                        let mutable player =
                                            match piece.Player with
                                            | Black -> White
                                            | White -> Black

                                        board <- board.Add(toKey, None).Add(key, None).Add(toKey, myPiece)
                                    
                                    
                                        state <- { Board = board ; Status = InProgress; ThisPlayer = player;Message = state.Message; }
                                    | None ->
                                        let mutable player =
                                            match piece.Player with
                                            | Black -> White
                                            | White -> Black

                                        board <- board.Add(key, None).Add(toKey, myPiece)

                                        state <- { Board = board ; Status = InProgress; ThisPlayer = player;Message = sprintf "%A to move." player; }

                                    chessBoard board

                    
                                queryBoard( )


                            | "N" | "n" -> queryBoard( )
                            | _ -> 
                                printfn "Fucking Ridiculous! Can't you read? Select y or n: \n\r"
                                moves ( )
                        else 
                            printfn "That's not your piece!"
                            queryBoard ( )
                    | None -> 
                        printf "This is vacant."
                        queryBoard( )

            moves ()
    
   // If it passes all checks, continue with the query
   
    System.Console.ReadKey() |> ignore
        

    queryBoard ( ) 
    0