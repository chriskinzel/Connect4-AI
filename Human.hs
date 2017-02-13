--module Assignment3IO where

import GameLogic
import Data.Maybe
import Data.List (intersperse)

main :: IO ()
main = do
    putStrLn "enter number of rows"
    rowsString <- getLine
    let rows = read rowsString :: Int 
    putStrLn "enter number of columns"
    columnsString <- getLine 
    let columns = read columnsString :: Int
    putStrLn "enter number to connect"
    nString <- getLine 
    let n = read nString :: Int 
    putStrLn "Yellow moves first"
    playGame (initializeBoard rows columns n Red)

initializeBoard :: Int -> Int -> Int -> Piece -> BoardState 
initializeBoard rows cols numConn lastPlayer = 
    BS {
        theBoard = take cols (repeat []),
        numRows = rows,
        numColumns = cols,
        numToConnect = numConn,
        lastMove = lastPlayer
    }

playGame :: BoardState -> IO ()
playGame bs =
    case checkBoardFull bs of
        True -> putStrLn "game is a draw"
        False -> playGameAvail bs

{-
This is too close to one of the functions you must implement, so
I left it undefined, for now.

This checks to see if all columns are full.
-}
checkBoardFull :: BoardState -> Bool
checkBoardFull bs = all (\col -> (length col) >= (numRows bs)) (theBoard bs)

playGameAvail :: BoardState -> IO ()
playGameAvail bs = if (lastMove bs) == Red
    then do
         putStrLn $ "Player's Turn!"
         --putStrLn $ "Suggested Move: " ++ show (fromJust (suggestMove bs))
         putStrLn $ "enter column number of next move: 1 - " ++ show (numColumns bs)
         nextMove <- getLine
         case makeMove bs (read nextMove :: Int) of
            Nothing -> do
                putStrLn "invalid move, try again"
                playGameAvail bs
            Just newBS -> do
                printBoard newBS
                case checkWin newBS of
                    Nothing -> playGame newBS
                    Just Red -> putStrLn "\nRed wins!!"
                    Just Yellow -> putStrLn "\nYellow wins!!"
    else do
        putStrLn $ "AI's Turn!\nThinking..."
        let turn = suggestMove2 bs
        let move = fromJust (fst turn)
        putStrLn $ show move
        case makeMove bs move of
            Nothing -> do
                putStrLn "invalid move, try again"
                playGameAvail bs
            Just newBS -> do
                printBoard newBS
                printMovesToWin (snd turn)
                case checkWin newBS of
                    Nothing ->  playGame newBS
                    Just Red -> putStrLn "\nRed wins!!"
                    Just Yellow -> putStrLn "\nYellow wins!!"

printMovesToWin :: Int -> IO ()
printMovesToWin movesLeft = if movesLeft > 1 then (putStrLn $ "AI win's in " ++ (show ((movesLeft-1) `div` 2) ) ++ " moves\n") else return ()

printBoard :: BoardState -> IO ()
printBoard bs = 
    let
        maybeRows = rows bs
        maybePieceToString :: Maybe Piece -> String 
        maybePieceToString p = 
            case p of
                Nothing ->     "        "
                Just Red ->    " Red    "
                Just Yellow -> " Yellow "
        rowsToStrings = map (map maybePieceToString) maybeRows 
        rowsVBars = map (intersperse "|" ) rowsToStrings
        rowsToLines = map (\r -> concat r ++ "\n") rowsVBars
        rowLength = length (head rowsToLines)
        longLine = (take rowLength (repeat '-')) ++ "\n" 
        rowsUnderlined = intersperse longLine rowsToLines
    in 
        putStrLn (concat rowsUnderlined)
