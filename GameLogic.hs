{-
    Assignment 3 - Implementation

    Name: Chris Kinzel
    ID: 10160447
-}

module GameLogic where

import Data.Maybe
import Data.List
import Control.Parallel.Strategies
--import Debug.Trace

-- TYPE DEF'S

data RoseTree a = Leaf a | RS a [RoseTree a ] deriving (Show)

data Piece = Yellow | Red deriving (Show,Eq)

type Column = [Piece]
type Board = [Column]

data BoardState = BS
    {
        theBoard :: Board,
        lastMove :: Piece,
        numColumns :: Int,
        numRows :: Int,
        numToConnect :: Int
    } deriving (Show)


{-
    This function inserts the current player's piece at
    the specified column index in the board if doing so
    is a legal move, otherwise it does Nothing (;
-}
makeMove :: BoardState -> Int -> Maybe BoardState
makeMove bs ci = if (checkLegal bs ci) then Just (addPiece bs ci (otherPlayer (lastMove bs)))
                 else Nothing


{-
    This function simply returns the opposite piece color using case
-}
otherPlayer :: Piece -> Piece
otherPlayer color = case color of
                        Yellow -> Red
                        Red -> Yellow


{-
    This function checks if a given move in the game is
    legal, that is, within the bounds of the board and
    in a column that is not full
-}
checkLegal :: BoardState -> Int -> Bool
checkLegal bs ci = ci > 0 && ci <= (numColumns bs) && columnHasSpace bs ci


{-
    This function determines whether or not a column is
    full
-}
columnHasSpace :: BoardState -> Int -> Bool
columnHasSpace bs ci = (length ((theBoard bs) !! (ci-1))) < (numRows bs)


{-
    This function drops a piece of a given color at the
    specified column index and returns the new board state
-}
addPiece :: BoardState -> Int -> Piece -> BoardState
addPiece bs ci piece = bs {
                            theBoard = take (ci-1) (theBoard bs) ++ [placeInColumn bs ci piece] ++ drop ci (theBoard bs),
                            lastMove = piece
                        }

{-
    This function computes the column resulting
    after a piece drop of a given color
-}
placeInColumn :: BoardState -> Int -> Piece -> Column
placeInColumn bs ci piece = ((theBoard bs) !! (ci-1)) ++ [piece]


{-
    This function determines if there is a win in
    the game and if so returns the winning player's
    color otherwise it returns Nothing
-}
checkWin :: BoardState -> Maybe Piece
checkWin bs = if (hasWinner (lastMove bs) bs)
                then Just (lastMove bs)
                else Nothing

{-
    This function checks if a given player has won
    the game
-}
hasWinner :: Piece -> BoardState -> Bool
hasWinner player bs = checkWinner (numToConnect bs) player (columns bs) || checkWinner (numToConnect bs) player (rows bs) || checkWinner (numToConnect bs) player (diagonalsForward bs) || checkWinner (numToConnect bs) player (diagonalsBackward bs)

{-
    This function determines whether or not a
    given player has connected n on the board
-}
checkWinner :: Int -> Piece -> [[Maybe Piece]] -> Bool
checkWinner n piece seqs = any (\seq -> case piece of
                                            Red -> (countRed seq) >= n
                                            Yellow -> (countYellow seq) >= n) seqs

{-
    This function returns the maximum consequetive
    number of red pieces in a given list of pieces
-}
countRed :: [Maybe Piece] -> Int
countRed seq = snd (foldr (\p (c,cmax) -> case p of
                                            Just Red -> (c+1,max (c+1) cmax)
                                            _ -> (0,cmax)) (0,0) seq)

{-
    This function returns the maximum consequetive
    number of yellow pieces in a given list of pieces
-}
countYellow :: [Maybe Piece] -> Int
countYellow seq = snd (foldr (\p (c,cmax) -> case p of
                                                Just Yellow -> (c+1,max (c+1) cmax)
                                                _ -> (0,cmax)) (0,0) seq)

{-
    This function pads a given list of pieces of
    size n with Nothing's to represent empty
    slots
-}
padN :: Int -> [Maybe Piece] -> [Maybe Piece]
padN n col = col ++ replicate (n - (length col)) Nothing


{-
    This function returns the columns of the board
    padded with Nothing's to represent emtpy slots
-}
columns :: BoardState -> [[Maybe Piece]]
columns bs = map (\col -> padN (numRows bs) col) ((map (map Just) (theBoard bs)))


{-
    This function returns the rows of the board
    padded with Nothing's to represent emtpy slots
-}
rows :: BoardState -> [[Maybe Piece]]
rows bs = map (\ri -> map (!! ri) (columns bs)) (reverse [0..((numRows bs)-1)])

{- Reference: Johnathan Gallagher D2L Assignment 3 Solution code -}

{-
Precondition: In order for this function to work properly,
the list input must be a perfectly rectangular list.  In other words
it must be a list of lists where each element has the same length.

The idea of this function:
m11 m12 m13 m14
m21 m22 m23 m24
m31 m32 m33 m34
m41 m42 m43 m44

If we extract the first row:
m11 m12 m13 m14
We take m11 and put it in completely processed.
We then take
m12 m13 m14
and pair it with
m21 m22 m23 with m24 left over.
We then get
[[m21,m12],[m22,m13],[m23,m14],[m24]]
We then take the head and put it onto completelyProcessed so that completely processed is
[[m11],[m21,m12]]
One can see this inductively keeps going!!
splitDiagonals completelyProcessed currentGen unProcessed
-}
splitDiagonals :: [[a]] -> [[a]] -> [[a]] -> [[a]]
splitDiagonals completelyProcessed finale [] = completelyProcessed ++ finale
splitDiagonals completelyProcessed (toBeEnqueued:currentGen) (nextRow:unProcessed) = splitDiagonals (completelyProcessed ++ [toBeEnqueued]) (pairShuffled currentGen nextRow) unProcessed

{-
Precondition: length of list1 is is length of list2 - 1
-}
pairShuffled :: [[a]] -> [a] -> [[a]]
pairShuffled [] newElement = [newElement]
pairShuffled (row:rows) (x:xs) = (x:row) : pairShuffled rows xs


{-
To use splitDiagonals to do diagonalsForward
We first take the first row, and

-}
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward = pureDiagonals . rows

{-
Precondition: pureDiagonals requires a non-empty list.
-}
pureDiagonals :: [[a]] -> [[a]]
pureDiagonals (firstRow:restOfTheRows) = splitDiagonals [] (map (\x -> [x]) firstRow) restOfTheRows

{-
If we rotate the board to the right and collect the diagonals, we will have collected all
the diagonals, but each diagonal will have been collected in the reverse order.  Hence
we map reverse onto each diagonal.

E.g.

if we have
m11 m12
m21 m22
and we rotate right we have
m21 m11
m22 m12
if we collect the diagonals here, we have
[[m21],[m22,m11],[m12]]
so that we have travelled down each diagonal, but we travelled the diagonals bottom right to top left
when we wanted top left to bottom right.  Thus we simply reverse each diagonal to get
[[m21],[m11,m22],[m12]]
which is correct.
-}
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward = map reverse . pureDiagonals . rotateRight . rows

rotateRight :: [[a]] -> [[a]]
rotateRight = (map reverse) . transpose




{-foldRose :: (a -> b) -> (a -> [b] -> b) -> RoseTree a -> b
foldRose l r (Leaf x) = l x
foldRose l r (RS x rss) = r x (map (foldRose l r ) rss)-}

foldRose :: (a -> b) -> (a -> [b] -> b) -> RoseTree a -> b
foldRose l r (Leaf x) = l x
foldRose l r (RS x rss) = r x (parMap rpar (foldRose l r ) rss)

suggestMove :: BoardState -> Maybe Int
suggestMove bs = if (checkFull bs) then Nothing else Just (pickBest (minimax (otherPlayer (lastMove bs)) (makeTree 5 bs)))

suggestMove2 :: BoardState -> (Maybe Int, Int)
suggestMove2 bs = if (checkFull bs) then (Nothing, 0) else let scoreTree = minimax (otherPlayer (lastMove bs)) (makeTree 5 bs) in (Just (pickBest scoreTree), movesToWin scoreTree)

movesToWin :: RoseTree (Int, Int) -> Int
movesToWin (Leaf _) = 0
movesToWin (RS _ children) = if (any (\subTree -> (snd (getScoreNode subTree)) > 90000000) children) then (maximum (map movesToWin children))+1 else 0

pickBest :: RoseTree (Int, Int) -> Int
pickBest (RS _ children) = fst (foldr (\subTree (bestCol, bestVal) -> if (snd (getScoreNode subTree)) >= bestVal then (getScoreNode subTree) else (bestCol, bestVal) ) (0, -1000000000) children)

minimax :: Piece -> RoseTree (Int, BoardState) -> RoseTree (Int, Int)
minimax player gameTree = foldRose (\(ci, bsi) -> Leaf (ci, (scoreBoard bsi player) - (scoreBoard bsi (otherPlayer player)) ) ) (\(ci, bsi) children -> if player == (lastMove bsi) then RS (ci, minimum (map (snd . getScoreNode) children) ) children
                                                                                                                                                                                    else RS (ci, maximum (map (snd . getScoreNode) children) ) children ) gameTree

getScoreNode :: RoseTree (Int, Int) -> (Int, Int)
getScoreNode (Leaf node) = node
getScoreNode (RS node rss) = node

checkFull :: BoardState -> Bool
checkFull bs = all (\col -> (length col) >= (numRows bs)) (theBoard bs)

nextMoves :: BoardState -> [(Int, BoardState)]
nextMoves bs = filterIllegal (map (\ci -> (ci, makeMove bs ci)) [1..(numColumns bs)])

filterIllegal :: [(Int, Maybe BoardState)] -> [(Int, BoardState)]
filterIllegal moves = map (\(ci, bsi) -> (ci, fromJust bsi)) (filter (\(ci, bsi) -> isJust bsi) moves)

deepenOneLevel :: RoseTree (Int, BoardState) -> RoseTree (Int, BoardState)
deepenOneLevel gameTree = foldRose (\(ci, bsi) -> if (not (hasWinner Red bsi) && not (hasWinner Yellow bsi) && not (checkFull bsi)) then RS (ci, bsi) (map Leaf (nextMoves bsi))
                                                                                                                                    else Leaf (ci, bsi) ) RS gameTree

deepenNLevels :: Int -> RoseTree (Int, BoardState) -> RoseTree (Int, BoardState)
deepenNLevels 0 gameTree = gameTree
deepenNLevels n gameTree = deepenOneLevel (deepenNLevels (n-1) gameTree)

makeTree :: Int -> BoardState -> RoseTree (Int, BoardState)
makeTree n bs = deepenNLevels n (Leaf (0, bs))

scoreBoard :: BoardState -> Piece -> Int
scoreBoard bs player = (scoreColumns bs player) + (scoreRows bs player) + (scoreDiagonals bs player) + (if (hasWinner player bs) then 100000000 else 0) - (if (hasWinner (otherPlayer player) bs) then 100000000 else 0)

scoreColumns :: BoardState -> Piece -> Int
scoreColumns bs player = scoreLines bs player (columns bs)

scoreRows :: BoardState -> Piece -> Int
scoreRows bs player = scoreLines bs player (rows bs)

scoreDiagonals :: BoardState -> Piece -> Int
scoreDiagonals bs player = (scoreLines bs player (diagonalsForward bs)) + (scoreLines bs player (diagonalsBackward bs))

scoreLines :: BoardState -> Piece -> [[Maybe Piece]] -> Int
scoreLines bs player lines = foldr (\line score -> case player of
                                                    Red -> score + (countRedTwoToN line (numToConnect bs)) + (countRedTwoToN (reverse line) (numToConnect bs))
                                                    Yellow -> score + (countYellowTwoToN line (numToConnect bs)) + (countYellowTwoToN (reverse line) (numToConnect bs)) ) 0 lines


countRedTwoToN :: [Maybe Piece] -> Int -> Int
countRedTwoToN seq 2 = countRedNs seq 2
countRedTwoToN seq n = (countRedNs seq n) + (countRedTwoToN seq (n-1))

countYellowTwoToN :: [Maybe Piece] -> Int -> Int
countYellowTwoToN seq 2 = countYellowNs seq 2
countYellowTwoToN seq n = (countYellowNs seq n) + (countYellowTwoToN seq (n-1))


countRedNs :: [Maybe Piece] -> Int -> Int
countRedNs seq n = fst (fst (foldr (\p ((count, c), (seenNothing, justScored)) -> case p of
                                                                                    Just Red -> updateCount ((count, c+1), (seenNothing, justScored)) n
                                                                                    Nothing  -> updateSeenNothing ((count, c), (seenNothing, justScored)) n
                                                                                    Just Yellow -> ((count, 0), (False, False)) ) ((0, 0), (False, False)) seq))

countYellowNs :: [Maybe Piece] -> Int -> Int
countYellowNs seq n = fst (fst (foldr (\p ((count, c), (seenNothing, justScored)) -> case p of
                                                                                    Just Yellow -> updateCount ((count, c+1), (seenNothing, justScored)) n
                                                                                    Nothing  -> updateSeenNothing ((count, c), (seenNothing, justScored)) n
                                                                                    Just Red -> ((count, 0), (False, False)) ) ((0, 0), (False, False)) seq))

updateCount :: ((Int, Int), (Bool, Bool)) -> Int -> ((Int, Int), (Bool, Bool))
updateCount ((count, c), (seenNothing, justScored)) n = if (c == n || justScored) && seenNothing then ((count+(10^n), 0), (False, not justScored)) else ((count, c), (seenNothing, False))

updateSeenNothing :: ((Int, Int), (Bool, Bool)) -> Int -> ((Int, Int), (Bool, Bool))
updateSeenNothing ((count, c), (True, justScored)) n  = ((count, 0), (False, justScored))
updateSeenNothing ((count, c), (False, justScored)) n = updateCount ((count, c), (True, justScored)) n

