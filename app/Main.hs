module Main where

import System.IO
import System.Random
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

data Direction = DirUp | DirDown | DirLeft | DirRight | DirUnknown deriving (Eq, Show)

countZeros :: [[Int]] -> Int
countZeros lists = sum [1 | sublist <- lists, elem <- sublist, elem == 0]

iToPos :: Int -> Int -> [Int]
iToPos pos numCols = [pos `div` numCols, pos `mod` numCols]

countValues :: [Int] -> Int -> Int
countValues [] value = 0
countValues list value =
    if head list == value then 1 + countValues (tail list) value
    else countValues (tail list) value

indexNthValue :: [Int] -> Int -> Int -> Int -> Int
indexNthValue list value n pos
  | n == 0 = pos-1
  | list!!pos == value = indexNthValue list value (n-1) (pos+1)
  | otherwise = indexNthValue list value n (pos+1)

indexNthValue2D :: [[Int]] -> Int -> Int -> [Int] -> [Int]
indexNthValue2D list value 0 pos = pos
indexNthValue2D list value n pos =
  let count = countValues (list !! head pos) value
  in
    if n > count
      then indexNthValue2D list value (n - count) (head pos + 1 : tail pos)
      else [head pos, indexNthValue (list !! head pos) value n 0]

incrementPos :: [Int] -> Int -> [Int] -> [Int]
incrementPos [y, x] inc [rows, cols]
  | x + inc >= cols = [y + 1, x + inc - cols]
  | otherwise = [y, x + inc]

isValidPos :: [[Int]] -> [Int] -> Bool
isValidPos board [row, col] =
    row >= 0 && row < length board && col >= 0 && col < length (head board)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal xs =
    let (before, _:after) = splitAt n xs
    in before ++ [newVal] ++ after

replaceAt2D :: Int -> Int -> a -> [[a]] -> [[a]]
replaceAt2D outerIdx innerIdx newVal xss =
    let (before, target:after) = splitAt outerIdx xss
        newTarget = replaceAt innerIdx newVal target
    in before ++ [newTarget] ++ after

move :: IORef [[Int]] -> Direction -> IO ()
move boardRef dir = do
    board <- readIORef boardRef
    let newBoard = reverseRotateMatrix (moveTiles (combineTiles (moveTiles (rotateMatrix board dir) [0,0]) [1,0]) [0,0]) dir
    num <- randomRIO (1,2)
    postotal <- randomRIO (0, countZeros newBoard)
    let pos = indexNthValue2D newBoard 0 postotal [0,0]
    let newBoard' = if dir /= DirUnknown && newBoard /= board then replaceAt2D (head pos) (pos!!1) (num*2) newBoard
                    else newBoard
    
    -- Write the updated board back to the IORef
    writeIORef boardRef newBoard'

combineTiles :: [[Int]] -> [Int] -> [[Int]]
combineTiles board startPos
  | not (isValidPos board startPos) = board
  | board!!head startPos!!(startPos!!1) == board!!(head startPos-1)!!(startPos!!1) = combineTiles
        (mergeTiles board [head startPos-1, startPos!!1] startPos)
        (incrementPos startPos 1 [length board+1, length (head board)])
  | otherwise = combineTiles board (incrementPos startPos 1 [length board+1, length (head board)])

mergeTiles :: [[Int]] -> [Int] -> [Int] -> [[Int]]
mergeTiles board destTile delTile = replaceAt2D (head delTile) (delTile!!1) 0 (replaceAt2D (head destTile) (destTile!!1) (board!!head destTile!!(destTile!!1)*2) board)

moveTiles :: [[Int]] -> [Int] -> [[Int]]
moveTiles board startPos =
    if not (isValidPos board startPos) then board
    else moveTiles (moveTile board startPos) (incrementPos startPos 1 [length board+1, length (head board)])

moveTile :: [[Int]] -> [Int] -> [[Int]]
moveTile board pos =
    if checkBox board pos then moveTile (replaceAt2D (head pos) (pos!!1) 0 (replaceAt2D (head pos-1) (pos!!1) (board!!head pos!!(pos!!1)) board)) [head pos-1, pos!!1]
    else board

checkBox :: [[Int]] -> [Int] -> Bool
checkBox board pos
  | head pos-1 <0 || board!!head pos!!(pos!!1) == 0 = False
  | board!!(head pos-1)!!(pos!!1) == 0 = True
  | otherwise = False

rotateMatrix :: [[Int]] -> Direction -> [[Int]]
rotateMatrix matrix dir =
    case dir of
        DirUp -> matrix
        DirDown -> reverse (map reverse matrix)
        DirRight -> reverse (transposeMatrix matrix)
        DirLeft -> map reverse (transposeMatrix matrix)
        DirUnknown -> matrix

reverseRotateMatrix :: [[Int]] -> Direction -> [[Int]]
reverseRotateMatrix matrix dir =
    case dir of
        DirUp -> matrix
        DirDown -> reverse (map reverse matrix)
        DirLeft -> reverse (transposeMatrix matrix)
        DirRight -> map reverse (transposeMatrix matrix)
        DirUnknown -> matrix

transposeMatrix :: [[a]] -> [[a]]
transposeMatrix ([]:_) = []
transposeMatrix x = map head x : transposeMatrix (map tail x)

printBoard :: [[Int]] -> IO ()
printBoard [] = return ()
printBoard (row:rows) = do
    print row
    printBoard rows

drawBoard :: [[Int]] -> GLfloat -> IO ()
drawBoard board tileSize = drawTiles board [-0.625, -0.625] tileSize 0.1 0

drawTiles :: [[Int]] -> [GLfloat] -> GLfloat -> GLfloat -> Int -> IO ()
drawTiles tiles startPos tileSize offset i = do
    if i >= length (concat tiles) then return ()
    else do
        let [row, col] = iToPos i (length (head tiles))
            xPos = head startPos + offset + (fromIntegral col * (offset + tileSize))
            yPos = startPos!!1 + offset + (fromIntegral (length tiles - 1 - row) * (offset + tileSize))
        drawRectangle [xPos, yPos] tileSize tileSize (getColour (concat tiles!!i))
        drawTiles tiles startPos tileSize offset (i+1)

getColour :: Int -> Color4 GLfloat
getColour num =
    case num of
        0 -> Color4 204 192 179 255
        2 -> Color4 238 228 218 255
        4 -> Color4 237 224 200 255
        8 -> Color4 242 177 121 255
        16 -> Color4 245 149 99 255
        32 -> Color4 246 124 95 255
        64 -> Color4 246 94 59 255
        128 -> Color4 237 207 114 255
        256 -> Color4 237 204 97 255
        512 -> Color4 237 200 80 255
        1024 -> Color4 237 197 63 255
        2048 -> Color4 237 194 46 255

display :: IORef [[Int]] -> DisplayCallback
display boardRef = do
    clear [ ColorBuffer ]

    board <- readIORef boardRef

    drawBoard board 0.25
    flush

color4Norm :: Color4 GLfloat -> Color4 GLfloat
color4Norm (Color4 r g b a) = Color4 (r / 255) (g / 255) (b / 255) (a / 255)

drawRectangle :: [GLfloat] -> GLfloat -> GLfloat -> Color4 GLfloat -> IO ()
drawRectangle pos width height color = do
  let normalizedColor = color4Norm color
  currentColor $= normalizedColor
  renderPrimitive Quads $ do
    vertex $ Vertex3 ((-width / 2) + head pos) ((-height / 2) + pos!!1) 0
    vertex $ Vertex3 (width / 2 + head pos) ((-height / 2) + pos!!1) 0
    vertex $ Vertex3 (width / 2 + head pos) (height / 2 + pos!!1) 0
    vertex $ Vertex3 ((-width / 2) + head pos) (height / 2 + pos!!1) 0

handleKeys :: IORef [[Int]] -> KeyboardMouseCallback
handleKeys boardRef (Char 'w') Down _ _ = moveAndRedraw boardRef DirUp
handleKeys boardRef (Char 's') Down _ _ = moveAndRedraw boardRef DirDown
handleKeys boardRef (Char 'a') Down _ _ = moveAndRedraw boardRef DirLeft
handleKeys boardRef (Char 'd') Down _ _ = moveAndRedraw boardRef DirRight
handleKeys _ _ _ _ _ = return ()

moveAndRedraw :: IORef [[Int]] -> Direction -> IO ()
moveAndRedraw boardRef dir = do
    move boardRef dir
    postRedisplay Nothing

main :: IO ()
main = do
    let initialBoard = [[2,0,0,0],
                        [0,0,0,0],
                        [0,0,2,0],
                        [0,0,0,0]]

    -- Create an IORef to hold the board state
    boardRef <- newIORef initialBoard
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "2048"
    displayCallback $= display boardRef
    keyboardMouseCallback $= Just (handleKeys boardRef)
    mainLoop