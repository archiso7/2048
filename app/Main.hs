module Main where

import System.IO
import System.Random
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import Data.IORef
import Graphics.Rendering.FTGL as FTGL

data Direction = DirUp | DirDown | DirLeft | DirRight | DirUnknown deriving (Eq, Show)
data Action = Pressed | Draw | DrawSelected
type Button = Action -> IORef [[Int]] -> FTGL.Font -> IO ()

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
  | pos > length list = error (show pos)
  | n == (-1) = pos - 1
  | list !! pos == value = indexNthValue list value (n - 1) (pos + 1)
  | otherwise = indexNthValue list value n (pos + 1)

indexNthValue2D :: [[Int]] -> Int -> Int -> [Int] -> [Int]
indexNthValue2D list value (-1) pos = pos
indexNthValue2D list value n pos =
  let count = (countValues (list !! head pos) value) - 1
  in if head pos >= length list then error (show (pos))
     else if n > count
          then indexNthValue2D list value (n - (count+1)) (head pos + 1 : tail pos)
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
    let zerosCount = countZeros newBoard
    postotal <- randomRIO (0, max 0 (zerosCount - 1))
    let pos = if zerosCount > 0 then indexNthValue2D newBoard 0 postotal [0,0] else [0,0]
    let newBoard' = if dir /= DirUnknown && newBoard /= board 
                    then replaceAt2D (head pos) (pos!!1) (num*2) newBoard
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
drawBoard board tileSize = do
    drawTiles board [-0.625, -0.625] tileSize 0.1 0

drawTiles :: [[Int]] -> [GLfloat] -> GLfloat -> GLfloat -> Int -> IO ()
drawTiles tiles startPos tileSize offset i = do
    if i >= length (concat tiles) then return ()
    else do
        let [row, col] = iToPos i (length (head tiles))
            xPos = head startPos + offset + (fromIntegral col * (offset + tileSize))
            yPos = startPos !! 1 + offset + (fromIntegral (length tiles - 1 - row) * (offset + tileSize))
            num = concat tiles !! i
            s = 0.005
        drawRectangle [xPos, yPos] tileSize tileSize (getColour num)
        if num /= 0
            then do 
                let fontSize = findFontSize (show num)
                font <- loadFont "COMICSANS.TTF" fontSize
                renderText font (show num) [s, s] (Color4 0.2 0.2 0.4 1) (Vector3 xPos (yPos-0.05) 0)
            else return ()
        drawTiles tiles startPos tileSize offset (i + 1)

findFontSize :: String -> Int
findFontSize text = 
    case (length text) of
        1 -> 30
        2 -> 30
        3 -> 27
        4 -> 20
        5 -> 16
        6 -> 13
        7 -> 12
        8 -> 10

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
        _ -> Color4 10 10 10 255

displayMenu :: IORef Int -> IORef [Button] -> String -> DisplayCallback
displayMenu selectedButtonRef buttonsRef menuName = do
    clear [ ColorBuffer ]
    selectedButton <- readIORef selectedButtonRef
    buttons <- readIORef buttonsRef
    font <- loadFont "COMICSANS.TTF" 24

    drawRectangle [0,0] 2 2 (Color4 30 0 72 255)
    drawButtons selectedButton buttons font
    renderText font menuName [0.01, 0.01] (Color4 0 0 0 1) (Vector3 0 0.25 0)

    flush

drawButtons :: Int -> [Button] -> FTGL.Font -> IO ()
drawButtons selectedButton [] font = return ()
drawButtons selectedButton buttons font = do
    let action = if selectedButton == 0 then DrawSelected else Draw
    tempIORef <- newIORef [[0]]
    (head buttons) action tempIORef font
    drawButtons (selectedButton-1) (tail buttons) font

displayBoard :: IORef [[Int]] -> DisplayCallback
displayBoard boardRef = do
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

buttonPress :: IORef Int -> IORef [Button] -> IORef [[Int]] -> IO ()
buttonPress selectedButtonRef buttonsRef boardRef = do
    selectedButton <- readIORef selectedButtonRef
    buttons <- readIORef buttonsRef
    font <- loadFont "COMICSANS.TTF" 24
    let button = buttons !! selectedButton
    button Pressed boardRef font

incrementIndex :: Int -> Int -> Int -> Int
incrementIndex length index inc = 
    let newIndex = index + inc in
    if newIndex < 0 then length + newIndex
    else if newIndex >= length then newIndex - length
    else newIndex

changeButton :: IORef Int -> IORef [Button] -> Int -> IO ()
changeButton selectedButtonRef buttonsRef dir = do
    selectedButton <- readIORef selectedButtonRef
    buttons <- readIORef buttonsRef
    writeIORef selectedButtonRef (incrementIndex (length buttons) selectedButton dir)

menuKeys :: IORef Int -> IORef [Button] -> IORef [[Int]] -> KeyboardMouseCallback
menuKeys selectedButtonRef buttonsRef boardRef (Char '\r') Down _ _ = do
    buttonPress selectedButtonRef buttonsRef boardRef
    postRedisplay Nothing
menuKeys selectedButtonRef buttonsRef boardRef (SpecialKey KeyUp) Down _ _ = do
    changeButton selectedButtonRef buttonsRef (-1)
    postRedisplay Nothing
menuKeys selectedButtonRef buttonsRef boardRef (SpecialKey KeyDown) Down _ _ = do
    changeButton selectedButtonRef buttonsRef 1
    postRedisplay Nothing
menuKeys _ _ _ _ _ _ _ = return ()

gameKeys :: IORef [[Int]] -> IORef Bool -> KeyboardMouseCallback
gameKeys boardRef hasWonRef (Char 'w') Down _ _ = moveAndRedraw boardRef DirUp hasWonRef
gameKeys boardRef hasWonRef (SpecialKey KeyUp) Down _ _ = moveAndRedraw boardRef DirUp hasWonRef
gameKeys boardRef hasWonRef (Char 's') Down _ _ = moveAndRedraw boardRef DirDown hasWonRef
gameKeys boardRef hasWonRef (SpecialKey KeyDown) Down _ _ = moveAndRedraw boardRef DirDown hasWonRef
gameKeys boardRef hasWonRef (Char 'a') Down _ _ = moveAndRedraw boardRef DirLeft hasWonRef
gameKeys boardRef hasWonRef (SpecialKey KeyLeft) Down _ _ = moveAndRedraw boardRef DirLeft hasWonRef
gameKeys boardRef hasWonRef (Char 'd') Down _ _ = moveAndRedraw boardRef DirRight hasWonRef
gameKeys boardRef hasWonRef (SpecialKey KeyRight) Down _ _ = moveAndRedraw boardRef DirRight hasWonRef
gameKeys _ _ _ _ _ _ = return ()

moveAndRedraw :: IORef [[Int]] -> Direction -> IORef Bool -> IO ()
moveAndRedraw boardRef dir hasWonRef = do
    move boardRef dir
    checkState boardRef hasWonRef
    postRedisplay Nothing

containsValue :: Eq a => a -> [[a]] -> Bool
containsValue val listOfLists = any (elem val) listOfLists

resume :: Button
resume action boardRef font = 
    case action of
        Pressed -> do
            hasWonRef <- newIORef True
            displayCallback $= displayBoard boardRef
            keyboardMouseCallback $= Just (gameKeys boardRef hasWonRef)
        Draw -> do 
            drawRectangle [0, -0.25] 0.5 0.2 (Color4 82 4 97 255)
            renderText font "resume" [0.005, 0.005] (Color4 0 0 0 1) (Vector3 0 ((-0.25)-(0.225/4)) 0)
        DrawSelected -> do
            drawRectangle [0, -0.25] 0.525 0.225 (Color4 228 0 116 255)
            drawRectangle [0, -0.25] 0.5 0.2 (Color4 82 4 97 255)
            renderText font "resume" [0.005, 0.005] (Color4 0 0 0 1) (Vector3 0 ((-0.25)-(0.225/4)) 0)

restart :: Button
restart action boardRef font = 
    case action of
        Pressed -> do
            let initialBoard = [[0,0,0,0],
                        [2,0,0,0],
                        [0,0,0,0],
                        [0,0,2,0]]
            writeIORef boardRef initialBoard
            hasWonRef <- newIORef False
            displayCallback $= displayBoard boardRef
            keyboardMouseCallback $= Just (gameKeys boardRef hasWonRef)
            postRedisplay Nothing
        Draw -> do 
            drawRectangle [0, -0.5] 0.5 0.2 (Color4 82 4 97 255)
            renderText font "restart" [0.005, 0.005] (Color4 0 0 0 1) (Vector3 0 ((-0.5)-(0.225/4)) 0)
        DrawSelected -> do
            drawRectangle [0, -0.5] 0.525 0.225 (Color4 228 0 116 255)
            drawRectangle [0, -0.5] 0.5 0.2 (Color4 82 4 97 255)
            renderText font "restart" [0.005, 0.005] (Color4 0 0 0 1) (Vector3 0 ((-0.5)-(0.225/4)) 0)

win :: IORef [[Int]] -> IO()
win boardRef = do
    selectedButtonRef <- newIORef 0
    let buttons = [resume, restart]
    buttonsRef <- newIORef buttons
    keyboardMouseCallback $= Just (menuKeys selectedButtonRef buttonsRef boardRef)
    displayCallback $= displayMenu selectedButtonRef buttonsRef "You Win!"
    postRedisplay Nothing

loss :: IORef [[Int]] -> IO()
loss boardRef = do
    selectedButtonRef <- newIORef 0
    let buttons = [restart]
    buttonsRef <- newIORef buttons
    keyboardMouseCallback $= Just (menuKeys selectedButtonRef buttonsRef boardRef)
    displayCallback $= displayMenu selectedButtonRef buttonsRef "You Lose"

checkState :: IORef [[Int]] -> IORef Bool -> IO ()
checkState boardRef hasWonRef = do
    board <- readIORef boardRef
    hasWon <- readIORef hasWonRef
    if (containsValue 2048 board) && (not hasWon) then win boardRef
    else if checkLoss board 0 then loss boardRef
    else return ()

checkLoss :: [[Int]] -> Int -> Bool
checkLoss board 4 = True
checkLoss board i = 
    let newBoard = reverseRotateMatrix (moveTiles (combineTiles (moveTiles (rotateMatrix board DirLeft) [0,0]) [1,0]) [0,0]) DirLeft
        in if (board /= newBoard) then False
            else checkLoss (rotateMatrix newBoard DirLeft) (i+1)

loadFont :: String -> Int -> IO FTGL.Font
loadFont fontPath fontSize = do
    font <- FTGL.createPolygonFont fontPath
    FTGL.setFontFaceSize font fontSize 72 -- Set font size
    return font

renderText :: FTGL.Font -> String -> [GLfloat] -> Color4 GLfloat -> Vector3 GLfloat -> IO ()
renderText font text s color pos = do
    currentColor $= color
    preservingMatrix $ do
        translate pos
        scale (head s) (s !! 1) (1 :: GLfloat)
        textWidth <- FTGL.getFontAdvance font text
        let xOffset = - realToFrac textWidth / 2 :: GLfloat
        translate $ Vector3 xOffset 0 0
        FTGL.renderFont font text FTGL.All

main :: IO ()
main = do
    let initialBoard = [[1024,1024,0,0],
                        [0,0,0,0],
                        [0,0,0,0],
                        [0,0,2,0]]

    -- Create an IORef to hold the board state
    boardRef <- newIORef initialBoard
    hasWonRef <- newIORef False
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "2048"
    displayCallback $= displayBoard boardRef
    keyboardMouseCallback $= Just (gameKeys boardRef hasWonRef)
    mainLoop