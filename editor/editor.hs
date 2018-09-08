import System.IO
import Control.Exception
import System.Console.ANSI
import System.Console.Terminal.Size

type FileBuffer = String
type Path = String
data EMode = EditMode | MenuMode deriving Eq
--newtype State = State (EMode, FileBuffer, Path, Int, Int)
data State = State { editorMode :: EMode, fileBuffer :: [String], filePath :: String, screenX :: Int, screenY :: Int, scrollLine :: Int }


-- Printing Functions

clearScreenAndSetCursor :: IO ()
clearScreenAndSetCursor = do
    clearScreen
    setCursorPosition 0 0

setCursorColor :: Color -> IO ()
setCursorColor c = setSGR [SetColor Foreground Vivid c]

setBold :: IO ()
setBold = setSGR [SetConsoleIntensity BoldIntensity]

resetFont :: IO ()
resetFont = setSGR [Reset]

screenHeight :: IO Int
screenHeight = do
    s <- size
    case s of
        Just w -> return (height w - 1)
        Nothing -> return 23

screenWidth :: IO Int
screenWidth = do
    s <- size
    case s of
        Just w -> return (width w - 1)
        Nothing -> return 79

-- Other Functions

ifBetween x a b y
    | x >= a && x <= b = x
    | otherwise = y

-- Menu Mode

printMenu :: State -> IO ()
printMenu s = do
    putStrLn "Choose one of the following options:"
    putStrLn "1) Open file"
    putStrLn "2) Save file"
    putStrLn "3) Edit current buffer"
    putStrLn "4) Exit without saving"
    putStrLn ""
    putStrLn ("Current file is: " ++ (filePath s))
    putStrLn ""

openFileMenu :: State -> IO ()
openFileMenu s@(State m _ _ x y l) = do
    putStrLn ""
    putStr "Enter file path: "
    p <- getLine
    fileOrException <- (try $ readFile p) :: IO (Either IOException String)
    case fileOrException of
         Left exception -> do
            putStrLn "File could not be read!"
            openFileMenu s
         Right content -> do
            clearScreenAndSetCursor
            mainLoop (State m (lines content) p x y l)

saveFileMenu :: State -> IO ()
saveFileMenu s@(State m f p x y l) = do
    putStrLn ""
    putStr ("Save at path: ")
    path <- getLine
    eitherException <- (try $ writeFile path (concat f)) :: IO (Either IOException ())
    case eitherException of
        Left exception -> do
            putStrLn "File could not be saved!"
            saveFileMenu s
        Right _ -> do
            clearScreenAndSetCursor
            mainLoop (State m f path x y l)

mainLoopMenu :: State -> IO ()
mainLoopMenu s@(State m f p x y l) = do
    resetFont
    setBold
    setCursorColor Yellow
    clearScreenAndSetCursor
    printMenu s
    c <- getChar
    case c of
        '1' -> openFileMenu s
        '2' -> saveFileMenu s
        '3' -> mainLoop (State EditMode f p x y l)
        '4' -> do
            putStrLn " Exiting!"
            resetFont
        _ -> mainLoopMenu s


-- Editor Mode
printLines :: State -> Int -> IO ()
printLines (State _ [] _ _ _ _) _ = return ()
printLines (State m (f:fs) p x y l) n 
    | n<= 0 = return ()
    | otherwise = do
        putStrLn ""
        putStr f
        w <- screenWidth
        printLines (State m fs p x y l) (n - 1 - (quot (length f) w))


printEditorMode :: State -> IO ()
printEditorMode (State m f p x y l) = do
    setBold
    setCursorColor Blue
    putStr ("File: " ++ p)
    resetFont
    h <- screenHeight
    printLines (State m dropped p x y l) (min (length dropped) h)
        where
            dropped = (drop l f)


insertChar :: State -> [Char] -> State
insertChar s@(State m f p x y l) c = State m new p x (y + 1) l
    where
        new = replaceAtIndex x [newLine] f
        newLine = insertAtIndex y c (f !! x)

insertEnter :: State -> State
insertEnter s@(State m f p x y l) = State m new p (x + 1) 0 l
    where
        new = replaceAtIndex x [xs,ys] f
        (xs,ys) = splitAt y (f !! x)

backspace :: State -> State
backspace s@(State m f p x y l)
    | y == 0 && x == 0 = s
    | y == 0 = State m newCombined p (x - 1) (length (f !! (x - 1))) l
    | otherwise = State m new p x (y - 1) l
        where
            newCombined = removeAtIndex x (replaceAtIndex (x - 1) [newLineCombined] f)
            newLineCombined = (f !! (x - 1)) ++ (f !! x)
            new = replaceAtIndex x [newLine] f
            newLine = removeAtIndex (y - 1) (f !! x)

mainLoopEdit :: State -> IO ()
mainLoopEdit s@(State m f p x y l) = do
    resetFont
    clearScreenAndSetCursor
    printEditorMode s
    setCursorPosition (screenX s + 1) (screenY s)
    c <- getKey
    h <- screenHeight
    w <- screenWidth
    case c of
        "\ESC" -> mainLoop (State MenuMode f p x y l)
        "\ESC[A" -> mainLoop (State EditMode f p (ifBetween (x - 1) 0 h x) y checkUp) -- up
            where
                checkUp
                    | x == 0 = l - 1
                    | otherwise = l
        "\ESC[B" -> mainLoop (State EditMode f p (ifBetween (x + 1) 0 (h - 1) x) y checkDown) -- down
            where
                checkDown
                    | x == h - 1 && (length f) - h > l = l + 1
                    | otherwise = l
        "\ESC[C" -> mainLoop (State EditMode f p x (ifBetween (y + 1) 0 w y) l) -- right
        "\ESC[D" -> mainLoop (State EditMode f p x (ifBetween (y - 1) 0 w y) l) -- left
        "\n" -> mainLoopEdit (insertEnter s)
        "\DEL" -> mainLoopEdit (backspace s)
        _ -> mainLoopEdit (insertChar s c)



-- Main Functions

mainLoop :: State -> IO ()
mainLoop s@(State MenuMode _ _ _ _ _) = mainLoopMenu s
mainLoop s@(State EditMode _ _ _ _ _) = mainLoopEdit s

main = do
    clearScreenAndSetCursor
    mainLoop (State MenuMode [""] "new" 0 0 0)


-- Source: https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
    where getKey' chars = do
            char <- getChar
            more <- hReady stdin
            (if more then getKey' else return) (char:chars)


-- Util Functions

replaceAtIndex :: Int -> [a] -> [a] -> [a]
replaceAtIndex index replacement list
    | length list <= index = list
    | otherwise = x ++ replacement ++ ys
        where (x,_:ys) = splitAt index list

insertAtIndex :: Int -> [a] -> [a] -> [a]
insertAtIndex index insert list = xs ++ insert ++ ys
    where (xs,ys) = splitAt index list

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex index list = replaceAtIndex index [] list