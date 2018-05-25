import System.IO
import System.Console.ANSI

type FileBuffer = String
type Path = String
data EMode = EditMode | MenuMode deriving Eq
--newtype State = State (EMode, FileBuffer, Path, Int, Int)
data State = State { editorMode :: EMode, fileBuffer :: [String], filePath :: String, screenX :: Int, screenY :: Int, scrollLine :: Int }

screenHeight = 24 - 1
screenWidth = 80 - 1

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
openFileMenu (State m _ _ x y l) = do
    putStrLn ""
    putStr "Enter file path: "
    p <- getLine
    f <- readFile p
    clearScreenAndSetCursor
    mainLoop (State m (lines f) p x y l)

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
        '2' -> mainLoop s
        '3' -> mainLoop (State EditMode f p x y l)
        '4' -> do
            putStrLn " Exiting!"
            resetFont
        _ -> mainLoopMenu s


-- Editor Mode
printLines (State _ _ _ _ _ _) 0 = return ()
printLines (State _ [] _ _ _ _) _ = return ()
printLines (State m (f:fs) p x y l) n = do
        putStrLn ""
        putStr f
        printLines (State m fs p x y l) (n - 1)


printEditorMode :: State -> IO ()
printEditorMode (State m f p x y l) = do
    setBold
    setCursorColor Blue
    putStr ("File: " ++ p)
    resetFont
    printLines (State m dropped p x y l) (min (length dropped) screenHeight)
        where
            dropped = (drop l f)


mainLoopEdit :: State -> IO ()
mainLoopEdit s@(State m f p x y l) = do
    resetFont
    clearScreenAndSetCursor
    printEditorMode s
    setCursorPosition (screenX s + 1) (screenY s)
    c <- getKey
    case c of
        "\ESC" -> mainLoop (State MenuMode f p x y l)
        "\ESC[A" -> mainLoop (State EditMode f p (ifBetween (x - 1) 0 screenHeight x) y checkUp) -- up
            where
                checkUp
                    | x == 0 = l - 1
                    | otherwise = l
        "\ESC[B" -> mainLoop (State EditMode f p (ifBetween (x + 1) 0 (screenHeight - 1) x) y checkDown) -- down
            where
                checkDown
                    | x == screenHeight - 1 && (length f) - screenHeight > l = l + 1
                    | otherwise = l
        "\ESC[C" -> mainLoop (State EditMode f p x (ifBetween (y + 1) 0 screenWidth y) l) -- right
        "\ESC[D" -> mainLoop (State EditMode f p x (ifBetween (y - 1) 0 screenWidth y) l) -- left
        _ -> mainLoopEdit s



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