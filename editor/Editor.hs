import System.IO
import Data.List
import Data.Maybe
import Control.Exception
import System.Console.ANSI
import System.Console.Terminal.Size
import Models
import Update

main = do
    clearScreenAndSetCursor
    mainLoop initialState

-- Main Functions

mainLoop :: State -> IO ()
mainLoop s@(State MenuMode _ _ _ _ _ _) = mainLoopMenu s
mainLoop s@(State EditMode _ _ _ _ _ _) = mainLoopEdit s


-- Menu Mode

mainLoopMenu :: State -> IO ()
mainLoopMenu s@(State m f p x y l rules) = do
    resetFont
    setBold
    setCursorColor Yellow
    clearScreenAndSetCursor
    printMenu s
    c <- getChar
    case c of
        '1' -> openFileMenu s
        '2' -> saveFileMenu s
        '3' -> mainLoop $ parseProgram s
        '4' -> do
            putStrLn " Exiting!"
            resetFont
        _ -> mainLoopMenu s

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
openFileMenu s@(State m _ _ x y l rules) = do
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
            mainLoop (State m (lines content) p x y l rules)

saveFileMenu :: State -> IO ()
saveFileMenu s@(State m f p x y l rules) = do
    putStrLn ""
    putStr ("Save at path: ")
    path <- getLine
    eitherException <- (try $ writeFile path (intercalate "\n" f)) :: IO (Either IOException ())
    case eitherException of
        Left exception -> do
            putStrLn "File could not be saved!"
            saveFileMenu s
        Right _ -> do
            clearScreenAndSetCursor
            mainLoop (State m f path x y l rules)


-- Editor Mode

mainLoopEdit :: State -> IO ()
mainLoopEdit s@(State m f p x y l rules) = do
    resetFont
    clearScreenAndSetCursor
    printEditorMode s
    setCursorPosition (screenX s + 1) (screenY s)
    c <- getKey
    h <- screenHeight
    w <- screenWidth
    case c of
        "\ESC" -> mainLoop (State MenuMode f p x y l rules)
        "\ESC[A" -> mainLoop $ moveUp s h
        "\ESC[B" -> mainLoop $ moveDown s h
        "\ESC[C" -> mainLoop $ moveRight s w
        "\ESC[D" -> mainLoop $ moveLeft s w
        "\n" -> mainLoopEdit $ insertEnter s
        "\DEL" -> mainLoopEdit $ backspace s
        _ -> mainLoopEdit $ insertChar s c

printEditorMode :: State -> IO ()
printEditorMode (State m f p x y l rules) = do
    setBold
    setCursorColor Blue
    putStr ("File: " ++ p)
    resetFont
    h <- screenHeight
    printLines (State m dropped p x y l rules) (min (length dropped) h) highlighted (length rules)
        where
            dropped = (drop l f)
            highlighted = findInRule (fst $ rules !! x) y

printLines :: State -> Int -> Maybe (Either Token Name) -> Int -> IO ()
printLines (State _ [] _ _ _ _ _) _ _ _ = return ()
printLines (State m (f:fs) p x y l (r:rs)) n highlighted nrOfRules
    | n<= 0 = return ()
    | otherwise = do
        putStrLn ""
        drawRule r x y highlightedForThisRule
        w <- screenWidth
        printLines (State m fs p x y l rs) (n - 1 - (quot (length f) w)) highlighted nrOfRules
            where
                highlightedForThisRule = 
                    case highlighted of
                        Just (Right name) -> highlighted -- highlight this name everywhere
                        _ -> if (nrOfRules - n) == x then highlighted else Nothing -- highlight only the variables in the same rule (scope)

-- Source: https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
    where getKey' chars = do
            char <- getChar
            more <- hReady stdin
            (if more then getKey' else return) (char:chars)


-- Terminal Functions

clearScreenAndSetCursor :: IO ()
clearScreenAndSetCursor = do
    clearScreen
    setCursorPosition 0 0

setCursorColor :: Color -> IO ()
setCursorColor c = setSGR [SetColor Foreground Vivid c]

setBGColor :: Color -> IO ()
setBGColor c = setSGR [SetColor Background Vivid c]

setBold :: IO ()
setBold = setSGR [SetConsoleIntensity BoldIntensity]

resetFont :: IO ()
resetFont = setSGR [Reset]

resetAndColor :: Color -> IO ()
resetAndColor c = do
    resetFont
    setCursorColor c

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

-- Line draw Functions

drawRule :: (Rule, [Char]) -> Int -> Int -> Maybe (Either Token Name) -> IO ()
drawRule rule x y highlighted = do
    case rule of
        (Rul h b, rest) -> do
            drawHead h highlighted
            drawBody b highlighted
            resetAndColor White
            putStr "."
            drawFailure rest
        (RuleMissingDot h b, rest) -> do
            drawHead h highlighted
            drawBody b highlighted
            drawFailure rest
        (FailedRule r1, r2) -> do
            drawFailure (r1 ++ r2)
    

drawFailure :: String -> IO ()
drawFailure f = do
    resetAndColor Red
    setBold
    putStr f

drawHead :: Head -> Maybe (Either Token Name) -> IO ()
drawHead (Hea a) highlighted = drawAtom a highlighted

drawBody :: Body -> Maybe (Either Token Name) -> IO ()
drawBody (Bod []) highlighted = return ()
drawBody (Bod (g:gs)) highlighted = do
    resetAndColor Blue
    putStr ":"
    drawGoal g highlighted
    drawBody (Bod gs) highlighted

drawGoal :: Goal -> Maybe (Either Token Name) -> IO ()
drawGoal (AtomGoal a) highlighted = drawAtom a highlighted
drawGoal (EqGoal p1 p2) highlighted = do 
    drawPattern p1 highlighted
    resetAndColor Magenta
    putStr "="
    drawPattern p2 highlighted
drawGoal (ShellGoal p1 p2 p3 p4) highlighted = do
    resetAndColor Magenta
    putStr "$"
    drawPattern p1 highlighted; drawPattern p2 highlighted
    resetAndColor Magenta
    putStr "-"
    drawPattern p3 highlighted; drawPattern p4 highlighted

drawAtom :: Atom -> Maybe (Either Token Name) -> IO ()
drawAtom (FailedAtom f) highlighted = drawFailure f
drawAtom (Ato n p1 p2) highlighted = do
    drawName n highlighted
    drawPatterns p1 highlighted
    putStr "-"
    drawPatterns p2 highlighted
        where
            drawPatterns (p:ps) highlighted = do
                drawPattern p highlighted
                drawPatterns ps highlighted
            drawPatterns _ highlighted = return ()

drawName :: Name -> Maybe (Either Token Name) -> IO ()
drawName o@(Nam n) highlighted = do
    resetAndColor Cyan
    case highlighted of
        (Just (Right name)) -> if name == o then drawHighlightedName else drawNormal
        _ -> drawNormal
    where
        drawHighlightedName = do
            setBGColor Black
            putStr n
        drawNormal = do
            putStr n
    

drawPattern :: Pattern -> Maybe (Either Token Name) -> IO ()
drawPattern (FailedPattern f) highlighted = drawFailure f
drawPattern (Pat ts) highlighted = do
    resetAndColor Cyan
    putStr "("
    drawTokens ts highlighted
    resetAndColor Cyan
    putStr ")"
        where
            drawTokens (t:ts) highlighted = do
                drawToken t highlighted
                drawTokens ts highlighted
            drawTokens _ highlighted = return ()

drawToken :: Token -> Maybe (Either Token Name) -> IO ()
drawToken t@(Lit s c) highlighted = drawToken' White highlighted "" t s
drawToken t@(Plus s c) highlighted = drawToken' Blue highlighted "+" t s
drawToken t@(Star s c) highlighted = drawToken' Magenta highlighted "*" t s
drawToken Space highlighted = drawToken' White highlighted "" Space " "

drawToken' :: Color -> Maybe (Either Token Name) -> String -> Token -> String -> IO ()
drawToken' c highlighted p t s = do
    resetAndColor c
    case highlighted of
        (Just (Left tt)) -> if tt == t then drawHighlightedToken else drawNormal
        _ -> drawNormal
    where
        drawHighlightedToken = do
            setBGColor Black
            putStr (p ++ s)
        drawNormal = do
            putStr (p ++ s)


-- Functions for finding the name/variable for which the cursor is currently over

-- Given a rule and the cursor position in that rule, this function finds the name or variable for which the cursor is currently over
findInRule :: Rule -> Int -> Maybe (Either Token Name)
findInRule (Rul h b) cursorPos
    | isJust (fst resultHead) = fst resultHead
    | isJust (fst resultBody) = fst resultBody
    | otherwise = Nothing
        where
            resultHead = findInHead h cursorPos 0
            resultBody = findInBody b cursorPos (snd resultHead)
findInRule _ _ = Nothing

findInHead :: Head -> Int -> Int -> (Maybe (Either Token Name), Int)
findInHead (Hea h) cursorPos currentPos = findInAtom h cursorPos currentPos

findInBody :: Body -> Int -> Int -> (Maybe (Either Token Name), Int)
findInBody (Bod gs) cursorPos currentPos = findInGoals gs cursorPos currentPos

findInGoals :: [Goal] -> Int -> Int -> (Maybe (Either Token Name), Int)
findInGoals [] cursorPos currentPos = (Nothing,currentPos)
findInGoals (g:gs) cursorPos currentPos = (combineMaybe [foundInGoal, (fst rest)],snd rest)
    where
        (foundInGoal,nextPos) = findInGoal g cursorPos (currentPos + 1)  -- add ':'
        rest = findInGoals gs cursorPos nextPos

findInGoal :: Goal -> Int -> Int -> (Maybe (Either Token Name), Int)
findInGoal (AtomGoal g) cursorPos currentPos = findInAtom g cursorPos currentPos
findInGoal (EqGoal g1 g2) cursorPos currentPos = (combineMaybe [foundInGoal1,foundInGoal2],currentPos2)
    where
        (foundInGoal1,currentPos1) = findInPattern g1 cursorPos currentPos
        (foundInGoal2,currentPos2) = findInPattern g2 cursorPos (currentPos1 + 1)  -- add '='
findInGoal (ShellGoal g1 g2 g3 g4) cursorPos currentPos = (combineMaybe [foundInGoal1, foundInGoal2, foundInGoal3, foundInGoal4],currentPos4)
    where
        (foundInGoal1,currentPos1) = findInPattern g1 cursorPos (currentPos + 2) -- add  "$("
        (foundInGoal2,currentPos2) = findInPattern g2 cursorPos (currentPos1 + 2) -- add ")("
        (foundInGoal3,currentPos3) = findInPattern g3 cursorPos (currentPos2 + 3)  -- add ')-('
        (foundInGoal4,currentPos4) = findInPattern g4 cursorPos (currentPos3 + 2) -- add ")("

findInPatterns :: [Pattern] -> Int -> Int -> (Maybe (Either Token Name), Int)
findInPatterns [] cursorPos currentPos = (Nothing,currentPos)
findInPatterns (a:as) cursorPos currentPos = (combineMaybe [foundInPattern, (fst rest)],snd rest)
    where 
        (foundInPattern,nextPos) = findInPattern a cursorPos (currentPos + 1)  -- add '('
        rest = findInPatterns as cursorPos (nextPos + 1)  -- add ')'

findInPattern :: Pattern -> Int -> Int -> (Maybe (Either Token Name), Int)
findInPattern (FailedPattern f) cursorPos currentPos = (Nothing,length f)
findInPattern (Pat ts) cursorPos currentPos = findInTokens ts cursorPos currentPos
    
findInTokens :: [Token] -> Int -> Int -> (Maybe (Either Token Name), Int)
findInTokens [] cursorPos currentPos = (Nothing,currentPos)
findInTokens (t:ts) cursorPos currentPos = (combineMaybe [foundInToken, (fst rest)],snd rest)
    where
        (foundInToken,nextPos) = findInToken t cursorPos currentPos
        rest = findInTokens ts cursorPos nextPos

findInToken :: Token -> Int -> Int -> (Maybe (Either Token Name), Int)
findInToken Space cursorPos currentPos = (Nothing,currentPos + 1)
findInToken o@(Lit t _) cursorPos currentPos = _findIn 0 o t Left cursorPos currentPos
findInToken o@(Plus t _) cursorPos currentPos = _findIn 1 o t Left cursorPos currentPos
findInToken o@(Star t _) cursorPos currentPos = _findIn 1 o t Left cursorPos currentPos

findInAtom :: Atom -> Int -> Int -> (Maybe (Either Token Name), Int)
findInAtom (FailedAtom f) cursorPos currentPos = (Nothing,length f)
findInAtom (Ato s as1 as2) cursorPos currentPos = (combineMaybe [foundInName, foundInPattern1, foundInPattern2],currentPos3)
    where
        (foundInName,currentPos1) = findInName s cursorPos currentPos
        (foundInPattern1,currentPos2) = findInPatterns as1 cursorPos currentPos1
        (foundInPattern2,currentPos3) = findInPatterns as2 cursorPos (currentPos2 + 1)  -- add '-'

findInName :: Name -> Int -> Int -> (Maybe (Either Token Name), Int)
findInName o@(Nam n) cursorPos currentPos = _findIn 0 o n Right cursorPos currentPos

_findIn :: Int -> a -> String -> (a -> b) -> Int -> Int -> (Maybe b, Int)
_findIn a o t b cursorPos currentPos
    | (currentPos <= cursorPos) && (cursorPos < newPos) = (Just (b o),newPos)
    | otherwise = (Nothing,newPos)
    where newPos = currentPos + a + length t

combineMaybe :: [Maybe a] -> Maybe a
combineMaybe xs
    | resultLength == 0 = Nothing
    | resultLength == 1 = Just (head result)
    | otherwise = error "Cursor cannot be in multiple positions!"
        where
            resultLength = length result
            result = catMaybes xs