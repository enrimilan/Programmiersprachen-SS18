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
        --putStr f
        drawRule r x y highlightedForThisRule -- todo wrapped lines
        w <- screenWidth
        printLines (State m fs p x y l rs) (n - 1 - (quot (length f) w)) highlighted nrOfRules
            where
                highlightedForThisRule = 
                    case highlighted of
                        Just (Right name ) -> highlighted
                        _ -> if (nrOfRules - n) == x then highlighted else Nothing

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
drawHead (Hea a) nt = drawAtom a nt

drawBody :: Body -> Maybe (Either Token Name) -> IO ()
drawBody (Bod []) nt = return ()
drawBody (Bod (g:gs)) nt = do
    resetAndColor Blue
    putStr ":"
    drawGoal g nt
    drawBody (Bod gs) nt

drawGoal :: Goal -> Maybe (Either Token Name) -> IO ()
drawGoal (AtomGoal a) nt = drawAtom a nt
drawGoal (EqGoal p1 p2) nt = do 
    drawPattern p1 nt
    resetAndColor Magenta
    putStr "="
    drawPattern p2 nt
drawGoal (ShellGoal p1 p2 p3 p4) nt = do
    resetAndColor Magenta
    putStr "$"
    drawPattern p1 nt; drawPattern p2 nt
    resetAndColor Magenta
    putStr "-"
    drawPattern p3 nt; drawPattern p4 nt

drawAtom :: Atom -> Maybe (Either Token Name) -> IO ()
drawAtom (FailedAtom f) nt = drawFailure f
drawAtom (Ato n p1 p2) nt = do
    drawName n nt
    drawPatterns p1 nt
    putStr "-"
    drawPatterns p2 nt
        where
            drawPatterns (p:ps) nt = do
                drawPattern p nt
                drawPatterns ps nt
            drawPatterns _ nt = return ()

drawName :: Name -> Maybe (Either Token Name) -> IO ()
drawName o@(Nam n) nt = do
    resetAndColor Cyan
    case nt of
        (Just (Right nn)) -> if nn == o then drawGreen else drawNormal
        _ -> drawNormal
    where
        drawGreen = do
            setBGColor Black
            putStr n
        drawNormal = do
            putStr n
    

drawPattern :: Pattern -> Maybe (Either Token Name) -> IO ()
drawPattern (FailedPattern f) nt = drawFailure f
drawPattern (Pat ts) nt = do
    resetAndColor Cyan
    putStr "("
    drawTokens ts nt
    resetAndColor Cyan
    putStr ")"
        where
            drawTokens (t:ts) nt = do
                drawToken t nt
                drawTokens ts nt
            drawTokens _ nt = return ()

drawToken :: Token -> Maybe (Either Token Name) -> IO ()
drawToken t@(Lit s c) nt = drawToken' White nt "" t s
drawToken t@(Plus s c) nt = drawToken' Blue nt "+" t s
drawToken t@(Star s c) nt = drawToken' Magenta nt "*" t s
drawToken Space nt = drawToken' White nt "" Space " "

drawToken' :: Color -> Maybe (Either Token Name) -> String -> Token -> String -> IO ()
drawToken' c nt p t s = do
    resetAndColor c
    case nt of
        (Just (Left tt)) -> if tt == t then drawGreen else drawNormal
        _ -> drawNormal
    where
        drawGreen = do
            setBGColor Black
            putStr (p ++ s)
        drawNormal = do
            putStr (p ++ s)


-- Coloring Functions

type CursorToElement = Int -> Int -> (Maybe (Either Token Name), Int)

combineMaybe :: [Maybe a] -> Maybe a
combineMaybe xs
    | resultLength == 0 = Nothing
    | resultLength == 1 = Just (head result)
    | otherwise = error "Cursor cannot be in multiple positions!"
        where
            resultLength = length result
            result = catMaybes xs

findInRule :: Rule -> Int -> (Maybe (Either Token Name))
findInRule (Rul h b) c
    | isJust (fst resultHead) = fst resultHead
    | isJust (fst resultBody) = fst resultBody
    | otherwise = Nothing
        where
            resultHead = findInHead h c 0
            resultBody = findInBody b c (snd resultHead)
findInRule _ _ = Nothing


findInBody :: Body -> CursorToElement
findInBody (Bod gs) c m = findInGoals gs c m

findInGoals :: [Goal] -> CursorToElement
findInGoals [] c m = (Nothing,m)
findInGoals (g:gs) c m = (combineMaybe [cg, (fst rest)],snd rest)
    where
        (cg,cm) = findInGoal g c (m + 1)  -- add ':'
        rest = findInGoals gs c cm

findInGoal :: Goal -> CursorToElement
findInGoal (AtomGoal g) c m = findInAtom g c m
findInGoal (EqGoal g1 g2) c m = (combineMaybe [cg1,cg2],cm2)
    where
        (cg1,cm1) = findInPattern g1 c m
        (cg2,cm2) = findInPattern g2 c (cm1 + 1)  -- add '='
findInGoal (ShellGoal g1 g2 g3 g4) c m = (combineMaybe [cg1, cg2, cg3, cg4],cm4)
    where
        (cg1,cm1) = findInPattern g1 c m
        (cg2,cm2) = findInPattern g2 c cm1
        (cg3,cm3) = findInPattern g3 c (cm2 + 1)  -- add '-'
        (cg4,cm4) = findInPattern g4 c cm3

findInAtom :: Atom -> CursorToElement
findInAtom (FailedAtom f) c m = (Nothing,length f)
findInAtom (Ato s as1 as2) c m = (combineMaybe [cs1, ca1, ca2],cm3)
    where
        (cs1,cm1) = findInName s c m
        (ca1,cm2) = findInPatterns as1 c cm1
        (ca2,cm3) = findInPatterns as2 c (cm2 + 1)  -- add '-'

findInPatterns :: [Pattern] -> CursorToElement
findInPatterns [] c m = (Nothing,m)
findInPatterns (a:as) c m = (combineMaybe [ca, (fst rest)],snd rest)
    where 
        (ca,cm) = findInPattern a c (m + 1)  -- add '('
        rest = findInPatterns as c (cm + 1)  -- add ')'

findInHead :: Head -> CursorToElement
findInHead (Hea h) c m = findInAtom h c m

findInPattern :: Pattern -> CursorToElement
findInPattern (FailedPattern f) c m = (Nothing,length f)
findInPattern (Pat ts) c m = findInTokens ts c m
    
findInTokens :: [Token] -> CursorToElement
findInTokens [] c m = (Nothing,m)
findInTokens (t:ts) c m = (combineMaybe [ct, (fst rest)],snd rest)
    where
        (ct,cm) = findInToken t c m
        rest = findInTokens ts c cm

findInToken :: Token -> CursorToElement
findInToken Space c m = (Nothing,m + 1)
findInToken o@(Lit t _) c m = _findIn 0 o t Left c m
findInToken o@(Plus t _) c m = _findIn 1 o t Left c m
findInToken o@(Star t _) c m = _findIn 1 o t Left c m

_findIn :: Int -> a -> String -> (a -> b) -> Int -> Int -> (Maybe b, Int)
_findIn a o t b c m
    | (m <= c) && (c < newM) = (Just (b o),newM)
    | otherwise = (Nothing,newM)
    where newM = m + a + length t

findInName :: Name -> CursorToElement
findInName o@(Nam n) c m = _findIn 0 o n Right c m
