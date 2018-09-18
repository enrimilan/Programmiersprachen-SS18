import System.IO
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
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
    printLines (State m dropped p x y l rules) (min (length dropped) h)
        where
            dropped = (drop l f)

printLines :: State -> Int -> IO ()
printLines (State _ [] _ _ _ _ _) _ = return ()
printLines (State m (f:fs) p x y l (r:rs)) n 
    | n<= 0 = return ()
    | otherwise = do
        putStrLn ""
        --putStr f
        drawRule r
        w <- screenWidth
        printLines (State m fs p x y l rs) (n - 1 - (quot (length f) w))

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

intToColor :: Integral a => a -> Color
intToColor i = case mod i 4 of
    0 -> Red
    1 -> Green
    2 -> Magenta
    3 -> Cyan

drawRule :: (Rule, [Char]) -> IO ()
drawRule rule = do
    case rule of
        (Rul h b, rest) -> do
            drawHead ch
            drawBody cb
            resetAndColor White
            putStr "."
            drawFailure rest
            where
                (ch, cm) = (colorHead h Map.empty)
                cb = fst (colorBody b cm)
        (RuleMissingDot h b, rest) -> do
            drawHead ch
            drawBody cb
            drawFailure rest
            where
                (ch, cm) = (colorHead h Map.empty)
                cb = fst (colorBody b cm)
        (FailedRule r1, r2) -> do
            drawFailure (r1 ++ r2)
    

drawFailure :: String -> IO ()
drawFailure f = do
    resetAndColor Red
    setBold
    putStr f

drawHead :: Head -> IO ()
drawHead (Hea a) = drawAtom a

drawBody :: Body -> IO ()
drawBody (Bod []) = return ()
drawBody (Bod (g:gs)) = do
    resetAndColor Blue
    putStr ":"
    drawGoal g
    drawBody (Bod gs)

drawGoal :: Goal -> IO ()
drawGoal (AtomGoal a) = drawAtom a
drawGoal (EqGoal p1 p2) = do 
    drawPattern p1
    resetAndColor Magenta
    putStr "="
    drawPattern p2
drawGoal (ShellGoal p1 p2 p3 p4) = do
    resetAndColor Yellow
    putStr "$"
    drawPattern p1; drawPattern p2
    resetAndColor Yellow
    putStr "-"
    drawPattern p3; drawPattern p4

drawAtom :: Atom -> IO ()
drawAtom (FailedAtom f) = drawFailure f
drawAtom (Ato n p1 p2) = do
    drawName n
    drawPatterns p1
    resetAndColor Green
    putStr "-"
    drawPatterns p2
        where
            drawPatterns (p:ps) = do
                drawPattern p
                drawPatterns ps
            drawPatterns _ = return ()

drawName :: Name -> IO ()
drawName (Nam n) = do
    resetAndColor Cyan
    putStr n

drawPattern :: Pattern -> IO ()
drawPattern (FailedPattern f) = drawFailure f
drawPattern (Pat ts) = do
    resetAndColor Magenta
    putStr "("
    drawTokens ts
    resetAndColor Magenta
    putStr ")"
        where
            drawTokens (t:ts) = do
                drawToken t
                drawTokens ts
            drawTokens _ = return ()

drawToken :: Token -> IO ()
drawToken (Lit t c) = drawToken' White c "" t
drawToken (Plus t c) = drawToken' Blue c "+" t
drawToken (Star t c) = drawToken' Yellow c "*" t
drawToken Space = drawToken' White Black "" " "

drawToken' :: Color -> Color -> String -> String -> IO ()
drawToken' c b p t = do
    resetAndColor c
    setBGColor b
    putStr (p ++ t)


-- Coloring Functions

colorBody :: Body -> Map [Char] Color -> (Body, Map [Char] Color)
colorBody (Bod gs) m = (Bod cg,cm)
    where (cg,cm) = colorGoals gs m

colorGoals :: [Goal] -> Map [Char] Color -> ([Goal], Map [Char] Color)
colorGoals [] m = ([],m)
colorGoals (g:gs) m = (cg : (fst rest),snd rest)
    where
        (cg,cm) = colorGoal g m
        rest = colorGoals gs cm

colorGoal :: Goal -> Map [Char] Color -> (Goal, Map [Char] Color)
colorGoal (AtomGoal g) m = (AtomGoal cg,cm)
    where (cg,cm) = colorAtom g m
colorGoal (EqGoal g1 g2) m = (EqGoal cg1 cg2,cm2)
    where
        (cg1,cm1) = colorPattern g1 m
        (cg2,cm2) = colorPattern cg1 cm1
colorGoal (ShellGoal g1 g2 g3 g4) m = (ShellGoal cg1 cg2 cg3 cg4,cm4)
    where
        (cg1,cm1) = colorPattern g1 m
        (cg2,cm2) = colorPattern g2 cm1
        (cg3,cm3) = colorPattern g3 cm2
        (cg4,cm4) = colorPattern g4 cm3

colorAtom :: Atom -> Map [Char] Color -> (Atom, Map [Char] Color)
colorAtom f@(FailedAtom _) m = (f,m)
colorAtom (Ato s as1 as2) m = (Ato s ca1 ca2,cm2)
    where
        (ca1,cm1) = colorPatterns as1 m
        (ca2,cm2) = colorPatterns as2 cm1

colorPatterns :: [Pattern] -> Map [Char] Color -> ([Pattern], Map [Char] Color)   
colorPatterns [] m = ([],m)
colorPatterns (a:as) m = (ca : (fst rest),snd rest)
    where 
        (ca,cm) = colorPattern a m
        rest = colorPatterns as cm

colorHead :: Head -> Map [Char] Color -> (Head, Map [Char] Color)
colorHead (Hea h) m = (Hea ch,cm)
    where (ch,cm) = colorAtom h m

colorPattern :: Pattern -> Map [Char] Color -> (Pattern, Map [Char] Color)
colorPattern f@(FailedPattern _) m = (f,m)
colorPattern (Pat ts) m = (Pat ct,cm)
    where
        (ct,cm) = colorTokens ts m
    
colorTokens :: [Token] -> Map [Char] Color -> ([Token], Map [Char] Color)
colorTokens [] m = ([],m)
colorTokens (t:ts) m = (ct : (fst rest),snd rest)
    where
        (ct,cm) = colorToken t m
        rest = colorTokens ts cm

colorToken :: Token -> Map [Char] Color -> (Token, Map [Char] Color)
colorToken Space m = (Space,m)
colorToken (Lit t _) m = _colorToken Lit t m ""
colorToken (Plus t _) m = _colorToken Plus t m "+"
colorToken (Star t _) m = _colorToken Star t m "*"

_colorToken :: Ord a1 => ([a1] -> Color -> a2) -> [a1] -> Map [a1] Color -> [a1] -> (a2, Map [a1] Color)
_colorToken c t m p
    | Map.member (p ++ t) m = (c t (m ! (p ++ t)),m)
    | otherwise = (c t (mapToColor m),Map.insert (p ++ t) (mapToColor m) m)

mapToColor :: Map k a -> Color
mapToColor m = intToColor (Map.size m)