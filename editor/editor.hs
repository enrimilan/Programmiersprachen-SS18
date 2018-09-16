import System.IO
import Data.List
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
    eitherException <- (try $ writeFile path (intercalate "\n" f)) :: IO (Either IOException ())
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
        '3' -> mainLoop (State EditMode f p 0 0 l)
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
        --putStr f
        drawRule f
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


-- Line draw Functions

drawRule :: String -> IO ()
drawRule l = do
    resetFont
    case parse l of
        (Rul h b, rest) -> do
            drawHead h
            drawBody b
            resetAndColor White
            putStr "."
            drawFailure rest
        (RuleMissingDot h b, rest) -> do
            drawHead h
            drawBody b
            drawFailure rest
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
drawToken (Lit t) = drawToken' White "" t
drawToken (Plus t) = drawToken' Blue "+" t
drawToken (Star t) = drawToken' Yellow "*" t
drawToken Space = drawToken' White "" " "

drawToken' :: Color -> String -> String -> IO ()
drawToken' c p t = do
    resetAndColor c
    putStr (p ++ t)


-- Parser (parts from Fortgeschrittene Funktionale Programmierung)

type Parse a b = [a] -> [(b,[a])]

data Token = Lit String | Plus String | Star String | Space deriving (Eq,Ord,Show)
data Pattern = Pat [Token] | FailedPattern String deriving (Eq,Ord,Show)
data Name = Nam String deriving (Eq,Ord,Show)
data Atom = Ato Name [Pattern] [Pattern] | FailedAtom String deriving (Eq,Ord,Show)
data Goal = AtomGoal Atom | EqGoal Pattern Pattern | ShellGoal Pattern Pattern Pattern Pattern deriving (Eq,Ord,Show)
data Body = Bod [Goal] deriving (Eq,Ord,Show)
data Head = Hea Atom deriving (Eq,Ord,Show)
data Rule = Rul Head Body | RuleMissingDot Head Body | FailedRule String deriving (Eq,Ord,Show)
--data Programm = Pro [Rule] deriving (Eq,Ord,Show)

specialChars = ['\\', ':', '.', '=', '$', '-', '(', ')', '+', '*', ' ']
notSpecial c = not (elem c specialChars)

none :: Parse a b
none _ = []

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]

token :: Char -> Parse Char [Char]
token t = spots (\c escaped -> c == t && not escaped)

spots :: (Char -> Bool -> Bool) -> Parse Char [Char]
spots p "\\" = []
spots p ('\\':x:xs)
    | notSpecial x = []  -- Escaped and not a special char
    | p x True = [('\\':[x],xs)]  -- Escaped and a special char and p returns true
    | otherwise = []
spots p (x:xs)
    | p x False = [([x],xs)]
    | otherwise = []
spots p [] = []

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp
    = [((y,z),rem2) | (y,rem1) <- p1 inp, (z,rem2) <- p2 rem1 ]

build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [(f x, rem)|(x,rem) <- p inp]

neList :: Parse a b -> Parse a [b]
neList p = (p `build` (:[])) `alt` ((p >*> list p) `build` (uncurry (:)))

list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

optional :: Parse a b -> Parse a [b]
optional p = (succeed []) `alt` (p  `build` (:[]))

myoptional p = optional


listOfNotSpecialButNotSpaceOrEscaped = neList (spots (\c escaped -> notSpecial c || escaped))

litToken = listOfNotSpecialButNotSpaceOrEscaped `build` makeToken
    where makeToken nameList = Lit (concat nameList)

plusStarToken ps t = (token ps >*> listOfNotSpecialButNotSpaceOrEscaped) `build` makeToken
    where makeToken (_,nameList) = t (concat nameList)

plusToken = plusStarToken '+' Plus
starToken = plusStarToken '*' Star

spaceToken = (spots (\c escaped -> c == ' ' && not escaped)) `build` makeToken
    where makeToken _ = Space 

openBracket = token '('
closeBracket = token ')'

_pattern = 
    (openBracket >*> closeBracket) `build` makeEmptyPattern
    `alt`
    ((openBracket >*>
    list (litToken `alt` plusToken `alt` spaceToken) >*>
    optional starToken >*>
    closeBracket) `build` makePattern)
    where
        makeEmptyPattern _ = Pat []
        makePattern (_,(a,(b,_))) = Pat (a ++ b)

pattern inp 
    | _pattern inp == [] = [(FailedPattern inp, [])]
    | otherwise = _pattern inp


--listPattern p inp 
--    | p inp == [(FailedPattern inp,[])] = [([FailedPattern inp],[])]
--    | otherwise = takeWhile t (list ((:[]).last.p) inp)
--        where
--            t (ts, []) = not $ any s ts 
--            t _ = True
--            s (FailedPattern _) = True
--            s _ = False

--listPattern2 inp 
--    | inp == [] = []  -- empty string is empty list
--    | _pattern inp == [] = [([FailedPattern inp],[])]  -- [] is an error
--    | snd pat == [] = conv pat  -- exactly one pattern successfully parsed
--    | otherwise = conv2 ( (fst pat : concat (map fst (listPattern2 (snd pat)))), snd pat) ++ listPattern2 (snd pat)
--        where
--            conv (a, b) = [([a], b)]
--            conv2 (a, b) = [(a, b)]
--            pat = last (_pattern inp)

listPattern inp = _listPattern inp []
_listPattern inp e
    | inp == [] = [(e,[])]  -- empty string
    | _pattern inp == [] = [(e ++ [FailedPattern inp],[]), (e,inp)]  -- an error
    | otherwise = conv2 (en, snd pat) ++ _listPattern (snd pat) (en)
        where
            en = e ++ [fst pat]
            conv (a, b) = [([a], b)]
            conv2 (a, b) = [(a, b)]
            pat = last (_pattern inp)


--list2 (FailedPattern a, b) = [(a,[b])]
--list2 inp = list inp

--pattern = optional _pattern `build` makePattern
--    where
--        makePattern [Pat x] = Pat x
--        makePattern [] = Pat []

name = listOfNotSpecialButNotSpaceOrEscaped `build` (Nam . concat)

-- test thing, that didn't work
--[a] -> [(b,[a])]
--tryp :: Parse Char [Char] -> Char -> Parse Char [Char]
--tryp f i = f
--    | f i == [] = i
--    | otherwise = f i

--test = (list litToken) >*> ( closeBracket)
--testb = test `build` testbb
--testbb ((Lit t:ts,b)) = t : testbb ((ts,b))
--testbb (([],_)) = []


_atom =
    (name >*>
    listPattern >*>
    token '-' >*>
    listPattern) `build` makeAtom
    where makeAtom (name,(p1,(_,p2))) = Ato name p1 p2

atom inp 
    | _atom inp == [] = [(FailedAtom inp, [])]
    | otherwise = _atom inp

goal =
    (atom `build` AtomGoal)
    `alt`
    ((pattern >*>
    token '=' >*>
    pattern) `build` makeEqGoal)
    `alt`
    ((token '$' >*>
    pattern >*>
    pattern >*>
    token '-' >*>
    pattern >*>
    pattern) `build` makeShellGoal)
    where
        makeEqGoal (p1, (_, p2)) = EqGoal p1 p2
        makeShellGoal (_, (p1, (p2, (_, (p3, p4))))) = ShellGoal p1 p2 p3 p4

body =
    (list (token ':' >*> goal)) `build` makeBody
    where makeBody goals = Bod (map snd goals)

headp = atom `build` Hea

_rule =
    (headp >*>
    body >*>
    token '.') `build` makeRule
    where makeRule (h, (b, _)) = Rul h b

rule inp 
    | _rule inp == [] = withoutDot inp
    | otherwise = _rule inp
    where
        withoutDot inp
            | (headp >*> body) inp == [] = [(FailedRule inp, [])]
            | otherwise = ((headp >*> body) `build` makeRule) inp
        makeRule (h, b) = RuleMissingDot h b

--program = list rule `build` Pro  -- todo not needed?

parse inp
    | rule inp == [] = error "This should not happen"
    | otherwise = findBest (rule inp)
    where
        isRule (Rul _ _) = True
        isRule _ = False
        checkRule = isRule . fst
        restLength = length . snd
        findBest res
            | any checkRule res = last (filter checkRule res)  -- Successful parse
            | otherwise = head $ sortBy (\x y -> compare (restLength x) (restLength y)) res  -- Unsuccessful parse, find 



