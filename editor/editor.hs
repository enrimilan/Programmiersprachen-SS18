import System.IO
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Exception
import System.Console.ANSI
import System.Console.Terminal.Size

type FileBuffer = String
type Path = String
data EMode = EditMode | MenuMode deriving Eq
data State = 
    State 
        { editorMode :: EMode
        , fileBuffer :: [String]
        , filePath :: String
        , screenX :: Int
        , screenY :: Int
        , scrollLine :: Int 
        , rules :: [(Rule, [Char])]
        }


-- Printing Functions

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
        '3' -> mainLoop (State EditMode f p 0 0 l (parseRules f))
        '4' -> do
            putStrLn " Exiting!"
            resetFont
        _ -> mainLoopMenu s


-- Editor Mode
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


insertChar :: State -> [Char] -> State
insertChar s@(State m f p x y l rules) c = State m new p x (y + 1) l (parseRules (drop l new))
    where
        new = replaceAtIndex x [newLine] f
        newLine = insertAtIndex y c (f !! x)

insertEnter :: State -> State
insertEnter s@(State m f p x y l rules) = State m new p (x + 1) 0 l (parseRules (drop l new))
    where
        new = replaceAtIndex x [xs,ys] f
        (xs,ys) = splitAt y (f !! x)

backspace :: State -> State
backspace s@(State m f p x y l rules)
    | y == 0 && x == 0 = s
    | y == 0 = State m newCombined p (x - 1) (length (f !! (x - 1))) l (parseRules (drop l newCombined))
    | otherwise = State m new p x (y - 1) l (parseRules (drop l new))
        where
            newCombined = removeAtIndex x (replaceAtIndex (x - 1) [newLineCombined] f)
            newLineCombined = (f !! (x - 1)) ++ (f !! x)
            new = replaceAtIndex x [newLine] f
            newLine = removeAtIndex (y - 1) (f !! x)

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
        "\ESC[A" -> mainLoop (State EditMode f p (ifBetween (x - 1) 0 h x) y checkUp rules) -- up
            where
                checkUp
                    | x == 0 = l - 1
                    | otherwise = l
        "\ESC[B" -> mainLoop (State EditMode f p (ifBetween (x + 1) 0 (h - 1) x) y checkDown rules) -- down
            where
                checkDown
                    | x == h - 1 && (length f) - h > l = l + 1
                    | otherwise = l
        "\ESC[C" -> mainLoop (State EditMode f p x (ifBetween (y + 1) 0 w y) l rules) -- right
        "\ESC[D" -> mainLoop (State EditMode f p x (ifBetween (y - 1) 0 w y) l rules) -- left
        "\n" -> mainLoopEdit (insertEnter s)
        "\DEL" -> mainLoopEdit (backspace s)
        _ -> mainLoopEdit (insertChar s c)



-- Main Functions

mainLoop :: State -> IO ()
mainLoop s@(State MenuMode _ _ _ _ _ _) = mainLoopMenu s
mainLoop s@(State EditMode _ _ _ _ _ _) = mainLoopEdit s

main = do
    clearScreenAndSetCursor
    mainLoop (State MenuMode [""] "new" 0 0 0 [])


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


-- Parser (parts from Fortgeschrittene Funktionale Programmierung)

type Parse a b = [a] -> [(b,[a])]

data Token = Lit String Color | Plus String Color | Star String Color | Space deriving (Eq,Ord,Show)
data Pattern = Pat [Token] | FailedPattern String deriving (Eq,Ord,Show)
data Name = Nam String deriving (Eq,Ord,Show)
data Atom = Ato Name [Pattern] [Pattern] | FailedAtom String deriving (Eq,Ord,Show)
data Goal = AtomGoal Atom | EqGoal Pattern Pattern | ShellGoal Pattern Pattern Pattern Pattern deriving (Eq,Ord,Show)
data Body = Bod [Goal] deriving (Eq,Ord,Show)
data Head = Hea Atom deriving (Eq,Ord,Show)
data Rule = Rul Head Body | RuleMissingDot Head Body | FailedRule String deriving (Eq,Ord,Show)

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


listOfNotSpecialButNotSpaceOrEscaped :: Parse Char [String]
listOfNotSpecialButNotSpaceOrEscaped = neList (spots (\c escaped -> notSpecial c || escaped))

litToken :: Parse Char Token
litToken = listOfNotSpecialButNotSpaceOrEscaped `build` makeToken
    where makeToken nameList = Lit (concat nameList) White

plusStarToken :: Char -> ([Char] -> Color -> c) -> Parse Char c
plusStarToken ps t = (token ps >*> listOfNotSpecialButNotSpaceOrEscaped) `build` makeToken
    where makeToken (_,nameList) = t (concat nameList) White

plusToken :: Parse Char Token
plusToken = plusStarToken '+' Plus
starToken :: Parse Char Token
starToken = plusStarToken '*' Star

spaceToken :: Parse Char Token
spaceToken = (spots (\c escaped -> c == ' ' && not escaped)) `build` makeToken
    where makeToken _ = Space 

openBracket :: Parse Char String
openBracket = token '('
closeBracket :: Parse Char String
closeBracket = token ')'

_pattern :: Parse Char Pattern
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

pattern :: Parse Char Pattern
pattern inp 
    | _pattern inp == [] = [(FailedPattern inp, [])]
    | otherwise = _pattern inp -- todo try speed improvement: [last (_pattern inp)]

listPattern :: [Char] -> [([Pattern], [Char])]
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

name :: Parse Char Name
name = listOfNotSpecialButNotSpaceOrEscaped `build` (Nam . concat)

_atom :: Parse Char Atom
_atom =
    (name >*>
    listPattern >*>
    token '-' >*>
    listPattern) `build` makeAtom
    where makeAtom (name,(p1,(_,p2))) = Ato name p1 p2

atom :: Parse Char Atom
atom inp 
    | _atom inp == [] = [(FailedAtom inp, [])]
    | otherwise = _atom inp

goal :: Parse Char Goal
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

body :: Parse Char Body
body =
    (list (token ':' >*> goal)) `build` makeBody
    where makeBody goals = Bod (map snd goals)

headp :: Parse Char Head
headp = atom `build` Hea

_rule :: Parse Char Rule
_rule =
    (headp >*>
    body >*>
    token '.') `build` makeRule
    where makeRule (h, (b, _)) = Rul h b

rule :: Parse Char Rule
rule inp 
    | _rule inp == [] = withoutDot inp
    | otherwise = _rule inp
    where
        withoutDot inp
            | (headp >*> body) inp == [] = [(FailedRule inp, [])]
            | otherwise = ((headp >*> body) `build` makeRule) inp
        makeRule (h, b) = RuleMissingDot h b

parse :: [Char] -> (Rule, [Char])
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
            | otherwise = head $ sortBy (\x y -> compare (restLength x) (restLength y)) res  -- Unsuccessful parse, find with least rest

parseRules :: [String] -> [(Rule, [Char])]
parseRules rules = map parse rules