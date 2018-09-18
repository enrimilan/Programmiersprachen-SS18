module Parser where

import System.Console.ANSI
import Data.List
import Models

-- Parser (parts from Fortgeschrittene Funktionale Programmierung)

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