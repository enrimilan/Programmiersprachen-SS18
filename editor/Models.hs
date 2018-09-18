module Models where

import System.Console.ANSI

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

initialState :: State
initialState = State MenuMode [""] "new" 0 0 0 []

type Parse a b = [a] -> [(b,[a])]

data Token = Lit String Color | Plus String Color | Star String Color | Space deriving (Eq,Ord,Show)
data Pattern = Pat [Token] | FailedPattern String deriving (Eq,Ord,Show)
data Name = Nam String deriving (Eq,Ord,Show)
data Atom = Ato Name [Pattern] [Pattern] | FailedAtom String deriving (Eq,Ord,Show)
data Goal = AtomGoal Atom | EqGoal Pattern Pattern | ShellGoal Pattern Pattern Pattern Pattern deriving (Eq,Ord,Show)
data Body = Bod [Goal] deriving (Eq,Ord,Show)
data Head = Hea Atom deriving (Eq,Ord,Show)
data Rule = Rul Head Body | RuleMissingDot Head Body | FailedRule String deriving (Eq,Ord,Show)