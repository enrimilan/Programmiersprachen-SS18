-- Functions for updating the state
module Update where

import Models
import Parser

parseProgram :: State -> State
parseProgram s@(State m f p x y l rules) = State EditMode f p 0 0 l (parseRules f)

moveUp :: State -> Int -> State
moveUp s@(State m f p x y l rules) screenHeight = 
    State EditMode f p (ifBetween (x - 1) 0 screenHeight x) y checkUp rules
        where
            checkUp
                | x == 0 = l - 1
                | otherwise = l

moveDown :: State -> Int -> State
moveDown s@(State m f p x y l rules) screenHeight =
    State EditMode f p (ifBetween (x + 1) 0 (screenHeight - 1) x) y checkDown rules
        where
            checkDown
                | x == screenHeight - 1 && (length f) - screenHeight > l = l + 1
                | otherwise = l

moveRight :: State -> Int -> State
moveRight s@(State m f p x y l rules) screenWidth = State EditMode f p x (ifBetween (y + 1) 0 screenWidth y) l rules

moveLeft :: State -> Int -> State
moveLeft s@(State m f p x y l rules) screenWidth = State EditMode f p x (ifBetween (y - 1) 0 screenWidth y) l rules

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

-- Util Functions

ifBetween x a b y
    | x >= a && x <= b = x
    | otherwise = y

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