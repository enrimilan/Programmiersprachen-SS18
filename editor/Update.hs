-- Functions for updating the state
module Update where

import Models
import Parser

parseProgram :: State -> State
parseProgram s@(State m f p x y l rules) = State EditMode f p 0 0 l (parseRules f)


-- Cursor movement

moveUp :: State -> Int -> State
moveUp s@(State m f p x y l rules) screenHeight = 
    State EditMode f p newX newY checkUp rules
        where
            newX = if x > 0 then x - 1 else x
            newY = if x > 0 && (length $ f!!(x-1)) > y then y else if x > 0 then length $ f!!(x-1) else y
            checkUp
                | x == 0 = l - 1
                | otherwise = l

moveDown :: State -> Int -> State
moveDown s@(State m f p x y l rules) screenHeight =
    State EditMode f p newX newY checkDown rules
        where
            newX = if x < length f - 1  then x + 1 else x
            newY = if x < length f - 1 && (length $ f!!(x+1)) > y then y else if x < length f - 1 then length $ f!!(x+1) else y
            checkDown
                | x == screenHeight - 1 && (length f) - screenHeight > l = l + 1
                | otherwise = l

moveRight :: State -> Int -> State
moveRight s@(State m f p x y l rules) screenWidth = 
    State EditMode f p newX newY l rules
        where
            ruleLength = length $ f!!x
            newX       = if x >= length f - 1 then x else if y < ruleLength then x else x+1
            newY       = if y < ruleLength then y+1 else if x >= length f - 1 then y else 0

moveLeft :: State -> Int -> State
moveLeft s@(State m f p x y l rules) screenWidth = 
    State EditMode f p newX newY l rules
        where
            newX = if x>0 && y == 0 then x - 1 else x
            newY = if y > 0 then y - 1 else if x > 0 then length $ f!!(x-1) else y


-- Modifying the code

insertChar :: State -> [Char] -> State
insertChar s@(State m f p x y l rules) c = State m new p x (y + 1) l newRules
    where
        newRules = replaceAtIndex x [parsedRule] rules 
        parsedRule = parse $ (drop l new) !! x
        new = replaceAtIndex x [newLine] f
        newLine = insertAtIndex y c (f !! x)

insertEnter :: State -> State
insertEnter s@(State m f p x y l rules) = State m new p (x + 1) 0 l newRules
    where
        newRules = if y >= (length $ f !! x) then insertAtIndex (x+1) [parse ""] rules else parseRules (drop l new)
        new = replaceAtIndex x [xs,ys] f
        (xs,ys) = splitAt y (f !! x)

backspace :: State -> State
backspace s@(State m f p x y l rules)
    | y == 0 && x == 0 = s
    | y == 0 = State m newCombined p (x - 1) (length (f !! (x - 1))) l (parseRules (drop l newCombined))
    | otherwise = State m new p x (y - 1) l newRules
        where
            newRules = replaceAtIndex x [parsedRule] rules 
            parsedRule = parse $ (drop l new) !! x
            newCombined = removeAtIndex x (replaceAtIndex (x - 1) [newLineCombined] f)
            newLineCombined = (f !! (x - 1)) ++ (f !! x)
            new = replaceAtIndex x [newLine] f
            newLine = removeAtIndex (y - 1) (f !! x)


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