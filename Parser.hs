module Parser where

import Prelude
import Turtle
import Data.List
import FPPrac.Graphics
import qualified Data.Map as Map
import Data.Char (isSpace,isDigit)

nativeFuncs = ["forward","left","right","move","pendown","penup","color"]

parseAcs :: String -> [Action]
parseAcs string = parseAc [] (lines string)

parseAc :: [(String,([String],[String]))] -> [String] -> [Action]
parseAc _ [] = []
parseAc pFuncs str@(l:ls)
    | key == "do" = parseAc (pFuncs++[dFunc]) dLines
    | key == "repeat" = rAcs ++ parseAc (pFuncs) rLines
    | key `elem` nativeFuncs = nAc : parseAc pFuncs ls
    | key `elem` parsedFuncs = pAcs ++ parseAc pFuncs ls
    | otherwise = error $ "Unknown key in line: " ++ l
    where
        elems = words l
        key = head elems
        --
        (dFunc, dLines) = strToDo str
        --
        (rAcs, rLines)  = strToRepeat pFuncs str
        --
        nAc = nativeStrToAction l --De actie van deze line
        --
        parsedFuncs = map fst pFuncs
        pAcs = pFuncToAction pFuncs l
       

strToDo ls 
    | not (null ls') = ((name, (arglist,tail cmds)), (tail ls'))
    | otherwise      = error "No 'end' tag found in 'do' block." 
    where
        elems   = tail . words . head $ ls
        name    = head elems
        arglist = tail elems
        (cmds,ls') = span (/="end") ls -- "end" zit nu nog in ls'
 
strToRepeat pFuncs ls
    | not (null ls') = (acs, (tail ls'))
    | otherwise      = error "No 'end' tag found in 'repeat' block."
    where
        (cmds,ls') = span (/="end") ls -- "end" zit nu nog in ls'
        elems   = tail . words . head $ ls
        count   = read(head elems)
        acs     = concat $ replicate count (parseAc pFuncs (tail cmds))
        

-- nativeStrToAction takes a string that contains a native function and converts it to one action
nativeStrToAction :: String -> Action
nativeStrToAction str = nativeStrToAction' cmd args
	where
		(cmd:args) = words str

-- Function that takes the function name and the argument list and retunrs the action
nativeStrToAction' :: String -> [String] -> Action
nativeStrToAction' "forward" [s]   | isNumber s = Forward (read s)
nativeStrToAction' "forward" _                  = error "Wrong use of forward. Expected one numeric argument."
nativeStrToAction' "left" [s]      | isNumber s = Turn (read s)
nativeStrToAction' "left" _                     = error "Wrong use of left. Expected one numeric argument."
nativeStrToAction' "right" [s]     | isNumber s = Turn (360-(read s))
nativeStrToAction' "right" _                    = error "Wrong use of right. Expected one numeric argument."
nativeStrToAction' "move" [x,y]    | isNumber x && isNumber y = GoTo (read x) (read y)
nativeStrToAction' "move" _                                   = error "Wrong use of move. Expected two numeric arguments."
nativeStrToAction' "pendown" []    = PenDown
nativeStrToAction' "pendown" _     = error "Wrong use of pendown. Expected no arguments."
nativeStrToAction' "penup" []      = PenUp
nativeStrToAction' "penup" _       = error "Wrong use of penup. Expected no arguments."
nativeStrToAction' "color" [r,g,b] | isNumber r && isNumber g && isNumber b = ChangeColor (makeColor8 (read r) (read g) (read b) 255)
nativeStrToAction' "color" _                                                = error "Wrong use of color. Expected three numeric arguments."
nativeStrToAction' x _             = error ("Incorrect call of nativeStrToAction: " ++ x ++ " is not a valid function.")

isNumber s = all isDigit s
