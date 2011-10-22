module Parser where

import Prelude
import Turtle
import Data.List
import FPPrac.Graphics
import qualified Data.Map as Map
import Data.Char (isSpace,isDigit)
import Debug.Trace

type ParsedFunction = (String,([String],[String]))

nativeFuncs = ["forward","left","right","move","pendown","penup","color","sleep","clear"]

parseAcs :: String -> [Action]
parseAcs string = parseAc [] (lines string)

parseAc :: [ParsedFunction] -> [String] -> [Action]
parseAc _ [] = []
parseAc pFuncs str@(l:ls)
    | null elems             = parseAc pFuncs ls
    | key == "do"            = parseAc (pFuncs++[dFunc]) dLines
    | key == "repeat"        = rAcs ++ parseAc (pFuncs) rLines
    | key `elem` nativeFuncs = nAc ++ parseAc pFuncs ls
    | key `elem` parsedFuncs = pAcs ++ parseAc pFuncs ls
    | otherwise              = error $ "Unknown key in line: " ++ l
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
        pAcs = pFuncToAcs pFuncs l
       

strToDo ls 
    | not (null ls') = ((name, (arglist,tail cmds)), (tail ls'))
    | otherwise      = error "No 'end' tag found in 'do' block." 
    where
        elems   = tail . words . head $ ls
        name    = head elems
        arglist = tail elems
        (cmds,ls') = findEnd 0 ([],ls) -- "end" zit nu nog in ls'
 
strToRepeat pFuncs ls
    | not (null ls') = (acs, (tail ls'))
    | otherwise      = error "No 'end' tag found in 'repeat' block."
    where
        (cmds,ls') = findEnd 0 ([],ls) -- "end" zit nu nog in ls'
        elems   = tail . words . head $ cmds
        count   = read(head elems)
        acs     = concat $ replicate count (parseAc pFuncs (tail cmds))

findEnd _ (_,[]) = error "No end found."
findEnd d (p,(l:ls))
    | key == "repeat" || key == "do"             = trace (show $ d+1) $ findEnd (d+1) (p++[l],ls)
    | key == "end" && d > 1                      = trace (show $ d-1) $ findEnd (d-1) (p++[l],ls)
    | key == "end"                               = (p,(l:ls))
    | otherwise                                  = findEnd d (p++[l],ls)
    where
        key = head . words $ l

-- nativeStrToAction takes a string that contains a native function and converts it to one action
nativeStrToAction :: String -> [Action]
nativeStrToAction str = nativeStrToAction' cmd args
    where
        (cmd:args) = words str

-- Function that takes the function name and the argument list and retunrs the action
nativeStrToAction' :: String -> [String] -> [Action]
nativeStrToAction' "forward" [s]   | isNumber s = [Forward (read s)]
nativeStrToAction' "forward" _                  = error "Wrong use of forward. Expected one numeric argument."
nativeStrToAction' "left" [s]      | isNumber s = [Turn (read s)]
nativeStrToAction' "left" _                     = error "Wrong use of left. Expected one numeric argument."
nativeStrToAction' "right" [s]     | isNumber s = [Turn (360-(read s))]
nativeStrToAction' "right" _                    = error "Wrong use of right. Expected one numeric argument."
nativeStrToAction' "move" [x,y]    | isNumber x && isNumber y = [GoTo (read x) (read y)]
nativeStrToAction' "move" _                                   = error "Wrong use of move. Expected two numeric arguments."
nativeStrToAction' "pendown" []    = [PenDown]
nativeStrToAction' "pendown" _     = error "Wrong use of pendown. Expected no arguments."
nativeStrToAction' "penup" []      = [PenUp]
nativeStrToAction' "penup" _       = error "Wrong use of penup. Expected no arguments."
nativeStrToAction' "color" [r,g,b] | isNumber r && isNumber g && isNumber b = [ChangeColor (makeColor8 (read r) (read g) (read b) 255)]
nativeStrToAction' "color" _                                                = error "Wrong use of color. Expected three numeric arguments."
nativeStrToAction' "sleep" [s]     | isNumber s = replicate (read s) NoOp
nativeStrToAction' "sleep" _                    = error "Wrong use of sleep. Expected one numeric argument."
nativeStrToAction' "clear" []      = [Clear]
nativeStrToAction' "clear" _       = error "Wrong use of clear. Expected no arguments."
nativeStrToAction' x _             = error ("Incorrect call of nativeStrToAction: " ++ x ++ " is not a valid function.")

isNumber s = all isDigit s

-- Converts a parsed function to actions
pFuncToAcs :: [ParsedFunction] -> String -> [Action]
pFuncToAcs pFuncs l 
    | length params == (length arglist) = acs
    | otherwise                         = error ("Wrong arguments for "++(head elems))
    where
        elems = words l
        params = tail elems
        Just (arglist,strs) = lookup (head elems) pFuncs -- `elem` is al gedaan.
        parseable = mkParsable (zip arglist params) strs
        acs = parseAc pFuncs parseable

mkParsable argmap [] = []       
mkParsable argmap (l:ls) = ((head elems)++" "++subParams) : (mkParsable argmap ls)
    where
        elems = words l
        subArgs = tail elems
        subParams = unwords $ map (argToParam argmap) subArgs
        
argToParam argmap arg@(':':_)
    | may /= Nothing = param
    | otherwise      = error "Unknown parameter found."
    where
        may = lookup arg argmap
        Just param = may
        
argToParam _ arg = arg