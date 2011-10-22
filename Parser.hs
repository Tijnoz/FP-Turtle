module Parser where

import Prelude
import Turtle
import Data.List
import FPPrac.Graphics
import qualified Data.Map as Map
import Data.Char (isSpace,isDigit)
import Debug.Trace
import Eval

type ParsedFunction = (String,([String],[String]))

nativeFuncs = ["forward","left","right","move","pendown","penup","color","sleep","clear"]

-- Parses a string into a list of actions
parseAcs :: String -> [Action]
parseAcs string = parseAc [] (lines string)

-- Parses a oneliner into a list of actions.
parseAc :: [ParsedFunction] -> [String] -> [Action]
parseAc _ [] = []
parseAc pFuncs str@(l:ls)
    | null elems             = parseAc pFuncs ls                                        -- Skip empty lines
    | head l == '#'          = parseAc pFuncs ls                                        -- Skip comments
    | key == "do"            = parseAc (pFuncs++[dFunc]) dLines                         -- Parse do blocks
    | key == "repeat"        = rAcs ++ parseAc (pFuncs) rLines                          -- Parse repeat blocks
    | key `elem` nativeFuncs = nAc ++ parseAc pFuncs ls                                 -- Parse native functions
    | key `elem` parsedFuncs = pAcs ++ parseAc pFuncs ls                                -- Parse previously parsed functions
    | otherwise              = error $ "Unknown command " ++ key ++ " in line: " ++ l   -- Error!
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
       
-- Parse a do block: returns the code that should be executed when the function is called, and the remaining lines
strToDo :: [String] -> (ParsedFunction, [String])
strToDo ls = ((name, (arglist,tail cmds)), ls')
    where
        elems   = tail . words . head $ ls
        name    = head elems
        arglist = tail elems
        (cmds,ls') = splitAtEnd ls -- "end" is not in ls' anymore
 
-- Parse a repeat block
strToRepeat :: [ParsedFunction] -> [String] -> ([Action], [String])
strToRepeat pFuncs ls = (acs, ls')
    where
        (cmds,ls') = splitAtEnd ls -- "end" is not in ls' anymore
        args  = tail . words . head $ ls
        count = read(head args)
        acs   = concat $ replicate count (parseAc pFuncs (tail cmds))

-- Helper method to split a block at the appropriate 'end'
splitAtEnd :: [String] -> ([String],[String])
splitAtEnd = splitAtEnd' 0 []

splitAtEnd' _ bl []                  = error $ "No end of block '" ++ (head . words . head $ bl) ++ "' found"
splitAtEnd' c bl (l:ls)
    | key `elem` ["repeat", "do"] = splitAtEnd' (c+1) (bl++[l]) ls
    | key == "end" && c > 1       = splitAtEnd' (c-1) (bl++[l]) ls
    | key == "end"                = (bl, ls)
    | otherwise                   = splitAtEnd' (c) (bl++[l]) ls
    where
        key = head . words $ l

-- nativeStrToAction takes a string that contains a native function and converts it to one action
nativeStrToAction :: String -> [Action]
nativeStrToAction str = nativeStrToAction' cmd args
    where
        (cmd:args) = words str

-- Function that takes the function name and the argument list and retunrs the action
nativeStrToAction' :: String -> [String] -> [Action]
nativeStrToAction' "forward" [s]   | isNumber s = [Forward (fromIntegral . eval $ s)]
nativeStrToAction' "forward" _                  = error "Wrong use of forward. Expected one numeric argument."
nativeStrToAction' "left" [s]      | isNumber s = [Turn (eval s)]
nativeStrToAction' "left" _                     = error "Wrong use of left. Expected one numeric argument."
nativeStrToAction' "right" [s]     | isNumber s = [Turn (360-(eval s))]
nativeStrToAction' "right" _                    = error "Wrong use of right. Expected one numeric argument."
nativeStrToAction' "move" [x,y]    | isNumber x && isNumber y = [GoTo (fromIntegral . eval $ x) (fromIntegral . eval $ y)]
nativeStrToAction' "move" _                                   = error "Wrong use of move. Expected two numeric arguments."
nativeStrToAction' "pendown" []    = [PenDown]
nativeStrToAction' "pendown" _     = error "Wrong use of pendown. Expected no arguments."
nativeStrToAction' "penup" []      = [PenUp]
nativeStrToAction' "penup" _       = error "Wrong use of penup. Expected no arguments."
nativeStrToAction' "color" [r,g,b] | isNumber r && isNumber g && isNumber b = [ChangeColor (makeColor8 (eval r) (eval g) (eval b) 255)]
nativeStrToAction' "color" _                                                = error "Wrong use of color. Expected three numeric arguments."
nativeStrToAction' "sleep" [s]     | isNumber s = replicate (eval s) NoOp
nativeStrToAction' "sleep" _                    = error "Wrong use of sleep. Expected one numeric argument."
nativeStrToAction' "clear" []      = [Clear]
nativeStrToAction' "clear" _       = error "Wrong use of clear. Expected no arguments."
nativeStrToAction' x _             = error ("Incorrect call of nativeStrToAction: " ++ x ++ " is not a valid function.")

isNumber s = True -- all isDigit s

-- Converts a parsed function to actions. Given are the list of parsed functions and the call.
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

-- Replaces all argument-occurences of keys in the argmap of the given lines to their respective values
-- Example: argmap = ((":x", 3)) and ls = ["ex :x"], then result is ["ex 3"]
mkParsable :: [(String, String)] -> [String] -> [String]
mkParsable argmap [] = []       
mkParsable argmap (l:ls) = ((head elems)++" "++(subParams)) : (mkParsable argmap ls)
    where
        elems = words l
        subArgs = tail elems
        subParams = unwords $ map (show . (evalA argmap)) subArgs -- This seems stupid, but we parse it as a string
