module Parser where

import Prelude
import Turtle
import Data.List
import FPPrac.Graphics
import qualified Data.Map as Map

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
        
 
nativeStrToAction :: String -> Action
nativeStrToAction ('f':'o':'r':'w':'a':'r':'d':' ':str)
    | length (words str) == 1 = Forward (read str)
    | otherwise = error "Wrong use of forward."
nativeStrToAction ('l':'e':'f':'t':' ':str)
    | length (words str) == 1 = Turn (read str)
    | otherwise = error "Wrong use of left."
nativeStrToAction ('r':'i':'g':'h':'t':' ':str)
    | length (words str) == 1 = Turn (360-(read str))
    | otherwise = error "Wrong use of right."
nativeStrToAction ('m':'o':'v':'e':' ':str)
    | length args == 2 = GoTo (read (head args)) (read (args !! 1))
    | otherwise = error "Wrong use of move."
    where
        args = (words str)
nativeStrToAction ('p':'e':'n':'d':'o':'w':'n':[]) = PenDown
nativeStrToAction ('p':'e':'n':'u':'p':[]) = PenUp
nativeStrToAction ('c':'o':'l':'o':'r':' ':str)
    | length args == 3 = ChangeColor (makeColor8 (read (head args)) (read (args !! 1)) (read (args !! 2)) 255)
    | otherwise = error "Wrong use of coleur."
    where
        args = (words str)
        
pFuncToAction :: String -> ([Action],String)
pFuncToAction pFuncs line = acs
    where
        elems = words line
        params = tail elems
        Just (arglist,strs) = lookup (head elems) pFuncs -- `elem` is al gedaan.
        