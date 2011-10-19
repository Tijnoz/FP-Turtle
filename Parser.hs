module Parser where

import Prelude
import Turtle

initFunctions = ["do","forward","left","right","move","pendown","penup"]
initParsed    = [("main",[])]

parseAcs :: String -> [Action]
parseAcs funcs string = 
    | fstLine = 