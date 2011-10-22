module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Data.List
import Debug.Trace
import Parser
import Turtle
import Eval
import qualified Control.Exception as E

data Process = EnteringValue | DoingNothing deriving (Eq, Show)

data Store = Store
  { code              :: String
  , turtle            :: Turtle
  , name              :: String
  , errorMsg          :: String
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)

initStore = Store { code     = ""
                  , turtle   = iniTurtle
                  , name     = ""
                  , errorMsg = ""
                  }

-----------------------------------------------------------

-- Redraws the entire window (i.e. a drawBoard and a drawBottomLine)
redraw :: Store -> Picture
redraw store = Pictures $ [clearBoard, drawInformation store] -- drawCode store, 

clearBoard = Color white $ rectangleSolid 1000 1000

-- Draws the information in the top
drawInformation :: Store -> Picture
drawInformation store 
    = Pictures 
      [ Translate (-280) 260 $ Color white $ rectangleSolid 210 50
      , Translate (-280) 260 $ Color black $ rectangleWire 210 50
      , Translate (-380) 270 $ Color blue  $ Scale 0.11 0.11 $ Text $ (name store)
      , Translate (-380) 255 $ Color red   $ Scale 0.11 0.11 $ Text $ (errorMsg store)
      , Translate (-380) 240 $ Color black $ Scale 0.09 0.09 $ Text $ "Use 'l' to load and parse a file"
      ]
        
        
-- Event handler
handleEvent :: Store -> Input -> (Store, [Output])

--- Load code
handleEvent store (KeyIn 'l')
    = (store {errorMsg=""}, [GraphPrompt ("Load code", "filename")])

--- Handle prompt
handleEvent store (Prompt ("Load code", filename))
    | filename /= "" = (store {errorMsg=""}, [ReadFile filename (TXTFile "")])
    | otherwise      = (store {errorMsg=""}, [])
  
--- Handle file load
handleEvent store (File filename (TXTFile input))
    | input /= "" = (store', [DrawPicture $ redraw store'])
    | otherwise   = (store, [])
    where
        acs = parseAcs input
        t = iniTurtle {actions=acs}
        store' = store {turtle=t, code=input, name=filename, errorMsg=""}
    
--- Handle drawing of picture if not done yet
handleEvent store@(Store {turtle=t@(Turtle {actions=(ac:acs)})}) _ = (store', [DrawPicture $ Pictures [p,drawInformation store']])
    where
        (t', p) = execAction t ac
        t'' = t' {actions=acs}
        store' = store {turtle=t'', errorMsg=if null acs then "Done drawing picture" else ""}


--- Unhandled event handler
handleEvent store _ = (store, [])
        
-----------------------------------------------------------

-- Draw the screen and install the event handler
doShow fn = installEventHandler "Let rut!" handleEvent store startPic 10
    where
        store = initStore
        startPic = Pictures $ [redraw store]
        
main = doShow ""