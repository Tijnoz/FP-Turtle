module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Data.List
import Debug.Trace
import Parser
import Turtle
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
redraw store = Pictures $ [drawCode store, drawInformation store]

-- Draws the code
drawCode :: Store -> Picture
drawCode store = Pictures $ execTurtle ((turtle store) {actions=(parseAcs (code store))})

-- Draws the information in the top
drawInformation :: Store -> Picture
drawInformation store 
    = Pictures 
      [ Translate (-260) 260 $ Color white $ rectangleSolid 250 50
      , Translate (-260) 260 $ Color black $ rectangleWire 250 50
      , Translate (-380) 270 $ Color blue  $ Scale 0.11 0.11 $ Text $ (name store)
      --, Translate (-380) 255 $ Color red   $ Scale 0.11 0.11 $ Text $ (errorMsg store)
      , Translate (-380) 240 $ Color black $ Scale 0.09 0.09 $ Text $ "Use 'l' to load and parse a file"
      ]
	  
drawErr :: String -> Picture
drawErr err = Translate (-380) 255 $ Color red $ Scale 0.11 0.11 $ Text err
        
        
-- Event handler
handleEvent :: Store -> Input -> (Store, [Output])

--- Load Sudoku board
handleEvent store (KeyIn 'l')
    = (store {errorMsg=""}, [GraphPrompt ("Load code", "filename")])

--- Handle prompt
handleEvent store (Prompt ("Load code", filename))
    | filename /= "" = (store {errorMsg=""}, [ReadFile filename (TXTFile "")])
    | otherwise      = (store {errorMsg=""}, [])
  
--- Handle file load
handleEvent store (File filename (TXTFile input))
    | input /= "" = (store', [DrawPicture $ redraw store'])
    | otherwise   = (store,[])
    where
        store' = store {code=input, name=filename, errorMsg=""}
	
--- Unhandled event handler
handleEvent store input = (store, [])
        
-----------------------------------------------------------

-- Draw the screen and install the event handler
doShow fn = installEventHandler "Let rut!" handleEvent store startPic 10
    where
        store = initStore
        startPic = Pictures $ [redraw store]
		
main = doShow ""