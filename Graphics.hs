module Graphics where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import System.FilePath (splitPath, dropExtension)
import Data.List
import Debug.Trace

data Process = EnteringValue | DoingNothing deriving (Eq, Show)

data Store = Store
  { board             :: Board
  , turtle            :: Turtle
  , errorMsg          :: String
  }

boardTop = 250
boardLeft = (-230)
squareTop y = (y * (-50) + boardTop)
squareLeft x = (x * 50 + boardLeft)

initStore ah = Store { board = emptyBoard
                     , name = ""
                     , errorMsg = ""
                     }


-- Draws the bottom line
bottomLineHeight = 25
bottomTextHeight = 10
        
drawBottomLine :: Store -> Picture
drawBottomLine store 
    = Pictures 
      [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
      , Color black $ Line [(-400,height1),(400,height1)] -- top
      , Color black $ Line [(-300,height1),(-300,-300)] -- left
      , Color black $ Line [(145,height1),(145,-300)] -- right
      , Translate (-394) height2 $ Color blue  $ Scale 0.11 0.11 $ Text $ (name store)
      , Translate (-290) height2 $ Color black $ Scale 0.09 0.09 $ Text "[n]ew [s]ave (a[S]) [l]oad s[o]lve [h]int [x]sudoku op[t]ions [b]acktracking"
      , Translate 155 height2 $ Color red   $ Scale 0.11 0.11 $ Text (errorMsg store)
      ]
    where
        height1 = -300 + bottomLineHeight
        height2 = -300 + bottomTextHeight
        
        
-- Event handler
handleEvent :: Store -> Input -> (Store, [Output])

--- Load Sudoku board
handleEvent store (KeyIn 'l')
    = (store {process=DoingNothing, errorMsg=""}, [GraphPrompt ("Load game", "filename")])

--- Actual loading handled by Sudoku Handler
handleEvent store (Prompt ("Load game", filename))
    | filename /= "" = (store {process=DoingNothing, errorMsg=""}, [ReadFile filename (TXTFile "")])
    | otherwise      = (store {process=DoingNothing, errorMsg=""}, [])

--- Unhandled event handler
handleEvent store input = (store, [])
        
-----------------------------------------------------------

-- Draw the screen and install the event handler
doShow ah = installEventHandler "Let rut!" handleEvent store startPic 10
    where
        store = initStore ah
        Store {board=board} = store
        startPic = Pictures
          [ drawBoard False board
          , drawBottomLine store
          ]
