module Turtle where

import Prelude
import FPPrac.Graphics
import Debug.Trace
import Graphics


data Turtle = Turtle 
    { x         :: Int
    , y         :: Int
    , pendown   :: Bool
    , color     :: Color
    , angle     :: Float  --Must be 0-2pi
    , actions   :: [Action]
    }
    
data Action = Forward Int | Turn Int | PenUp | PenDown | ChangeColor Color | GoTo Int Int


iniTurtle = Turtle { x          = 0
                   , y          = 0
                   , pendown    = False
                   , color      = black
                   , angle      = pi/2
                   , actions    = []
                   }

execTurtle t@(Turtle {actions=[]}) = []
execTurtle t@(Turtle {actions=((Forward dist):acs)}) = res ++ (execTurtle t')
    where
        Turtle {x=x,y=y,pendown=pendown,color=color,angle=angle} = t
        x' = x + dist * (cos angle)
        y' = y + dist * (sin angle)
        res = if pendown then [Color color $ Line [(x,y),(x',y')]] else []
        t' = t {x=x',y=y'}

execTurtle t@(Turtle {actions=((GoTo x' y'):acs)}) = res ++ (execTurtle t')
    where
        Turtle {x=x,y=y,pendown=pendown,color=color} = t
        res = if pendown then [Color color $ Line [(x,y),(x',y')]] else []
        t' = t {x=x',y=y'}
        
-- @require dAng >= 0
execTurtle t@(Turtle {actions=((Turn dAng):acs)}) = execTurtle t'
    where
        Turtle {angle=angle} = t
        a' = (angle+(degToRad dAng)) `mod` (2*pi)
        t' = t {angle=a'}
        
execTurtle t@(Turtle {actions=((PenUp):acs)})         = execTurtle $ t {pendown=False}
execTurtle t@(Turtle {actions=((PenDown):acs)})       = execTurtle $ t {pendown=True}
execTurtle t@(Turtle {actions=((ChangeColor c):acs)}) = execTurtle $ t {color=c}        
        
degToRad deg = deg * pi / 180