module Turtle where

import Prelude
import FPPrac.Graphics
import Debug.Trace

data Turtle = Turtle 
    { x         :: Float
    , y         :: Float
    , pendown   :: Bool
    , col     :: Color
    , angle     :: Int  --Must be 0-2pi
    , actions   :: [Action]
    } deriving (Show, Eq)
    
data Action = Forward Float | Turn Int | PenUp | PenDown | ChangeColor Color | GoTo Float Float
    deriving (Show, Eq)


iniTurtle = Turtle { x          = 0
                   , y          = 0
                   , pendown    = False
                   , col      = black
                   , angle      = 90
                   , actions    = []
                   }

execTurtle t@(Turtle {actions=[]}) = []
execTurtle t@(Turtle {actions=((Forward dist):acs)}) = res ++ (execTurtle t')
    where
        Turtle {x=x,y=y,pendown=pendown,col=col,angle=angle} = t
        x' = x + dist * (cos $ degToRad (fromIntegral angle))
        y' = y + dist * (sin $ degToRad (fromIntegral angle))
        res = if pendown then [Color col $ Line [(x,y),(x',y')]] else []
        t' = t {x=x',y=y',actions=acs}

execTurtle t@(Turtle {actions=((GoTo x' y'):acs)}) = res ++ (execTurtle t')
    where
        Turtle {x=x,y=y,pendown=pendown,col=col} = t
        res = if pendown then [Color col $ Line [(x,y),(x',y')]] else []
        t' = t {x=x',y=y',actions=acs}
        
-- @require dAng >= 0
execTurtle t@(Turtle {actions=((Turn dAng):acs)}) = execTurtle t'
    where
        Turtle {angle=angle} = t
        a' = ((angle+(dAng)) `mod` (360))
        t' = t {angle=a',actions=acs}
        
execTurtle t@(Turtle {actions=((PenUp):acs)})         = execTurtle $ t {pendown=False,actions=acs}
execTurtle t@(Turtle {actions=((PenDown):acs)})       = execTurtle $ t {pendown=True,actions=acs}
execTurtle t@(Turtle {actions=((ChangeColor c):acs)}) = execTurtle $ t {col=c,actions=acs}        
        
degToRad deg = deg * pi / 180