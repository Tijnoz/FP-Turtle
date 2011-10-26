module Turtle where

import Prelude
import FPPrac.Graphics
import Debug.Trace

data Turtle = Turtle 
    { x         :: Float
    , y         :: Float
    , pendown   :: Bool
    , col       :: Color
    , angle     :: Int  --Must be 0-2pi
    , actions   :: [Action]
    } deriving (Show, Eq)
    
data Action = Forward Float | Turn Int | PenUp | PenDown | ChangeColor Color | GoTo Float Float | NoOp | Break | Clear
    deriving (Show, Eq)


iniTurtle = Turtle { x          = 0
                   , y          = 0
                   , pendown    = False
                   , col        = black
                   , angle      = 90
                   , actions    = []
                   }
                   
                  
execTurtle :: Turtle -> [Picture]
execTurtle t@(Turtle {actions=[]})      = []
execTurtle t@(Turtle {actions=(a:acs)}) = p : (execTurtle t'')
    where
        (t', p) = execAction t a
        t'' = t' {actions=acs}

execAction :: Turtle -> Action -> (Turtle, Picture)
execAction t (Forward dist) = (t', res)
    where
        Turtle {x=x,y=y,pendown=pendown,col=col,angle=angle} = t
        x' = x + dist * (cos $ degToRad (fromIntegral angle))
        y' = y + dist * (sin $ degToRad (fromIntegral angle))
        res = if pendown then (Color col $ Line [(x,y),(x',y')]) else Blank
        t' = t {x=x',y=y'}

execAction t (GoTo x' y') = (t', res)
    where
        Turtle {x=x,y=y,pendown=pendown,col=col} = t
        res = if pendown then (Color col $ Line [(x,y),(x',y')]) else Blank
        t' = t {x=x',y=y'}
        
-- @require dAng >= 0
execAction t (Turn dAng) = (t', Blank)
    where
        Turtle {angle=angle} = t
        a' = ((angle+(dAng)) `mod` (360))
        t' = t {angle=a'}
        
execAction t (PenUp)         = (t {pendown=False}, Blank)
execAction t (PenDown)       = (t {pendown=True}, Blank)
execAction t (ChangeColor c) = (t {col=c}, Blank)   
execAction t (NoOp)          = (t, Blank)
execAction t (Break)         = (t, Blank)
execAction t (Clear)         = (t, Color white $ rectangleSolid 1000 1000)

degToRad deg = deg * pi / 180