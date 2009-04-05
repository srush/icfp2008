module Common where

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Data.Monoid
import Control.Monad (ap, MonadPlus(..))
import Numeric
import qualified Graphics.DrawingCombinators as Draw
import Debug.Trace

class Parsable a where 
    parser :: Parser a

class Drawable a where 
    draw :: a -> Draw.Draw ()


-- Parser with a space
sp :: (Parsable a) => Parser a
sp = char ' ' *> parser

-- Parser without a space
p :: (Parsable a) => Parser a
p = parser

instance Parsable Double where 
    parser = 
        do s <- getInput
           case readSigned readFloat s of
             [(n, s')] -> n <$ setInput s'
             _         -> empty

data Location = Location {
      x :: Double,
      y :: Double   
} deriving (Show, Eq) 

deg2rad = (*) ((pi) / 180.0)

circLine :: Location -> Double -> Draw.Color -> Double -> Draw.Draw ()
circLine l rad col ang = 
    Draw.translate (x l, y l)
  $ Draw.scale rad rad
  $ mappend 
        (Draw.color (0.0, 0.0, 0.0, 0.0) $ 
             Draw.rotate (deg2rad ang)
             $ Draw.scale 5.0 5.0
             $ Draw.line (0.0, 0.0) (1.0, 0.0))
        (Draw.color col Draw.circle)

circ :: Location -> Double -> Draw.Color -> Draw.Draw ()
circ l rad col = 
    Draw.translate (x l, y l)
  $ Draw.scale rad rad
  $ Draw.color col
  $ Draw.circle

ellipsis max min ang l =
    Draw.translate (x l, y l)
  $ Draw.rotate (deg2rad ang)
  $ Draw.translate (trace (show focus) focus, 0.0)
  $ Draw.scale ((max + min)/2.0)  ((minor max min) /2.0)
  $ Draw.color (1.0,1.0,1.0, 0.0)
  $ Draw.circle
      where focus = (max - min) / 2

minor max min = (min + max) * (sqrt(1- (((max - min) / (2 * (min + max)))**2))) 
focusOffset max min = max * eccentricity max min 
eccentricity max min = sqrt (1 - ((min/max) ** 2))



