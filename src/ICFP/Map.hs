{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Map where
import Common
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))    
import Control.Applicative
import Data.Monoid
import Control.Monad (ap, MonadPlus(..))
import Test.HUnit
import Debug.Trace

import qualified Graphics.DrawingCombinators as Draw

data WorldMap = WorldMap {
      boulders :: [Boulder],
      craters :: [Crater],
      martians :: [Martian],
      size :: (Double, Double),
      vehicle :: VehiclePos 
    } 

instance Drawable WorldMap where 
    draw m = 
        scale $ mconcat $ 
             ((map draw $ boulders m) ++ 
              (map draw $ craters m) ++  
              (map draw $ martians m) ++
              [home, draw $ vehicle m]
             ) 
        where 
          (w, h) = size m
          max = vMaxSensor $ vehicle m
          loc = vLoc $ vehicle m 
          scale = Draw.scale (1.0/max) (1.0/max) 
                  . Draw.translate (-(x loc), -(y loc)) 
          home = circ (Location 0.0 0.0) homeRad (0, 0, 1.0, 0) 
              
rovRad = 4 -- fixed rover radius

data VehiclePos = VehiclePos {
      vLoc :: Location,
      vDir :: Double,
      vMinSensor :: Double,
      vMaxSensor :: Double
}

instance Drawable VehiclePos where 
    draw pos = mappend
               (circLine  (vLoc pos) rovRad (1.0,1.0,0.0,0.0) (vDir pos))
               (ellipsis ((vMaxSensor pos) + 0.5) ((vMinSensor pos) + 0.5) (vDir pos) (vLoc pos))

data Martian = Martian {
      loc :: Location, 
      enemy_dir :: Double,
      enemy_speed :: Double
} deriving (Show, Eq) 
martianRadius = 0.4

instance Parsable Martian where 
    parser = pure Martian <*> p <*> sp <*> sp  

instance Drawable Martian where 
    draw mart = circLine (loc mart) martianRadius (1.0, 0, 1.0, 0) (enemy_dir mart) 
           
data Boulder = Boulder Location Double
             deriving (Eq, Show)
instance Parsable Boulder where 
    parser = pure Boulder <*> p <*> sp 

instance Drawable Boulder where 
    draw (Boulder l r) = circ l r (1.0, 0, 0, 0) 

data Crater = Crater Location Double
            deriving (Eq, Show)
instance Parsable Crater where 
    parser = pure Crater <*> p <*> sp 

instance Drawable Crater where 
    draw (Crater l r) = circ l r (0, 1.0, 0, 0)


homeRad = 5.0 -- fixed home radius
data Home = Home Location Double
            deriving (Eq, Show)
instance Parsable Home where 
    parser = pure Home <*> p <*> sp 





instance Parsable Location where 
    parser = trace "location" $ pure Location <*> p <*> sp 

tests = []

sampleMap = WorldMap {
              boulders = [Boulder (Location (-10.0) 15.0) 2.0],
              craters = [],
              martians = [],
              size = (20.0,20.0),
              vehicle = VehiclePos (Location 14.0 (17.8)) 5.0 5.0 10.0
}