{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module RNetwork where
-- Section 3  - Network Protocol


import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Common
import Controller hiding (tests)
import Map hiding (tests)
import Test.HUnit
import Debug.Trace
import System.IO
import qualified Network as N
import Data.Maybe
newtype TimeStamp = TimeStamp Integer deriving (Eq, Show, Num)
instance Parsable TimeStamp where 
    parser =     
        do s <- getInput
           case reads s of
             [(n, s')] ->  TimeStamp n <$ setInput s'
             _         -> empty


data Init = 
    Init {
      dx :: Double,
      dy :: Double,
      time_limit :: TimeStamp, 
      min_sensors :: Double, 
      max_sensors :: Double, 
      max_speed :: Double, 
      max_turn :: Double, 
      max_hard_turn :: Double 
    } deriving (Show, Eq)

instance Parsable Init where 
    parser = char 'I' *>
             pure Init <*> (sp)
                      <*> (sp)
                      <*> (sp)
                      <*> (sp)
                      <*> (sp)
                      <*> (sp)
                      <*> (sp)
                      <*> (sp)

data Object = M Martian | 
              B Boulder |
              C Crater | 
              H Home
                   deriving (Show, Eq) 

instance Parsable Object where 
    parser = 
        (char 'b' *> pure B <*> sp <|>
        char 'c' *> pure C <*> sp <|>
        char 'h' *> pure H <*> sp <|>
        char 'm' *> pure M <*> sp)



data Died = BoulderCrash | CraterCrash | Killed 
            deriving (Show, Eq)

instance Parsable Died where 
    parser = 
        BoulderCrash <$ char 'B' <|>
        CraterCrash <$ char 'C' <|>
        Killed <$ char 'K'

data Adverse = Adverse Died TimeStamp 
             deriving (Show, Eq)

instance Parsable Adverse where 
    parser = pure Adverse <*> p <*> sp

data Success = Success Double
               deriving (Show, Eq)
data EndOfRun = EndOfRun Double Double
              deriving (Show, Eq)
data Message = I Init | T Telemetry | A Adverse | S Success | E EndOfRun
             deriving (Show, Eq)

instance Parsable Message where 
    parser = 
        (pure I <*> p <|>
        pure T <*> p) <* (char ' ' <|> return ' ');
        

data Telemetry = 
    Telemetry {
      time_stamp :: TimeStamp,
      vehicle_state :: VehicleState,
      vehicle_loc  :: Location,
      vehicle_dir  :: Double,
      vehicle_speed  :: Double,
      objects :: [Object]
} deriving (Show, Eq) 

instance Parsable Telemetry where 
    parser = char 'T' *>
             pure Telemetry <*> sp -- time_stamp
             <*> sp 
             <*> sp
             <*> sp
             <*> sp
             <*> (many $ try sp)


-- oh god, this is terrible, stolen from hGetNewLine
hGetSemi h = do
  c <- hGetChar h
  if c == ';' then
    return ""
   else do
    l <- getRest
    return (c:l)
 where
  getRest = do
    c <- 
      catch 
        (hGetChar h)
        (\ err -> do
             ioError err)
    if c == ';' then
       return ""
     else do
       s <- getRest
       return (c:s)

tele2map tele (Just init) =
    WorldMap {
  boulders = filterMap isBoulder objs ,
  craters = filterMap isCrater objs ,
  martians = filterMap isMartian objs,
  size = (dx init, dy init),
  vehicle = VehiclePos {
              vLoc = vehicle_loc tele,
              vDir = vehicle_dir tele,
              vMinSensor = min_sensors init,
              vMaxSensor = max_sensors init         
            }
}
   where objs = objects tele
         isBoulder (B b) = Just b
         isBoulder _ = Nothing
         isCrater (C c) = Just c
         isCrater _ = Nothing
         isMartian (M m) = Just m
         isMartian _ = Nothing


connect = do
  handle <- N.connectTo "localhost" (N.PortNumber 17676)
  hSetBuffering handle NoBuffering
  getLoop handle Nothing
      where 
        getLoop handle init = do 
            contents <- hGetSemi handle
            let message = (parse parser "" contents)
            --print $ either (\e -> contents ++ show e) 
            --          (\(a::Message) -> show a) message 
            init' <- case message of 
              Right (T tele) -> do  
    
                  let wmap = tele2map tele init
                  Draw.draw $ draw wmap
                  SDL.glSwapBuffers
                  mapM_ (hPutStr handle) $ map show $ controller wmap    
                  return init
              Right (I init') -> 
                  return (Just init') 
              _ -> return init
            getLoop handle init'

filterMap :: (a->Maybe b) -> [a] -> [b]
filterMap f = map (\(Just a) -> a) 
              . filter isJust 
              . map f
------------------------------------------
------------------------------------------
testParse (str,correct)  = 
    TestCase $
             either (assertFailure . show)
                    (assertEqual "init comp" correct) 
                    (parse parser "" str)

testInitData = ("I -23.23 73.45 1234 23.32 56.34 100.56 20.3 25.69",
    Init {
  dx = -23.23,
  dy = 73.45,
  time_limit = 1234,
  min_sensors = 23.32,
  max_sensors = 56.34,
  max_speed = 100.56,
  max_turn = 20.3,
  max_hard_turn = 25.69})

testInit = testParse testInitData

testTeleData = 
    ("T 123 bR 123.2 321.3 56.34 100.56  "
    , Telemetry {
       time_stamp = 123,
       vehicle_state = 
           VehicleState { vMoveState = Braking,
                          vDirState = MHardRight},
       vehicle_loc = Location 123.2 321.3,
       vehicle_dir = 56.34,
       vehicle_speed = 100.56,
       objects = []})


testTelemetry = testParse testTeleData


testObjectStr = ("b -220.000 750.00 12.000",
                 B (Boulder (Location (-220.0) 750.0) 12.0))
testObjectStr2 = ("m  -240.00 812.00 90.0 9.100",
                  M (Martian (Location (-240.00) 812.00) 90.0 9.100)) 
testObjects = 
    [testParse  testObjectStr,
     testParse  testObjectStr2]

testAdverseData = ("B 123", Adverse BoulderCrash 123)
testAdverse = testParse  testAdverseData

testMessageData = (mess, I dat)
    where (mess, dat) = testInitData
    
testMessageData2 = (mess, T dat)
    where (mess, dat) = testTeleData

testMessData3 = ("T 0 -- -25.000 25.000 50.0 0.000 h 0.000 0.000 5.000 b -53.125 53.125 0.608 b -28.125 78.125 0.654 b -28.125 65.625 0.677 b -26.562 60.938 0.596 b -34.375 53.125 0.930 b -28.125 53.125 0.660 b -18.750 68.750 1.490 b -21.875 53.125 0.778 b -4.688 54.688 0.490 b -1.562 51.562 0.440 "
                 , T Telemetry {
                           time_stamp = 0,
                           vehicle_state = 
                           VehicleState { vMoveState = Roll,
                                          vDirState = MStraight},
                           vehicle_loc = Location (-25.0) 25.0,
                           vehicle_dir = 50.0,
                           vehicle_speed = 0.0,
                           objects = [H (Home (Location {x = 0.0, y = 0.0}) 5.0),
                                      B (Boulder (Location {x = (-53.125), y = 53.125}) 0.608),
                                      B (Boulder (Location {x = (-28.125), y = 78.125}) 0.654),
                                      B (Boulder (Location {x = (-28.125), y = 65.625}) 0.677),
                                      B (Boulder (Location {x = (-26.562), y = 60.938}) 0.596),
                                      B (Boulder (Location {x = (-34.375), y = 53.125}) 0.93),B (Boulder (Location {x = (-28.125), y = 53.125}) 0.66),
                                      B (Boulder (Location {x = (-18.75), y = 68.75}) 1.49),
                                      B (Boulder (Location {x = (-21.875), y = 53.125}) 0.778),
                                      B (Boulder (Location {x = (-4.688), y = 54.688}) 0.49),
                                      B (Boulder (Location {x = (-1.562), y = 51.562}) 0.44)]})


testMessages = 
    [testParse testMessageData,
     testParse testMessageData2,
     testParse testMessData3]

tests = TestList $ [testInit, testTelemetry, testAdverse] ++ testObjects ++ testMessages


