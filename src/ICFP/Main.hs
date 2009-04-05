module Main where 

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
import Common
import RNetwork
import Control.Concurrent
--import System.Concurrent
import Map

resX = 480
resY = 480

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()


main :: IO ()
main = do
    initScreen

    SDL.glSwapBuffers
    forkIO $ connect 
    waitClicks
 
    SDL.waitEvent
    
    SDL.quit
    return ()

    where
    waitClicks = do
        ev <- SDL.waitEvent
        case ev of
             SDL.Quit -> return ()
             _ -> waitClicks
