module Display where 

import qualified Graphics.DrawingCombinators as Draw



main = do
    Draw.init
    Draw.runDrawing $ Draw.point (0.0, 0.0)