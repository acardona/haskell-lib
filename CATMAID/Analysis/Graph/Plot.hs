module Analysis.Graph.Plot where

import Data.Packed.Vector (Vector, toList)
import Graphics.Rendering.Plot
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk hiding (Circle, Cross)
import Data.Colour.Names (red, lightgray)
import Control.Monad.Trans


-- Create a Figure containing a Plot of two Vector vx, vy
-- For documentation see:
-- http://hackage.haskell.org/package/plot-0.2.3/docs/Graphics-Rendering-Plot-Figure.html
create :: Vector Double -> Vector Double -> [String]
       -> String -> String -> String
       -> Figure ()
create vx vy labels title xLabel yLabel =  do
    withTitle $ setText title
    withPointDefaults $ setPointSize 1.0
    setPlots 1 1
    withPlot (1, 1) $ do
        --setLegend True SouthWest Inside
        addAxis XAxis (Side Lower) $ withAxisLabel $ setText xLabel
        addAxis YAxis (Side Lower) $ do
            withAxisLabel $ setText yLabel
            withGridLine Major $ do
                setDashStyle [Dash]
                setLineColour lightgray
        --setRange XAxis Lower Linear (min vx) (max vx)
        --setRange YAxis Lower Linear (min vy) (max vy)
        setDataset (vx, [point vy (Bullet, red)]) -- Circle, Box, Diamond, Asterisk, Triangle, Bullet, Top, Bot
        let annotation (x, y, label) = text (x, -y)
                                            (do
                                             setText label
                                             setFontSize 8)
            in withAnnotations $ do mapM_ annotation $ zip3 (toList vx) (toList vy) labels
        setRangeFromData XAxis Lower Linear
        setRangeFromData YAxis Lower Linear


-- Show a Plot instance in a window
view graph = display $ render graph

-- Write a plot to a file as PDF
write filepath graph = writeFigure PDF filepath graph


-- Create a window and show in it a renderable element like a Plot
-- Mostly copied from https://github.com/amcphail/plot/blob/master/examples/Test4.hs
display :: ((Int, Int) -> C.Render ()) -> IO ()
display r = do
    initGUI
    window <- windowNew
    set window [ windowTitle := "Plot"
               , windowDefaultWidth := 600
               , windowDefaultHeight := 400
               , containerBorderWidth := 1
               ]
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
    widgetShowAll window
    on canvas exposeEvent $ tryEvent $ do s <- liftIO $ widgetGetSize canvas
                                          drw <- liftIO $ widgetGetDrawWindow canvas
                                          liftIO $ renderWithDrawable drw (r s)
    onDestroy window mainQuit
    mainGUI

