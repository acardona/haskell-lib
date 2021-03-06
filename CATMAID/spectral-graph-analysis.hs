import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Analysis.Graph.SpectralGraphAnalysis (run)
import qualified Analysis.Graph.Plot as P
import qualified Analysis.Graph.CATMAID as CM

main = do
    [mode, path] <- getArgs
    csv <- TIO.readFile path
    -- putStrLn $ show $ run $ snd $ parse csv
    let (labels, adjM) = CM.parse csv
        vs = run adjM
        (_, signalFlow) = vs !! 0
        (firstEigenValue, firstEigenVector) = vs !! 1
        graph = P.create firstEigenVector
                         signalFlow
                         labels
                         "Spectral Graph Analysis"
                         (printf "1st eigenvalue (%.2f)" firstEigenValue)
                         "Signal flow"
      in case mode of
        "view" -> P.view graph
        "PDF"  -> P.write "graph.pdf" (512, 512) graph
        "SVG"  -> P.writeSVG "graph.svg" (512, 512) graph
        _      -> putStrLn "Run like:\n./spectral-graph-analysis <view|SVG|PDF> </path/to/adjacency-matrix.csv>"

