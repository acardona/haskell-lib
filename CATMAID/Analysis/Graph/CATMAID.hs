module Analysis.Graph.CATMAID where

import qualified Data.Text as T -- proper unicode handling
import Data.Packed.Matrix (Matrix, fromLists)


-- Parse CATMAID's neural circuit adjacency matrix CSV file, consisting in
-- a header row of:
--     "neuron", "neuron_name 1", "neuron_name 2", ...
-- and then a row for the synapses of a neuron with all other neurons:
--     "neuron_name 1", 0, 10, 2, 4, 0, 1, 0, 23, ...
-- 
-- NOTE: currently fragile, can't parse when neuron names contain ',' (comma) characters.
-- TODO: should use the Cassava Csv parsing library instead.
parse :: T.Text -> ([String], Matrix Double)
parse csv = (trimmedLabels, matrix)
    where labels:rows = map (  map T.unpack
                             . drop 1
                             . T.split (== ','))
                            $ T.lines csv
          -- remove the starting and ending quotes, and crop at one char before the sharp
          trimmedLabels = map (init . takeWhile ('#' /=) . drop 1) labels
          matrix = fromLists $ map (map read) rows

