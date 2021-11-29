{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Functions for outputting rows in CSV format.
module CSVOutput where

import Records (Row)
import Data.List (intercalate)
import Control.Monad (forM_)

-- | Convert a 'Row' of values to a comma separated values
-- representation with appropriate quoting for values that contain
-- quote marks, commas or newline characters. For example,
--
-- > ["Value 1","Value 2"]
--
-- Could generate the string:
--
-- > "Value 1,Value 2"
--
-- or (quoting every field):
--
-- > "\"Value 1\",\"Value 2\""
--
-- There will be no newline value at the end.
--
-- If a value in the row contains a quote, comma, or newline
-- character, then that value will be quoted in the final output.
--
-- Quote marks in quoted strings are escaped by doubling them. The
-- Haskell string @"hello \"world\"."@ becomes @"hello ""world""."@ in
-- the CSV output.
stringOfRow :: Row -> String
stringOfRow (row:rows)
  | rows /= [] = row++',':stringOfRow rows 
  | otherwise = row

-- HINT: the function 'intercalate' (imported above) may be useful.

-- | Prints a 'Row' to the standard output, using 'stringOfRow'.
printRow :: Row -> IO ()
printRow row = putStrLn (stringOfRow row)

printCSVFile :: [String] -> [Row] -> IO ()
printCSVFile header rows =
  do printRow header
     forM_ rows printRow
