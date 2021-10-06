module Main where

import System.Environment (getArgs)

import CSVInput
import CSVOutput
import QueryLanguage
import Records
import Result

-- | A query to find all records where the @Country@ field is @"S"@
-- (for "Scotland"). See the file @data/Countries.csv@ for a list of
-- the possible countries.
query :: Query
query = Condition (Equal (Field "Country") (Literal "S"))

-- | The @main@ function where execution starts from.
main :: IO ()
main =
  do -- Get the CSV filename to read from the input.
     --
     -- FIXME: This is not robust. Can you alter it so that it reports
     -- a user friendly error if the filename is not present? What if
     -- we want to include additional command line options?
     [filename] <- getArgs

     -- Read the raw data in from the file given.
     --
     -- FIXME: What if the user wants to query several CSV files and
     -- join them?
     rawText <- readFile filename

     -- Parse the file into separate header and rows.
     (header, rows) <- abortOnError (parseCSV rawText)

     -- Convert each of the rows into records, ready for filtering.
     records <- abortOnError (rowsToRecords header rows)

     -- Filter the records using a query.
     --
     -- FIXME: This works with a fixed query defined above. To change
     -- the query the user must change the code above and then
     -- recompile the program. Can you alter the program so the user
     -- can provide the query on the command line?
     filteredRecords <- abortOnError (filterByQuery query records)

     -- Convert the filtered records into rows, ready for printing.
     outputRows <- abortOnError (recordsToRows header filteredRecords)

     -- Print the filtered CSV file to the standard output.
     --
     -- FIXME: What if the user wants to output to a file? What if
     -- they don't want all the fields in the original file? What if
     -- they want a file format other than CSV?
     printCSVFile header outputRows
