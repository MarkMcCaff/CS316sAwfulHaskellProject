-- | Implements 'Record's, looking up fields in them, and their
-- conversion to and from rows.
module Records where

import Result (Result (..))

-- | A row is a list of strings, one for each field. For example:
--
-- > ["Mount Snowden", "Wales"]
type Row = [String]

-- | A record is a list of fieldname / value pairs. For example:
--
-- > [("Mountain", "Mont Blanc"), ("Country", "France")]
type Record = [(String,String)]

-- | Look up a field in a record, returning @Error@ if the field is
-- not in the record. For example,
-- > lookupField "a" [("a","1"),("b","2")]
-- returns @Ok "1"@, but
-- > lookupField "c" [("a","1"),("b","3")]
-- returns @Error "Field 'c' not present."@.
lookupField :: String -> Record -> Result String
lookupField fieldname []= Error "lookupField: not implemented"
lookupField fieldname ((x,y):records) = 
    if fieldname == x then
      Ok y
    else
      lookupField fieldname records

-- | Given a header listing field names, like:
--
-- >  ["Mountain", "Country"]
--
-- and a row like:
--
-- >   ["Ben Nevis", "Scotland"]
--
-- turn it into a record like:
--
-- >   [("Mountain", "Ben Nevis"), ("Country", "Scotland")]
--
-- If the number of field names in the header does not match the
-- number of fields in the row, an @Error@ should be returned.
rowToRecord :: [String] -> Row -> Result Record
rowToRecord header row =
  Error "rowToRecord: not implemented"

-- | Given a header listing field names, and a list of rows, converts
-- each row into a record. See 'rowToRecord' for how individual rows
-- are converted to records.
rowsToRecords :: [String] -> [Row] -> Result [Record]
rowsToRecords header rows =
  Error "rowsToRecord: not implemented"

-- | Given a header listing field names, like:
--
-- >   ["Mountain", "Country"]
--
-- and a record like:
--
-- >   [("Mountain", "Ben Nevis"), ("Country", "Scotland")]
--
-- turn it into a row like:
--
-- >   ["Ben Nevis", "Scotland"]
--
-- It does not matter what order the fields in the record are in, so the
-- record:
--
-- >   [("Country", "Scotland"), ("Mountain", "Ben Nevis")]
--
-- should result in the same row.
--
-- This function returns an @Error@ if any of the field names listed in
-- the header are not in the record.
recordToRow :: [String] -> Record -> Result Row
recordToRow header record =
  Error "recordToRow: not implemented"

-- | Given a header listing field names, and a list of records,
-- converts each record into a row. See 'recordToRow' for how
-- individual records are converted to rows.
recordsToRows :: [String] -> [Record] -> Result [Row]
recordsToRows header records =
  Error "recordsToRows: not implemented"
