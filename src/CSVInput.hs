-- | Parsing of CSV files from Strings.
module CSVInput where

import Result (Result)
import ParserCombinators ( Parser, satisfies, spaces, quotedString, sepBy
                         , newline, zeroOrMore, isChar, completeParse, orElse, char, failParse)
import Records (Row)

-- | Parser for characters that can appear in unquoted CSV fields.
csvFieldChar :: Parser Char
csvFieldChar =
  satisfies "csv field character" (\c -> c /= ',' && c /= '\n' && c /= '\r')

csvQuotedChar :: Parser Char
csvQuotedChar =
  do c <- char
     case c of
       '"' -> (do isChar '"'; return '"') `orElse` failParse ""
       c   -> return c

csvQuotedString :: Parser String
csvQuotedString =
  do isChar '"'
     str <- zeroOrMore csvQuotedChar
     isChar '"'
     return str

-- | Parser for CSV fields: either a quoted string (surrounded by
-- spaces), or an unquoted field consisting of 'csvFielChar'
-- characters.
field :: Parser String
field = do spaces; s <- csvQuotedString; spaces; return s
        `orElse`
        zeroOrMore csvFieldChar

-- | Parser for CSV rows: zero or more fields separated by commas,
-- followed by a newline ('\n' for UNIX-like, '\r\n' for Windows).
row :: Parser Row
row =
  do fields <- sepBy (isChar ',') field
     newline
     return fields

-- | Parser for CSV files with headers. The first line is a header
-- giving the field names. The rest of the lines are the rows of
-- fields.
csvFile :: Parser ([String], [Row])
csvFile =
  do header <- row
     rows <- zeroOrMore row
     return (header, rows)

-- | Attempt to parse a CSV file (i.e. a header with field names, and
-- a list of 'Row's) from a String.
parseCSV :: String -> Result ([String], [Row])
parseCSV = completeParse csvFile
