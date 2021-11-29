{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Implementation of a Query Language for filtering lists of records.
module QueryLanguage where

import Control.Monad (filterM)
import Result (Result (..))
import Records

-- | A query is a boolean combinations of 'Condition's. 'Query's are
-- evaluated against 'Record's by the 'evalQuery' function.
data Query
  = Condition Condition     -- ^ A 'Condition' on a record
  | And       Query Query   -- ^ "And" combination of two 'Query's
  | Or        Query Query   -- ^ "Or" combination of two 'Query's
  | Not       Query         -- ^ Negation of a 'Query'
  | AlwaysTrue              -- ^ Query that is always true
  | AlwaysFalse             -- ^ Query that is always false
  deriving (Eq, Show)

-- | Conditions are comparisons between fields and values, or between
-- two fields. 'Condition's are evaluated against 'Record's by the
-- 'evalCondition' function.
data Condition
  = Equal    FieldOrLiteral FieldOrLiteral
  deriving (Eq, Show)

-- | 'FieldOrLiteral' represent either field lookups in a record, or
-- literal strings. 'FieldOrLiteral's are evaluated against 'Record's
-- by the 'evalFieldOrLiteral' function.
data FieldOrLiteral
  = Field   String
  | Literal String
  deriving (Eq, Show)

-- | Evaluate a 'FieldOrLiteral' against a record. For @Field
-- fieldname@, look that field up in the record and it (if it
-- exists). For @Literal s@, just return the string @s@.
--
-- Examples:
--
-- > evalFieldOrLiteral (Field "Country") [("Country", "S"),("Name","Ben Nevis")] == Ok "S"
--
-- > evalFieldOrLiteral (Field "Country") [("Name", "Ben Nevis")] == Error ...
--
-- > evalFieldOrLiteral (Literal "E") [("Country", "S"),("Name","Ben Nevis")] == Ok "E"
evalFieldOrLiteral :: FieldOrLiteral -> Record -> Result String
evalFieldOrLiteral (Field _) [] = Error "Not Present"
evalFieldOrLiteral (Literal a) _ = Ok a
evalFieldOrLiteral (Field a) ((x,y):records)
  | a == x = Ok y
  | otherwise = evalFieldOrLiteral (Field a) records



-- | Evaluate a 'Condition' against a record. For @Equal e1 e2@, uses
-- 'evalFieldOrLiteral' to evaluate @e1@ and @e2@. If they both
-- succeed, then compares the results to check for equality. Any
-- errors arising from evaluation are returned to the caller.
evalCondition :: Condition -> Record -> Result Bool
evalCondition (Equal x y) record =
  if evalFieldOrLiteral x record /= Error "Not Present" && evalFieldOrLiteral y record /= Error "Not Present" then
    Ok (evalFieldOrLiteral x record == evalFieldOrLiteral y record)
  else
    Error "Both Conditions not equal"



-- | Evaluate a 'Query' against a record. For @Condition c@, uses
-- 'evalCondition'. For the other forms of query, it evaluates the
-- sub-queries recursively and combines the results. Any errors that
-- occur during evaluation are returned to the caller.
evalQuery :: Query -> Record -> Result Bool
evalQuery query [] = Error "Record is empty"
evalQuery _ record = Error "Query is empty"
evalQuery AlwaysTrue       _    = Ok True
evalQuery AlwaysFalse      _    = Ok False
evalQuery (Condition x) records = evalCondition x records
evalQuery (And x y)     records = 
  Ok ((evalQuery x records /= Error "Both Conditions not equal") && (evalQuery y records /= Error "Both Conditions not equal"))
evalQuery (Or  x y)     records = 
  Ok ((evalQuery x records /= Error "Both Conditions not equal") || (evalQuery y records /= Error "Both Conditions not equal"))
evalQuery (Not x  )     records = 
  Ok (evalQuery x records  == Error "Both Conditions not equal")

-- | Evaluate a 'Query' against every 'Record' in a list, returning
-- only those records for which the query says 'True'.
filterByQuery :: Query -> [Record] -> Result [Record]
filterByQuery query records = Error "filterByQuery: not implemented"
