module Tables
    (RoguestarTable(..),
     tableSelect,
     tableSelect1,
     tableSelect2,
     tableSelect2Integer,
     tableSelect3Integer,
     tableLookup,
     tableLookupInteger)
    where

import Data.List
import Data.Maybe

-- |
-- This is a simple implementation of a relational data table, and is always used to represent information
-- that has been sent to us from roguestar-engine.
--
data RoguestarTable = RoguestarTable { table_name, table_id :: String, table_header :: [String], table_data :: [[String]] }
		      deriving (Eq,Show)

-- |
-- Select from a table, like the SQL select statement.
-- For example:
-- tableSelect people ["name","sex","phone-number"] = [["bob","male","123-4567"],["susan","female","987-6543"]]
-- If a given header is not in the table, lists "???" as the value.
--
tableSelect :: RoguestarTable -> [String] -> [[String]]
tableSelect table headers = let header_indices = map (\x -> elemIndex x $ table_header table) headers
				in map (rowSelect header_indices) $ table_data table

rowSelect :: [Maybe Int] -> [String] -> [String]
rowSelect (Nothing:more) row = "???" : rowSelect more row
rowSelect (Just x:more) row = (row !! x) : rowSelect more row
rowSelect [] _ = []

-- |
-- As tableSelect, but select a single header.
--
tableSelect1 :: RoguestarTable -> String -> [String]
tableSelect1 table header = map head $ tableSelect table [header]

-- |
-- As tableSelect, but select exactly two headers as a pair.
--
tableSelect2 :: RoguestarTable -> (String,String) -> [(String,String)]
tableSelect2 table headers = map (\x -> (x !! 0,x !! 1)) $ tableSelect table [fst headers,snd headers]

-- |
-- As tableSelect2, converting the second element into a Maybe Integer.
--
tableSelect2Integer :: RoguestarTable -> (String,String) -> [(String,Maybe Integer)]
tableSelect2Integer table headers = map (\x -> (fst x,readInteger $ snd x)) $ tableSelect2 table headers

-- |
-- As tableSelect2Integer, but with the second and third elements converted to integers.
--
tableSelect3Integer :: RoguestarTable -> (String,String,String) -> [(String,(Maybe Integer,Maybe Integer))]
tableSelect3Integer table (h1,h2,h3) = map ( \ x -> (x !! 0,(readInteger $ x !! 1,readInteger $ x !! 2))) $ tableSelect table [h1,h2,h3]

-- |
-- tableLookup table ("name","phone-number") "bob" = bob's phone number, or nothing if "bob" isn't in the table.
--
tableLookup :: RoguestarTable -> (String,String) -> String -> Maybe String
tableLookup table headers value = lookup value $ tableSelect2 table headers

-- |
-- As tableLookup, but answers an integer.
--
tableLookupInteger :: RoguestarTable -> (String,String) -> String -> Maybe Integer
tableLookupInteger table headers value = fromMaybe Nothing $ lookup value $ tableSelect2Integer table headers

readInteger :: String -> Maybe Integer
readInteger = listToMaybe . map fst . reads