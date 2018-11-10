module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (class Foldable, and, length, intercalate)
import Data.Array (zipWith, zip, filter, insertAt, updateAt, deleteAt)
import Data.Maybe (Maybe(..), fromJust)
import Data.Enum (enumFromTo)
import Data.Tuple (fst, snd)
import Partial.Unsafe(unsafePartial)

main :: Effect Unit
main = do
  log "Hello sailor!"

--------------------------------------------------------------------------------
--Data Structures

newtype DB = DB {schema :: DBSchema, data :: DBData}

type DBData = Array DBRow

type DBSchema = Array DBColumn
type DBRow = Array DBValue
-- newtype DBSchema = DBSchema {pkCol :: DBColumn, columns :: Array DBColumn}
-- newtype DBRow = DBRow {pkVal :: DBValue, values :: Array DBValue}

--newtype DBColumn = DBColumn {name :: String, type :: DBType, nullable :: Boolean, unique :: Boolean}
newtype DBColumn = DBColumn {name :: String, type :: DBType, nullable :: Boolean}
data DBType = IntT | DoubleT | StringT | BooleanT
data DBValue = IntV Int | DoubleV Number | StringV String | BooleanV Boolean | Null

--------------------------------------------------------------------------------
--Typeclass Instances

instance showDB :: Show DB where
  show (DB db) = "\nschema :\n" <> prettyFunctor db.schema <> "\n\n" <> "data :\n" <> prettyFunctor db.data

prettyFunctor :: forall f a. Foldable f => Functor f => Show a => f a -> String
prettyFunctor = intercalate "\n" <<< map show

instance showDBColumn :: Show DBColumn where
  show (DBColumn c) = "{name : " <> c.name <> ", type : " <> show c.type <> ", nullable : " <> show c.nullable <> "}"

instance showDBType :: Show DBType where
  show t = case t of
    IntT -> "IntT"
    DoubleT -> "DoubleT"
    StringT -> "StringT"
    BooleanT -> "BooleanT"

instance showDBValue :: Show DBValue where
  show v = case v of
    (IntV i) -> show i
    (DoubleV d) -> show d
    (StringV s) -> show s
    (BooleanV b) -> show b
    Null -> "null"

derive instance eqDBType :: Eq DBType

derive instance eqDBValue :: Eq DBValue

--------------------------------------------------------------------------------
--Consistency Checking

--Consistency checks
--1. Check that all DBRows are the same length as DBSchema
--2. Check that all DBValues conform to the DBType assigned to them by their DBColumn
--3. Check that any Null DBValue is in a DBColumn where nullable = True

--(ones below haven't been implemented yet)
--4. Check that in each column where unique = True, values are unique
--5. Check that pk DBColumn has nullable = false and unique = true

failedRows :: DB -> Array Int --returns 1-indexed list of DBRows which fail the consistencyCheck
failedRows (DB db) = map fst $ filter f $ zip indecies $ map (checkRow db.schema) db.data
  where indecies = enumFromTo 1 (length db.data)
        f x = snd x == false

checkRow :: DBSchema -> DBRow -> Boolean
checkRow s r = lengthCheckRow s r && typeCheckRow s r

lengthCheckRow :: DBSchema -> DBRow -> Boolean
lengthCheckRow s r = length s == (length r :: Int)

typeCheckRow :: DBSchema -> DBRow -> Boolean
typeCheckRow s r = and $ zipWith typeCheckValue s r

typeCheckValue :: DBColumn -> DBValue -> Boolean
typeCheckValue (DBColumn c) v = case (getDBType v) of
  Nothing -> c.nullable --Adding this automatically adds a null validity check
  (Just t) -> t == c.type

getDBType :: DBValue -> Maybe DBType
getDBType v = case v of
  (IntV i) -> Just IntT
  (DoubleV d) -> Just DoubleT
  (StringV s) -> Just StringT
  (BooleanV b) -> Just BooleanT
  Null -> Nothing

--------------------------------------------------------------------------------
--DB Creation and Modification



--------------------------------------------------------------------------------
--Testing

testDB :: DB --passes all checks
testDB = DB {
  schema : [
    DBColumn {name : "Age", type : IntT, nullable : false},
    DBColumn {name : "IsHappy", type : BooleanT, nullable : true},
    DBColumn {name : "Message", type : StringT, nullable : true}
  ],
  data : [
    [IntV 4, Null, StringV "hello"],
    [IntV 16, BooleanV true, Null]
  ]
}

testDB2 :: DB --voilates length check
testDB2 = DB {
  schema : [
    DBColumn {name : "Age", type : IntT, nullable : false},
    DBColumn {name : "IsHappy", type : BooleanT, nullable : true},
    DBColumn {name : "Message", type : StringT, nullable : true}
  ],
  data : [
    [IntV 4, Null, StringV "hello", Null],
    [IntV 16, BooleanV true, Null]
  ]
}

testDB3 :: DB --violates type check
testDB3 = DB {
  schema : [
    DBColumn {name : "Age", type : IntT, nullable : false},
    DBColumn {name : "IsHappy", type : BooleanT, nullable : true},
    DBColumn {name : "Message", type : StringT, nullable : true}
  ],
  data : [
    [IntV 4, Null, StringV "hello"],
    [IntV 16, StringV "uh oh", Null]
  ]
}

testDB4 :: DB --violates null check
testDB4 = DB {
  schema : [
    DBColumn {name : "Age", type : IntT, nullable : false},
    DBColumn {name : "IsHappy", type : BooleanT, nullable : true},
    DBColumn {name : "Message", type : StringT, nullable : true}
  ],
  data : [
    [IntV 4, Null, StringV "hello"],
    [Null, BooleanV true, Null]
  ]
}

--------------------------------------------------------------------------------
--New Idea

{-
This is a datagrid.
Minimum rows: 0
Minimum columns: 1
All data is strings
There are no column titles
Rows and columns are both 0 indexed

All of the above properties of this table are enforced by the api below:
References to non-existant rows or columns fail to modify the table.
There is no way to delete rows or columns below the minimum for each.
All rows and columns which are added respect the current dimension of the table.
-}

--------------------------------------------------------------------------------
--Data Structures

newtype DataGrid = DataGrid {numColumns :: Int, numRows :: Int, data :: Array DataGridRow}

instance showDataGrid :: Show DataGrid where
  show (DataGrid dg) = "\nnumColumns : " <> show dg.numColumns <> ", numRows : " <> show dg.numRows <> "\n" <> prettyFunctor dg.data

type DataGridColumn = Array DataGridCell
type DataGridRow = Array DataGridCell
type DataGridCell = String

--------------------------------------------------------------------------------
--Helper functions

minRows :: Int
minRows = 0

minCols :: Int
minCols = 1

inRange :: Int -> Int -> Boolean
inRange index size = index >= 0 && index < size

newDataGrid :: DataGrid --minimum number of columns is 1
newDataGrid = DataGrid {numColumns : 1, numRows : 0, data : []}

--------------------------------------------------------------------------------
--Insert and Delete columns at arbitrary indexes

insertColumn :: Int -> DataGrid -> DataGrid
insertColumn n dg@(DataGrid {numColumns : c, numRows : r, data : d}) =
  case (inRange n c) of
    true -> DataGrid {numColumns : (c+1), numRows : r, data : unsafePartial $ map (fromJust <<< insertAt n "") d}
    false -> dg

deleteColumn :: Int -> DataGrid -> DataGrid
deleteColumn n dg@(DataGrid {numColumns : c, numRows : r, data : d}) =
  case (c > minCols && inRange n c) of
    true -> DataGrid {numColumns : (c-1), numRows : r, data : unsafePartial $ map (fromJust <<< deleteAt n) d}
    false -> dg

--------------------------------------------------------------------------------
--Insert, Update and Delete rows at arbitrary indexes

insert :: Int -> DataGridRow -> DataGrid -> DataGrid
insert n dr dg@(DataGrid {numColumns : c, numRows : r, data : d}) =
  case (length dr == c && inRange n r) of
    true -> DataGrid {numColumns : c, numRows : (r+1), data : unsafePartial $ fromJust $ insertAt n dr d}
    false -> dg

update :: Int -> DataGridRow -> DataGrid -> DataGrid
update n dr dg@(DataGrid {numColumns : c, numRows : r, data : d}) =
  case (length dr == c && inRange n r) of
    true -> DataGrid {numColumns : c, numRows : r, data : unsafePartial $ fromJust $ updateAt n dr d}
    false -> dg

delete :: Int -> DataGrid -> DataGrid
delete n dg@(DataGrid {numColumns : c, numRows : r, data : d}) =
  case (r > minRows && inRange n r) of
    true -> DataGrid {numColumns : c, numRows : (r-1), data : unsafePartial $ fromJust $ deleteAt n d}
    false -> dg
