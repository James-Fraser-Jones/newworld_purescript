module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (class Foldable, and, length, intercalate)
import Data.Array (zipWith, zip, filter)
import Data.Maybe (Maybe(..))
import Data.Enum (enumFromTo)
import Data.Tuple (fst, snd)

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
--Monologue time

{-
So here's the idea:

We are dead set on using react for our UI.
This, then, means that we're dead set on using Javascript as our language.
Which then means that we're going to have to use JSON to represent our database information.

Since we intend to update our database using UI interaction, we will require functions
for transforming our JSON representation of a database in the same way that SQL might transform
a real database.

Having this will give us a very simple way of reasoning about what is happening as well as making
it very easy to translate the transformation that is happening at the JSON level to a transformation
at the SQL level. This should allow us to keep the UI representation of the database aligned with the
actual database.

It's also important, however, that we do not lean on the JSON level too much. We want to strictly
enforce a constant flow between the database and the UI so as to captialize as much as we can on the
robustness of the database implementation. As well as to ensure that the UI is accurately representing
the database.

Two things to keep in mind with regard to "constant flow". 1. There can be computational and memory bandwidth
over-head. 2. There can be network overhead which might even be quite expensive. The extent to which this
might become a problem is very dependent on how I end up implementing things and also dependent on certain
implementations/technologies and things that I don't understand very well.

This issue is complicated more by the fact that we intend multiple people to be editing the database simultaneously.
Does this mean that the entire table state needs to be sent to each client on every update? It seems the most sensible
thing to do honestly but also seems very costly, especially since it has to be converted to JSON every time and then
sent across a network. The alternative would be calculating the changes but then you risk changes not getting picked
up and UI falling out of sync and stuff like that.

I think that our main selling points are:
Providing an effecive database structure to meet the specific needs of auction companies.
Providing online and collaborative services to conveniently create and modify tables in said database.
Providing information security by means of automated database backups.
Providing useful external tools to deliver the data from said database in useful ways (e.g. receipts)

What is even the benefit of using a database in this circumstance???
I feel like at the moment, the only obvious advantage is that
-}
