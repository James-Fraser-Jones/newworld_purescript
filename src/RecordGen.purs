--------------------------------------------------------------------------------
-- all generated functions are based on the following template:

-- modify_fieldName' :: forall a b c. (a -> b) -> {fieldName :: a | c} -> {fieldName :: b | c}
-- modify_fieldName' f rec = rec {fieldName = f rec.fieldName}
--------------------------------------------------------------------------------

module Data.Record.Gen where
import Prelude

type DBRecord' = {name :: String, age :: Int, gender :: Boolean, likesTomatoes :: Boolean}

newtype DBRecord = DBRecord DBRecord'

instance showDBRecord :: Show DBRecord where
	show (DBRecord a) = show a

new_DBRecord :: String -> Int -> Boolean -> Boolean -> DBRecord
new_DBRecord a b c d = DBRecord {name : a, age : b, gender : c, likesTomatoes : d}

liftR :: (DBRecord' -> DBRecord') -> DBRecord -> DBRecord
liftR f (DBRecord a) = DBRecord $ f a

--------------------------------------------------------------------------------
--name

modify_name' :: forall a b c. (a -> b) -> {name :: a | c} -> {name :: b | c}
modify_name' f rec = rec {name = f rec.name}

modify_name :: (String -> String) -> DBRecord -> DBRecord
modify_name = liftR <<< modify_name'

update_name :: String -> DBRecord -> DBRecord
update_name = liftR <<< modify_name' <<< const

--------------------------------------------------------------------------------
--age

modify_age' :: forall a b c. (a -> b) -> {age :: a | c} -> {age :: b | c}
modify_age' f rec = rec {age = f rec.age}

modify_age :: (Int -> Int) -> DBRecord -> DBRecord
modify_age = liftR <<< modify_age'

update_age :: Int -> DBRecord -> DBRecord
update_age = liftR <<< modify_age' <<< const

--------------------------------------------------------------------------------
--gender

modify_gender' :: forall a b c. (a -> b) -> {gender :: a | c} -> {gender :: b | c}
modify_gender' f rec = rec {gender = f rec.gender}

modify_gender :: (Boolean -> Boolean) -> DBRecord -> DBRecord
modify_gender = liftR <<< modify_gender'

update_gender :: Boolean -> DBRecord -> DBRecord
update_gender = liftR <<< modify_gender' <<< const

--------------------------------------------------------------------------------
--likesTomatoes

modify_likesTomatoes' :: forall a b c. (a -> b) -> {likesTomatoes :: a | c} -> {likesTomatoes :: b | c}
modify_likesTomatoes' f rec = rec {likesTomatoes = f rec.likesTomatoes}

modify_likesTomatoes :: (Boolean -> Boolean) -> DBRecord -> DBRecord
modify_likesTomatoes = liftR <<< modify_likesTomatoes'

update_likesTomatoes :: Boolean -> DBRecord -> DBRecord
update_likesTomatoes = liftR <<< modify_likesTomatoes' <<< const
