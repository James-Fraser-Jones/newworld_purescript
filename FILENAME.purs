--------------------------------------------------------------------------------
-- all generated functions are based on the following template:

-- modify_fieldName' :: forall a b c. (a -> b) -> {fieldName :: a | c} -> {fieldName :: b | c}
-- modify_fieldName' f rec = rec {fieldName = f rec.fieldName}
--------------------------------------------------------------------------------

module Data.Record.Gen where
import Prelude

type TYPEALIAS = {LABEL1 :: TYPE1}

newtype TYPECON = DATACON TYPEALIAS

instance showTYPECON :: Show TYPECON where
	show (DATACON a) = show a

new_TYPECON :: TYPE1 -> TYPECON
new_TYPECON a = DATACON {LABEL1 : a}

liftR :: (TYPEALIAS -> TYPEALIAS) -> TYPECON -> TYPECON
liftR f (DATACON a) = DATACON $ f a

--------------------------------------------------------------------------------
--LABEL1

modify_LABEL1' :: forall a b c. (a -> b) -> {LABEL1 :: a | c} -> {LABEL1 :: b | c}
modify_LABEL1' f rec = rec {LABEL1 = f rec.LABEL1}

modify_LABEL1 :: (TYPE1 -> TYPE1) -> TYPECON -> TYPECON
modify_LABEL1 = liftR <<< modify_LABEL1'

update_LABEL1 :: TYPE1 -> TYPECON -> TYPECON
update_LABEL1 = liftR <<< modify_LABEL1' <<< const

