module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Record.Gen(DBRecord, new_DBRecord, update_age)

main :: Effect Unit
main = do
  log "Hello sailor!"

test :: DBRecord
test = new_DBRecord "James" 23 true false
