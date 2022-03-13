module First.Second (getFirstAndLastName) where

import qualified First.Second.Third as FST

getFirstAndLastName :: IO (String, String)
getFirstAndLastName = 
  FST.prompt "First Name: " >>= \fn ->
    FST.prompt "Last Name: " >>= \ln ->
      pure (fn, ln)