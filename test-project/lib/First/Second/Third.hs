module First.Second.Third (prompt) where

import System.IO (hFlush, stdout) -- Import only hFlush and stdout from System.IO
-- import System.IO -- Import everything from System.IO
-- import System.IO as SIO -- Import everything from System.IO and alias as SIO
-- import qualified System.IO -- Specifies that you must include the module name when refering to imports
-- import qualified System.IO as SIO -- allows you to alias module name (but it must still be specified)
-- import System.IO hiding (stdout) -- Import all except the specifie module(s)

flush :: IO ()
flush = hFlush stdout

prompt :: String -> IO String
prompt msg = putStr msg >> flush >> getLine