module Animation.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

type Animation env state a = ReaderT env (StateT state IO) a

runAnimation :: env -> state -> Animation env state a -> IO a
runAnimation env state action = evalStateT (runReaderT action env) state
