module Animation.Env where

data Env = Env {
    size :: (Int, Int),
    velocity :: Int
  }

defaultEnv :: Env
defaultEnv = Env {
    size = (20, 10),
    velocity = 1
  }