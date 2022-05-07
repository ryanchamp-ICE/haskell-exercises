module Main where

-- import Animation.Env (Env(..), defaultEnv)
-- import Animation.State (State(..), Direction(..), defaultState)
-- import Animation.Render (renderIO)
-- import Animation.Type (Animation)

import Animation
  ( Animation,
    Direction(..),
    Env(..),
    State(..),
    defaultEnv,
    defaultState,
    next,
    renderIO,
    runAnimation,
    directionFromInt )

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

initialState :: State
initialState = State {
  position = (5, 5),
  direction = (Positive, Positive)
}

putInitialState :: Animation Env State ()
putInitialState = do
  (Env (width, height) _) <- ask
  posX <- lift $ lift $ randomRIO (0, width)
  posY <- lift $ lift $ randomRIO (0, height)
  dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (0, 2)
  dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (0, 2)
  lift $ put $ State (posX, posY) (dirX, dirY)

animate :: Animation Env State ()
animate = do
  renderIO
  next
  lift (lift (threadDelay 1000000))
  animate

mainAnimation :: Animation Env State ()
mainAnimation = do
  putInitialState
  animate

main :: IO ()
main = do
  -- let Env (width, height) _ = defaultEnv
  -- posX <- randomRIO (0, width)
  -- posY <- randomRIO (0, height)
  -- let initialState = State (posX, posY) (Positive, Positive)
  -- animate defaultEnv initialState
  runAnimation defaultEnv defaultState mainAnimation
