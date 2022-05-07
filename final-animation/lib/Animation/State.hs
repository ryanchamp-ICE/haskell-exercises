module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Animation.Env (Env (..))
import Animation.Type (Animation (..))

data Direction = Positive | Negative | Neutral

directionFromInt :: Int -> Direction
directionFromInt 0 = Neutral
directionFromInt 1 = Positive
directionFromInt 2 = Negative
directionFromInt _ = error "Invalid direction"

directionToMultiplier :: Direction -> Int
directionToMultiplier Positive = 1
directionToMultiplier Negative = -1
directionToMultiplier Neutral = 0

data State = State {
  position :: (Int, Int),
  direction :: (Direction, Direction)
}

defaultState :: State
defaultState = State {
  position = (0, 0),
  direction = (Neutral, Neutral)
}

next :: Animation Env State ()
next = do
  env <- ask
  prevState <- lift get
  lift (put $ nextInternal env prevState)

nextInternal :: Env -> State -> State
nextInternal (Env (width, height) velocity) (State (prevX, prevY) (prevXDir, prevYDir)) =
  State (nextX, nextY) (nextXDir, nextYDir)
  where 
    nextXUnbounded = prevX + directionToMultiplier prevXDir * velocity
    nextYUnbounded = prevY + directionToMultiplier prevYDir * velocity

    nextX =
      case prevXDir of
        Neutral -> nextXUnbounded
        Positive -> min nextXUnbounded width
        Negative -> max nextXUnbounded 0

    nextY =
      case prevYDir of
        Neutral -> nextYUnbounded
        Positive -> min nextYUnbounded height
        Negative -> max nextYUnbounded 0

    nextXDir =
      case prevXDir of
        Neutral -> Neutral
        Positive ->
          if nextXUnbounded > width
            then Negative
            else Positive
        Negative ->
          if nextXUnbounded < 0
            then Positive
            else Negative

    nextYDir = 
      case prevYDir of
        Neutral -> Neutral
        Positive ->
          if nextYUnbounded > height
            then Negative
            else Positive
        Negative ->
          if nextYUnbounded < 0
            then Positive
            else Negative