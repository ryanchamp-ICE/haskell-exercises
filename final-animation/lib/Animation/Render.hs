module Animation.Render where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Animation.Env (Env (..))
import Animation.State (State (..))
import Animation.Type (Animation)


renderIO :: Animation Env State ()
renderIO = do
  output <- render
  lift (lift (putStrLn output))

render :: Animation Env State String
render = do
  env <- ask
  state <- lift get
  return (renderInternal env state)

renderInternal :: Env -> State -> String
renderInternal env state = makeBox (size env) (position state)

makeVertBorder :: Int -> Int -> String
makeVertBorder width ballX
 | ballX < 0 = '|' : replicate width ' ' ++ "|\n"
 | otherwise = '|' : emptySpaceBefore ++ "O" ++ emptySpaceAfter ++ "|\n"
    where
        emptySpaceBefore :: String
        emptySpaceBefore = replicate ballX ' '

        emptySpaceAfter :: String
        emptySpaceAfter = replicate (width - ballX - 1) ' '

makeHorizBorder :: Int -> String
makeHorizBorder width = replicate (width + 2) '-' ++ "\n"

makeBox :: (Int, Int) -> (Int, Int) -> String
makeBox (width, length) (ballX, ballY)
    | ballX > width || ballY > length || ballY < 0 = error "Ball out of bounds"
    | otherwise = makeHorizBorder width ++ 
                    concat (replicate emptyRowsBefore (makeVertBorder width (-1))) ++
                    makeVertBorder width ballX ++
                    concat (replicate emptyRowsAfter (makeVertBorder width (-1))) ++
                    makeHorizBorder width
    where
        emptyRowsBefore :: Int
        emptyRowsBefore = (ballY + 1) - 1

        emptyRowsAfter :: Int
        emptyRowsAfter = length - (ballY + 1)