{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State next) = State $ \s ->
                          let (a, s1) = next s
                          in (f a, s1)

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: State s a -> (a -> State s b) -> State s b


-- Applicative parsers
-- parsec
-- megaparsec
-- attoparsec

instance Functor (State s) => Applicative (State s) where
  pure :: a -> State s a
  pure a = State (\s -> (a, s))

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) (State sf) (State sa) = State $ \s ->
                                  let (f, s1) = sf s
                                      (a, s2) = sa s1
                                  in
                                    (f a, s2)


instance Applicative (State s) => Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State nextA) f = State $ \s ->
                          let (a, nextS) = nextA s
                              (State nextB1) = f a
                          in
                            nextB1 nextS

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- Example

data GameState = GameState Bool Int

data GameInput = TogglePower
                | Punch
                | GetPunched

type Game a = State GameState a

punch :: Game ()
punch = do
  (GameState isOn score) <- get
  if isOn
  then put (GameState isOn (score + 1))
  else put (GameState isOn score)

getPunched :: Game ()
getPunched = do
  (GameState isOn score) <- get
  if isOn
  then put (GameState isOn (score - 1))
  else put (GameState isOn score)

togglePower :: Game ()
togglePower = do
  (GameState isOn score) <- get
  put (GameState (not isOn) score)

getScore :: Game Int
getScore = do
  (GameState _ score) <- get
  return score

toGameCombinator :: GameInput -> Game ()
toGameCombinator Punch = punch
toGameCombinator GetPunched = getPunched
toGameCombinator TogglePower = togglePower


-- makeGameComputation :: [GameInput] -> Game Int
-- makeGameComputation [] = getScore
-- makeGameComputation (x:xs) = toGameCombinator x >> makeGameComputation xs

makeGameComputation :: [GameInput] -> Game Int
makeGameComputation xs
  = foldr ((>>) . toGameCombinator) getScore xs

runGame :: [GameInput] -> Int
runGame inputList =
  let (finalScore, _) = runState (makeGameComputation inputList) (GameState False 0)
    in finalScore