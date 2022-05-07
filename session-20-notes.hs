import System.Environment (getArgs)

-- Do notation

main = do
  (command:args) <- getArgs
  options <- getCommand command args
  runAnimation options

-- equivalent to the following
-- main2 = do
--   getArgs >>= (\(command:args) -> getCommand command args) >>= (\options -> runAnimation options)

main2 = do
  putStr "Hello"
  putStr "World"

-- equivalent to the following
-- putStr "Hello" >> putStr "World"

--Get random coordinates
randIniPos :: Board -> IO Coord
randIniPos (Board xSize ySize) = randomRIO (0, xSize) >>=
                  \x ->
                    randomRIO (0, ySize) >>=
                    \y ->
                      pure (Coord x y)

randIniPos' :: Board -> IO Coord
randIniPos' (Board xSize ySize) = do
  x <- randomRIO (0, xSize)
  y <- randomRIO (0, ySize)
  pure (Coord x y)


main :: IO ()
main =  getInputs >>= 
            \current -> 
                threadDelay 1000000 >> print current >>
                threadDelay 1000000 >> moveBall current >> 
                    putStrLn "Nice shot!"

main' :: IO ()
main' = do
  current <- getInputs
  threadDelay 1000000
  print current
  threadDelay 1000000
  moveBall current
  putStrLn "Nice shot!"

-- Functor
{-
class Functor t where
  fmap :: (a -> b) -> t a -> t b
  <$> :: (a -> b) -> t a -> t b
-}
class Functor' t where
  fmap1 :: (a -> b) -> t a -> t b

instance Functor' Maybe where
  fmap1 :: (a -> b) -> Maybe a -> Maybe b
  fmap1 = maybeMap

instance Functor' [] where
  fmap1 :: (a -> b) -> [a] -> [b]
  fmap1 = map

--- **********************
--- Functor is a type class that implement fmap ... so any container type that implement "fmap" is a Functor
--- ***********************

instance Functor ((->) e) where
  fmap :: (a -> b) -> ((->) e a) -> ((->) e b)

  fmap :: (a -> b) -> (e -> a) -> (e -> b)

instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b


fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 f a b = fmap0 f <*> a <*> b

instance Applicative (Maybe a) where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) (Just f) (Just a) = Just (f a)
  (<*>) _ _ = Nothing

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
-- Applicatives must follow this rule (among others)
-- f `fmap` a == pure f <*> a

instance Applicative [] where
  pure :: a -> [a]
  pure a = [a]

  (<*>) :: [(a -> b)] -> [a] -> [b]
  (<*>) (f:fs) (x:xs) = f x : fs <*> xs
  (<*>) [] _ = []
  (<*>) _ [] = []

  -- ex3 = do 
  --   res <- computationA 
  --   computationB 
  --   computationC res

  -- ex3 = complutationA >>= (\res -> computationB >> computationC res)
