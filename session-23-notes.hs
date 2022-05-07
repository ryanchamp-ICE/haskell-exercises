{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}


-- data Reader env a = Reader {
--   runReader :: env -> a
-- }

-- instance Functor (Reader env) where
--   fmap f reader = Reader $ \env -> f (runReader reader env)

-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a

-- instance Monoid [] where
--   mempty = []
--   mappend = (++)

  -- class Functor F where
  --   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- Writer
data Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f writerA = Writer (f a, w)
    where
      (a, w) = runWriter writerA

instance Monoid w => Applicative (Writer w) where   -- Here Monoid constraint is added so that we have mempty
  pure :: a -> Writer w a
  pure a = Writer (a, mempty)

  (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  (<*>) writerF writerA = Writer (b, w)
    where
      b = f a
      w = w1 `mappend` w2
      (f, w1) = runWriter writerF
      (a, w2) = runWriter writerA

-- class Monad m where
--   return :: a -> m a

--   (>>=) :: m a -> (a -> m b) -> m b

--   (>>) :: m a -> m b -> m b
--   m1 >> m2 = m1 >>= (\_ -> m2)

instance (Applicative (Writer w), Monoid w) => Monad (Writer w) where
  return :: a -> Writer w a
  return = pure

  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  (>>=) writerA f = Writer (b, w)
    where
      (a, w1) = runWriter writerA
      writerB = f a
      (b, w2) = runWriter writerB
      w = w1 `mappend` w2

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

-- Calculator that logs
type Calc a = Writer String a

tellStr :: String -> Calc ()
tellStr s = tell (s ++ "\n")

-- makeNumber :: Int -> Calc Int
-- makeNumber i = do
--   tellStr("Making the number " ++ show i)
--   return i

integerToCalc :: (Num a, Show a) => Integer -> Calc a
integerToCalc i = do
  tellStr("Making the number " ++ show i)
  return (fromInteger i)

-- plusNumber :: Calc Int -> Calc Int -> Calc Int
-- plusNumber wi wj = do
--   i <- wi
--   j <- wj
--   tellStr("Adding 2 numbers: " ++ show i ++ " and " ++ show j)
--   return (i + j)

-- plusNumber (makeNumber 1) (makeNumber 2)


-- minusNumber :: Calc Int -> Calc Int -> Calc Int
-- minusNumber wi wj = do
--   i <- wi
--   j <- wj
--   tellStr("Substracting 2 numbers: " ++ show i ++ " and " ++ show j)
--   return (i - j)

opNumber :: Show a => String -> (a -> a -> a) -> Calc a -> Calc a -> Calc a
opNumber opName op wi wj = do
  i <- wi
  j <- wj
  tellStr(opName ++ " 2 numbers: " ++ show i ++ " and " ++ show j)
  return (i `op` j)

plusNumber :: (Show a, Num a) => Calc a -> Calc a -> Calc a
plusNumber = opNumber "Adding" (+)

minusNumber :: (Show a, Num a) => Calc a -> Calc a -> Calc a
minusNumber = opNumber "Subtracting" (-)

multNumber :: (Show a, Num a) => Calc a -> Calc a -> Calc a
multNumber = opNumber "Multiplying" (*)

instance (Show a, Num a) => Num (Calc a) where
  (+) = plusNumber
  (-) = minusNumber
  (*) = multNumber
  fromInteger = integerToCalc

prettyPrintCalc :: Show a => Calc a -> IO ()
prettyPrintCalc calc = do
  putStrLn ("The result is: " ++ show a)
  putStrLn ""
  putStrLn "The log is:"
  putStrLn "-----------------------------"
  putStrLn w
  putStrLn "-----------------------------"
  where
    (a, w) = runWriter calc

main :: IO ()
main = prettyPrintCalc (2 + 3 - 4 * 5)