-- class Monad m where
--   return :: a -> m a

--   (>>=) :: m a -> (a -> m b) -> m b

--   (>>) :: m a -> m b -> m b
--   m1 >> m2 = m1 >>= (\_ -> m2)

instance Monad Maybe where
  return :: a -> Maybe a
  return = pure

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) Nothing _ = Nothing
  (>>=) (Just x) f = f x