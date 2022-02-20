data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing' = True 
    (Just' x) == (Just' y) = x == y
    _ == _ = False

data Either' a b =
    Left' a |
    Right' b

instance (Eq a, Eq b) => Eq (Either' a b) where
    Left' x == Left' y = x == y
    Right' x == Right' y = x == y
    _ == _ = False