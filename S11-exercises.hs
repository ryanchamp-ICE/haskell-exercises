-- class StringLike s where
--   length :: s -> Int
--   join :: s -> s -> s
--   characterAt :: Int -> Char

-- data StringWithLength = StringWithLength String Int
--   deriving Show

-- EXERCISE: Make String a member of StringLike
--instance StringLike String where

-- EXERCISE: Make StringWithLength a member of StringLike
-- Optional: Use Text from Data.Text

-- Old naive way
-- getMessage :: User -> ChatId -> String
-- -- I know typeclasses way
-- getMessage :: StringLike s => User -> ChatId -> s