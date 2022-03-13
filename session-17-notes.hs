-- Monads (IO)
import System.IO (hFlush, stdout)

takeMyNameAndSayHi :: IO ()
takeMyNameAndSayHi =
    getLine
        >>= \name -> putStrLn ("Hello, " ++ name)

takeMyNameAndSayHiFullName :: IO ()
takeMyNameAndSayHiFullName =
    putStr ("First name: ") >> getLine >>=
      (\firstName -> putStr ("Last name: ") >> getLine >>=
        \lastName -> putStrLn ("Hello, " ++ firstName ++ " " ++ lastName))

-- main :: IO ()
-- main = takeMyNameAndSayHiFullName

data Color = Black | Gold | Red | Green | Blue | Orange
  deriving (Show, Read)

data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  twitter :: String,
  color :: Color
}

instance Show Person where
    show p =
        "This is my person:\n" ++
         "First Name: " ++ firstName p ++ "\n" ++
         "Last Name: " ++ lastName p ++ "\n" ++
         "Age: " ++ show (age p) ++ "\n" ++
         "Twitter: " ++ twitter p ++ "\n" ++
         "Favorite Color: " ++ show (color p)

prompt :: String -> IO String
prompt msg = putStrLn msg >> hFlush stdout >> getLine

main :: IO ()
main =
  prompt "First name: " >>=
      \fn -> prompt "Last name: " >>=
      \ln -> prompt "Age: " >>=
      \a -> prompt "Twitter: " >>=
      \tw -> prompt "Color: " >>=
      \c -> pure Person { firstName = fn, lastName = ln, age = read a, twitter = tw, color = read c } >>=
      \myPerson -> putStrLn ("Hello, " ++ show myPerson)