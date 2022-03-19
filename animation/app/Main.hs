module Main where
import Control.Concurrent

main :: IO ()
main = animate

offset :: Int -> IO ()
offset 0 = pure ()
offset i = putStrLn "" >> offset (i - 1)

squarify :: Int -> Int -> IO ()
squarify i b = 
    offset 30 >> putStrLn (makeBox (i, i) (b, b))

animate :: IO ()
animate =
    putStrLn "Animation begin!"
        >> animateHelper 10 0
        >> putStrLn "Animation end!"

animateHelper :: Int -> Int -> IO ()
animateHelper i b =
    if b < i
    then
        squarify i b
            >> threadDelay 1000000
            >> animateHelper i (b + 1)
    else
        pure ()

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