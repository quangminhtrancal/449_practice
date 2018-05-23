import Data.Char
--fromEnum :: Char -> Int
--toEnum :: Int -> Char

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

-- 3.16
stringUpper :: String -> String
stringUpper xs = [toUpper ch| ch <- xs]

--toUpper :: Char -> Char
--toUpper ch = toEnum (fromEnum ch + offset)

-- 3.17 ????
charToNum :: Char -> Int
charToNum ch 
    | ('0' <= ch) && (ch <= '9') = ord ch - ord '0'
    | otherwise = 0

--3.18

--main :: IO ()
--main = do
    --line <- promptLine "Enter a value: "                -- line :: String
    --let line2 = "\"" ++ line ++ "\""                    -- line2 :: String
    --putStrLn ("you said " ++ line2)
    --putStrLn "Bye."

onThreelines :: String -> String -> String -> String
onThreelines a b c = a++ "\n" ++b++c
--3.19
romanDigit :: Char -> String
romanDigit ch
    | ch == '1'="I"
    | ch == '2'="II"
    | ch == '3'="III"
    | ch == '4'="IV"
    | ch == '5'="V"
    | ch == '6'="VI"
    | ch == '7'="VII"
    | ch == '8'="VIII"
    | ch == '9'="IX"
    | otherwise = [ch]
--3.20
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / fromIntegral 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c 
    | (fromIntegral a>averageThree a b c) && (fromIntegral b>averageThree a b c) = 2
    | (fromIntegral b>averageThree a b c) && (fromIntegral c>averageThree a b c) = 2
    | (fromIntegral a>averageThree a b c) && (fromIntegral c>averageThree a b c) = 2
    | (fromIntegral a>averageThree a b c) = 1
    | (fromIntegral b>averageThree a b c) = 1
    | (fromIntegral c>averageThree a b c) = 1
    | otherwise = 0

--3.22
numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | b*b-4*a*c>0 = 2
    | b*b-4*a*c==0 = 1
    | b*b-4*a*c<0 = 0
--3.23

-- 4.1


maxFour :: Int -> Int -> Int -> Int -> Int
maxFour x y z t = max (maxThree x y z) t

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max (max x  y) z

-- 4.2
between :: Int -> Int -> Int -> Bool
between x y z
    | (x<=y)&&(y<=z)=True
    | otherwise = False

-- 4.3
howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
    | x==y && y==z = 3  
    | x/=y && y/=z && z/=x = 0
    | otherwise = 2

-- Define where
sumSquares n m
    = sqN + sqM
      where
      sqN=n*n
      sqM=m*m

maxsq x y
    | sq x > sq y = sq x
    | otherwise = sq y
        where 
        sq x = x*x

-- Define type
data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper


--data Bool = False | True
--    deriving (Show, Eq, Ord)
    -- False has lower order than True or False < True

fac :: Integer -> Integer
fac n
    | n == 0 =1
    | n>0 = fac (n-1) * n

-- 4.17
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
    | m > n = 0
    | m ==n = n
    | otherwise = m * rangeProduct (m+1) n

-- 4.18
fac1 :: Integer -> Integer
fac1 n = rangeProduct 1 n

-- 4.19

multiplyNumber :: Integer -> Integer -> Integer
multiplyNumber x y 
    | x== 1 = y 
    | x > 0 = y+ (multiplyNumber (x-1) y)


-- 4.20
intSquareRoot :: Int -> Int
intSquareRoot n = check n where
  check i   | i*i > n   = check (i - 1) 
          | i*i <= n  = i

--4.21

--4.22

--4.23

--4.32
power2 :: Int -> Int
power2 n
    | n ==1 = 2
    | n `rem` 2 == 0 = power2 (n `div` 2)* power2 (n `div` 2)
    | n `rem` 2 /= 0 = 2* power2 ((n-1) `div` 2)*power2 ((n-1) `div` 2)
