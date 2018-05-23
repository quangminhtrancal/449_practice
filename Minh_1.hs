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