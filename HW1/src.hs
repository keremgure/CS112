import Data.List
import Data.Maybe
count :: String -> Char -> Int
count [] c = 0
count (x:xs) c | x == c = 1 + count xs c
               | otherwise = count xs c

nubS :: String -> String
nubS "" = ""
nubS str = nubb str []


nubb :: String -> [Char] -> String
nubb [] list2 = []
nubb (x:xs) list2 | includes list2 x = nubb xs list2
                  | otherwise = x : nubb xs (x:list2)


includes :: [Char] -> Char -> Bool
includes [] c = False
includes (x:xs) c | x == c = True
                  | otherwise = includes xs c


lengths :: (Eq a) => [[a]] -> [Int]
lengths [[]] = [0]
lengths lists = map getLength lists

getLength :: (Eq a) => [a] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs


takeOdds :: [Int] -> [Int]
takeOdds [] = []
takeOdds (x:xs) | (odd x) = x : takeOdds xs
                | otherwise = takeOdds xs

wholePart :: String -> String
wholePart "" = ""
wholePart (x:xs) | x /= '.' = x:wholePart xs
                 | otherwise = wholePart ""

fracPart :: String -> String
fracPart "" = ""
fracPart (x:xs) | x == '.' = frac (x:xs) True
                | otherwise = fracPart xs


frac :: String -> Bool -> String
frac "" False = ""
frac (x:xs) True | x == '.' = frac xs True
frac (x:xs) True | otherwise = (x:xs)

whole2Int :: String -> Int
whole2Int "" = 0
whole2Int str = let st = wholePart str
                in read st :: Int


letterIndex :: Char -> Int
letterIndex c = let (y:ys) = ['A'..'Z']
                in fromJust $elemIndex c (y:ys)

letterIndexv2 :: Char -> [Char] -> Int ->Int
letterIndexv2 c [] a = -1
letterIndexv2 c (x:xs) a | x /= c = letterIndexv2 c xs (a+1)
                         | otherwise = a

convertIndex :: String -> Int
convertIndex "" = 0
convertIndex (x:xs) = let size = getLength (x:xs)
                          alphSize = 26
                          elementIndex = letterIndex x
                        in elementIndex + (alphSize * (size -1)) + convertIndex xs

convertIndexv2 :: String -> Int
convertIndexv2 "" = 0
convertIndexv2 (x:xs) = let size = getLength (x:xs)
                            alphSize = 26
                            elementIndex = letterIndexv2 x ['A'..'Z'] 0
                        in elementIndex + (alphSize * (size - 1)) + convertIndex xs
