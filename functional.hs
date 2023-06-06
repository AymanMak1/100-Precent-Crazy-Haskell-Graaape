-- Imported but not used
import Data.Char
import Data.List
import Data.Maybe

-- Sample --

indicesOfEmpties :: Eq a => [[a]] -> [Int]
indicesOfEmpties xs = [i | (i,x) <- zip [1..] xs, null x]
-- indicesOfEmpties [] == []
-- indicesOfEmpties [[]] == [1]
-- indicesOfEmpties [[],[]] == [1,2]
-- indicesOfEmpties [[],[1..10], [], []] == [1,3,4]
-- indicesOfEmpties [[],[1..], [], []] == [1,3,4]
-- indicesOfEmpties [[],[1..], [], [], [2,3], [212], []] == [1,3,4,7]
-- indicesOfEmpties [[1,2,3,4],[5,6],[7,8,9],[10]] == []
-- indicesOfEmpties ["[1,2,3,4]","[5,6]","[7,8,9]","","[10]",""] == [4,6]
-- indicesOfEmpties [[10..],[1..], [12..], [20,40,60], [2,3], [212], [0..]] == []

applyOnWords :: (String -> String) -> String -> String
applyOnWords f str = unwords [f word | word <- words str]
-- applyOnWords (id) "" == ""
-- applyOnWords (++ ",") "This is an example sentence" == "This, is, an, example, sentence,"
-- applyOnWords (take 1) "How a spider keeps eight long legs" == "H a s k e l l"
-- applyOnWords (reverse) "Lorem ipsum dolor sit amet" == "meroL muspi rolod tis tema"
-- applyOnWords (drop 1) "The quick brown fox jumps over the lazy dog" == "he uick rown ox umps ver he azy og"
-- applyOnWords (\a -> a ++ a) "I repeat every word twice" == "II repeatrepeat everyevery wordword twicetwice"

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll _ [] _ = []
replaceAll toBeReplaced (x:xs) by 
      | x == toBeReplaced = by: replaceAll toBeReplaced xs by
      | otherwise         = x: replaceAll toBeReplaced xs by
-- replaceAll 1 [] 2 == []
-- replaceAll 1 [23,21,23,123,3,2,1,1,23,1] 2 == [23,21,23,123,3,2,2,2,23,2]
-- replaceAll 'a' "The quick brown fox jumps over the lazy dog" 'e' == "The quick brown fox jumps over the lezy dog"
-- replaceAll ' ' "The quick brown fox jumps over the lazy dog" '_' == "The_quick_brown_fox_jumps_over_the_lazy_dog"
-- replaceAll False [False, False, False] True == [True, True, True]
-- replaceAll 50 [44,45,46,47,48,49] 100 == [44,45,46,47,48,49]
-- replaceAll 99 [97..103] 100 == [97,98,100,100,101,102,103]

applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhile _ _ [] = []
applyWhile pred f (x:xs) | pred x    = f x : applyWhile pred f xs
                         | otherwise = x: applyWhile pred f xs

--applyWhile (<5) (+3) [1,2,3,4,5,6,7,8,9] == [4,5,6,7,5,6,7,8,9]
--applyWhile (>10) (+ (-1)) [1,2,3] == [1,2,3]
--applyWhile isUpper toLower "ALMAfa SZILVA" == "almafa SZILVA"
--take 10 (applyWhile (\x -> x `mod` 5 == 0) (`div` 5) [0,5..]) == [0..9]
--take 15 (applyWhile (< 10) (`div` 2) [0..]) == [0,0,1,1,2,2,3,3,4,4,10,11,12,13,14]
--map (\f -> f 3) (applyWhile (const True) (\f y -> f (y+1)) [(+1),(*2)]) == [5,8]


fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int
fixedPointIn f x n
  | n < 0 = Nothing
  | f x == x = Just 0
  | otherwise = case fixedPointIn f (f x) (n - 1) of
                  Just steps -> Just (steps + 1)
                  Nothing -> Nothing
-- fixedPointIn (\x -> x) 3 0 == Just 0
-- fixedPointIn abs (-3) 0 == Nothing
-- fixedPointIn abs (-3) 1 == Just 1
-- fixedPointIn abs (-3) 4 == Just 1
-- fixedPointIn abs (-3) (-1) == Nothing
-- fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) (-1) == Nothing
-- fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) 4 == Just 2
-- fixedPointIn (drop 2) [1..] 5 == Nothing
-- fixedPointIn (drop 2) [1..20] (-10) == Nothing
-- fixedPointIn (drop 2) [1..20] 9 == Nothing
-- fixedPointIn (drop 2) [1..20] 10 == Just 10
-- fixedPointIn (drop 2) [1..20] 13 == Just 10

lackOfLetters :: String -> [Char] -> Maybe [Char]
lackOfLetters "" _ = Nothing
lackOfLetters text font = case missingChars of
    [] -> Nothing
    chars -> Just chars
    where
      lowerText = map toLower text
      missingChars = nub $ filter (`notElem` font) lowerText

-- lackOfLetters "" "" == Nothing
-- lackOfLetters "" "asdf" == Nothing
-- lackOfLetters "Cheesecake" "acehks" == Nothing
-- lackOfLetters "" ['a'..'z'] == Nothing
-- lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" (['a'..'z']) == Nothing
-- sort (fromJust (lackOfLetters "Cheesecake" "aeiou")) == (sort "chsk")
-- sort (fromJust (lackOfLetters "programming" ['a'..'i'])) == (sort "promn")
-- sort (fromJust (lackOfLetters "programming" ['j'..'z'])) == (sort "gai")
-- sort (fromJust (lackOfLetters "BreakingNews" ['c'..'o'])) == (sort "braws")
-- sort (fromJust (lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" ['b'..'l'])) == (sort "tqurownxmpsvazy")

maxValFun :: Ord b => [a -> b] -> a -> Maybe Int
maxValFun [] _ = Nothing
maxValFun fs x = Just $ snd $ maximum [(f x,i) | (f,i) <- zip fs [1..]]

--maxValFun [] 3 == Nothing
--maxValFun [(+1), (*2), (^2), (*4)] 4 == Just 4
--maxValFun [(+1), (*2), (^2), (*4), (+2)] 4 == Just 4
--maxValFun [(+1), (*2), (^2), (*4), (+2)] 6 == Just 3
--maxValFun [(\f -> f 2), (\f -> f 1 * 5), const 9, (\f -> f 0)] (5*) == Just 2

-- File Structure --
-- 0- Quizzes
-- 1- Basics
-- 2- Tuples
-- 3- Guards
-- 4- Lists
-- 5- Record types
-- 6- Recursive functions and definitions
-- 7- Lists perations
-- 8- HOF

-- 0- Quizzes
-- Define a function isParenthesis that checks whether a character is a parenthesis.

isParenthesis :: Char -> Bool
isParenthesis '(' = True
isParenthesis ')' = True
isParenthesis _   = False
-- Tests:
-- isParenthesis '(' == True
-- isParenthesis ')' == True
-- isParenthesis 'a' == False
-- isParenthesis 'G' == False
-- isParenthesis '$' == False
-- isParenthesis '@' == False

-- Define a function
--median3 :: (Int,Int,Int) -> Int
--The function median3 should return the median of the three input integers.
--
--The median of x,y,z is the middle value after sorting x,y,z in ascending order.

median3 :: (Int,Int,Int) -> Int
median3 (x, y, z) = sort [x, y, z] !! 1

median3' (x, y, z)
  | (x <= y && y <= z) || (z <= y && y <= x) = y
  | (y <= x && x <= z) || (z <= x && x <= y) = x
  | otherwise = z

-- tests :: [Bool]
-- tests = [ median3 (1,2,3) == 2 
--         , median3 (3,2,1) == 2
--         , median3 (1,3,2) == 2
--         , median3 (2,1,3) == 2
--         , median3 (2,3,1) == 2
--         , median3 (3,1,2) == 2
--         , median3 (18,4,67) == 18
--         , median3 (78,256,98) == 98
--         ]

--Define a function splitIncrPrefix :: [Int] -> ([Int], [Int]) 
--which splits a list after its largest increasing prefix.
--If the input list is [x(1), x(2), ..., x(n)], 
--then the function should return the pair ([x(1), x(2), ..., x(i)], [x(i+1), ..., x(n)]) 
--where [x(1),x(2),...,x(i)] is increasing and x(i) > x(i+1).

splitIncrPrefix :: [Int] -> ([Int], [Int])
splitIncrPrefix [] = ([], [])
splitIncrPrefix [x] = ([x], [])
splitIncrPrefix (x:y:xs)
  | x < y = let (ys, zs) = splitIncrPrefix (y:xs)
            in (x:ys, zs)
  | otherwise = ([x], y:xs)

-- tests :: [Bool]
-- tests = [ splitIncrPrefix [] == ([], [])
--         , splitIncrPrefix [1,3,2] == ([1,3], [2])
--         , splitIncrPrefix [1,2,3] == ([1,2,3], [])
--         , splitIncrPrefix [3,1,2] == ([3], [1,2])
--         , splitIncrPrefix [2,1,3] == ([2], [1,3])
--         , splitIncrPrefix [2,3,1] == ([2,3], [1])
--         ]

--Define a function included :: Eq a => [a] -> [a] -> Bool. 
--included xs ys should check whether all elements of the list xs are also elements of ys.
--Define a function sorted :: Ord a => [a] -> Bool that checks whether the input list is sorted. 
--Don't sort the input list, solutions similar to sorted xs = (sort xs == xs) are not accepted.

included :: Eq a => [a] -> [a] -> Bool
included [] _ = True
included (x:xs) ys | x `elem` ys = included xs ys
                   | otherwise = False

included' xs ys = all (`elem` ys) xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x1:x2:xs) | x1 <= x2 = sorted (x2:xs)
                  | otherwise = False

--tests :: [Bool]
--tests = [ included [1,2] [3,2,1]
--        , included [] [42]
--        , not (included [3] [1])
--        , not (included [1..5] [1,2,4,5] )
--        , sorted [1,2,3]
--        , sorted ([] :: [Int])
--        , not (sorted [2,1])
--        ]

--Define a function isProduct :: [Int] -> [Int] -> Int -> Bool.
--isProduct xs ys z should check whether the integer z can be written as the product 
--of an element of xs with an ys.

isProduct :: [Int] -> [Int] -> Int -> Bool
isProduct xs ys z = elem z [x * y | x <- xs, y <- ys]

-- tests :: [Bool]
-- tests = [ isProduct [1] [1] 1
--         , not (isProduct [1] [1] 2)
--         , not (isProduct [] [1..100] 10)
--         , not (isProduct [1..10] [] 10)
--         , isProduct [1..10] [1..10] 64
--         , not (isProduct [1..10] [1..10] 67)
--         ]

-- The data type OneTwoThree encodes numbers in the range 1 .. 3. 
--Define a function max' :: OneTwoThree -> OneTwoThree -> OneTwoThree 
-- that returns the maximum of two numbers in this encoding.

data OneTwoThree 
    = One
    | Two
    | Three
    deriving (Eq, Show)

max' :: OneTwoThree -> OneTwoThree -> OneTwoThree
max' One One = One
max' One Two = Two
max' Two One = Two
max' Two Two = Two
max' _ Three = Three
max' Three _ = Three

-- tests :: [Bool]
-- tests = [ max' One Three == Three
--         , max' Two One == Two
--         , max' Three Three == Three
--         , max' One One == One
--         ]

-- Define a function fun :: Int -> Int that returns the largest power of 2 
-- that is less than or equal to the input number. 
-- You can assume that the input number is greater than zero.

fun :: Int -> Int
fun n = last [2^x | x <- [1..n], 2^x <= n]

-- tests :: [Bool]
-- tests = 
--   [ fun 1 == 1
--   , fun 2 == 2
--   , fun 3 == 2
--   , fun 4 == 4
--   , fun 5 == 4
--   , fun 6 == 4
--   , fun 15 == 8
--   , fun 16 == 16
--   ]

-- Define a function interleave :: [a] -> [a] -> [a] that interleaves the elements of the two input lists. 
-- You can assume that the two input lists have the same length.


interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys)= x:y:interleave xs ys

--tests :: [Bool]
--tests = [ interleave "" "" == ""
--        , interleave "abc" "xyz" == "axbycz"
--        , interleave "a" "b" == "ab"
--        , interleave "aba" "bab" == "abbaab"
--        , interleave "abcde" "edcba" == "aebdccdbea"
--        ]

--Define a function replacePred :: (a -> Bool) -> a -> [a] -> [a].
--replacePred p b xs should replace the elements of the list xs that satisfy the predicate p by b.

replacePred :: (a -> Bool) -> a -> [a] -> [a]
replacePred _ _ [] = []
replacePred p b (x:xs)
    | p x       = b: replacePred p b xs
    | otherwise = x: replacePred p b xs
-- Examples:
--   replacePred even 99 [1, 2, 3, 4] == [1, 99, 3, 99]
--   replacePred odd  99 [1, 2, 3, 4] == [99, 2, 99, 4]
--   replacePred even 99 [1, 3, 5]    == [1, 3, 5]
--   replacePred odd  99 [1, 3, 5]    == [99, 99, 99]

--Define a function mapPairsAndSwap of type (a -> b) -> (c -> d) -> [(a,c)] -> [(d,b)].

mapPairsAndSwap :: (a -> b) -> (c -> d) -> [(a,c)] -> [(d,b)]
mapPairsAndSwap f g [] = []
mapPairsAndSwap f g ((x,y):xs) = [(g y, f x)] ++ mapPairsAndSwap f g xs

-- tests :: [Bool]
-- tests = 
--   [ mapPairsAndSwap (+1) (*2) [(1,2), (3,4), (5,6)] == [(4,2), (8,4), (12,6)]
--   , mapPairsAndSwap id id [(1,2), (3,4), (5,6)] == [(2,1), (4,3), (6,5)]
--   ]

-- 1- Basics
-- Define a function `subtract'` that subtracts its first argument from its second argument.
--   subtract' 2 10  ==  8
subtract' :: Int -> Int -> Int
subtract' x y = x - y

-- Define functions `even'` and `odd'` that check whether a number is even or odd.
even' :: Int -> Bool
even' x = x `mod` 2 == 0

odd' :: Int -> Bool
odd' x = x `mod` 2 /= 0

-- Define the exclusive or function for booleans.
xor :: Bool -> Bool -> Bool
xor False  False = False
xor False  True  = True
xor True   False = True
xor True   True  = False

-- Define a function `isLetter` that checks if a character is a lowercase letter.
--   isLetter 'a' == True
--   isLetter 'A' == False
--   isLetter 'z' == True
--   isLetter '9' == False
isLetter' :: Char -> Bool
isLetter' c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

-- Define a function `isLetterOrSpace` that checks if a character is a lowercase letter or a space.
--   isLetterOrSpace 'a' == True
--   isLetterOrSpace 'A' == False
--   isLetterOrSpace 'z' == True
--   isLetterOrSpace '9' == False
--   isLetterOrSpace ' ' == True
isLetterOrSpace :: Char -> Bool
isLetterOrSpace c = isLetter' c || c == ' '

-- Define a function `divides` that checks whether an integer divides another integer.
divides :: Int -> Int -> Bool
divides n d= n `rem` d == 0

-- Define a function `isLeapYear` that determines if a given year is a leap year.
-- A year is a leap year if it is divisible by 4 but not by 100, or if it is divisible by 400.
isLeapYear :: Int -> Bool
isLeapYear year = (divides year 4 && not(divides year 100)) || divides year 400

-- 2- Tuples

emptyTuple :: ()
emptyTuple = ()

oneTuple :: (Int)
oneTuple = (2)


twoTuple :: (Int, String)
twoTuple = (3,"wow")

first :: Int
first = 1

second :: String
second = "ahm"

swap :: (Int, String) -> (String, Int)
swap (a,b) = (b,a)


threeTuple :: (Int, String, Bool)
threeTuple = (1,"nani",True)


{- Person -}

--             Name    Age  Height Weight
type Person = (String, Int, Float, Float)

me :: Person
me = ("Ayman", 23, 181, 88)

describe :: Person -> String
describe (name,_,_,_) = name


-- https://en.wikipedia.org/wiki/Body_mass_index
-- Body Mass Index = weight / (height ^ 2)

bmi :: Person -> Float
bmi (_n,_a,h,w) = w / (h^2)

bmi' :: Person -> String
bmi' p = undefined

-- Define a function `swap3` with the following type:
swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (x,y,z)= (z,x,y)

-- Define a function `assoc` with the following type:
assoc :: ((Int, String), (Float, Double)) -> (Int, (String, Float), Double)
assoc ((a,b),(c,d))= (a,(b,c),d)

{- Fractions / rational numbers -}

-- Type synonym:  Frac  is a synonym for the type  (Int, Int)
type Frac = (Int,      Int)
--           numerator denominator
-- (a, b) represents the fraction (a / b)

-- Note: Built-in fraction type:
--   import Data.Ratio
--   type Frac' = Ratio Int
--   (%) :: Int -> Int -> Frac'

zeroFrac :: Frac
zeroFrac = (0, 1)

oneFrac :: Frac
oneFrac = (1, 1)

twoThirdsFrac :: Frac
twoThirdsFrac = (2, 3)

-- Define the arithmetic operations for fractions.

addFrac :: Frac -> Frac -> Frac
addFrac (a,b) (c,d) = (a*d + b*c,b*d)

negFrac :: Frac -> Frac
negFrac (a,b)= (-a,b)

subFrac :: Frac -> Frac -> Frac
subFrac (a,b) (c,d) = (a*d - b*c, b*d)

mulFrac :: Frac -> Frac -> Frac
mulFrac (a,b) (c,d)= (a*c, b*d)

divFrac :: Frac -> Frac -> Frac
divFrac (a,b) (c,d) = (a*d,b*c)

-- 3- Guards

-- Guards filter the clauses of a function definition based on a boolean condition.
is42 :: Int -> Bool
is42 n | n == 42   = True -- This clause is only used if  n == 42
       | otherwise = False

is43 :: Int -> Bool
is43 n
  | n == 43   = True
  | otherwise = False

-- Note: otherwise is a synonym for True
otherwise' :: Bool
otherwise' = True

-- Define the function min that computes the minimum of two integers.
min' :: Int -> Int -> Int
min' a b | a > b = b
         | otherwise = a

-- Define a function that sorts a pair of integers using pattern matching and guards.
sort2 :: (Int, Int) -> (Int, Int)
sort2 (a,b) | a > b = (b,a)
            | otherwise = (a,b)
-- sort2 (1, 2) == (1, 2)
-- sort2 (2, 1) == (1, 2)

--  Compute the reduced form of a fraction!
--  Examples:
--  ∙ reduceFrac (0, 5)   == (0, 1)
--  ∙ reduceFrac (-1, -1) == (1, 1)
--  ∙ reduceFrac (3, 9)   == (1, 3)
--
--  Hint: use the built in "gcd" (greatest common divisor) function!
reduceFrac :: Frac -> Frac
reduceFrac (an,ad) = 
    if rd < 0 then (-rn, -rd) else (rn, rd) where
    d  = gcd an ad
    rn = an `div` d
    rd = ad `div` d


-- 4- Lists

emptyList :: [Int]
emptyList = []
-- Empty list: []

intList :: [Int]
intList = 1 : [2,3]
-- Add element: (:)

intList' :: [Int]
intList' = [1,3,4]
-- List expression: [x, y, z]

oneToTen :: [Int]
oneToTen = [1..10]
-- Range: [from..to]

tenToOne :: [Int]
tenToOne = [10,9..1]
-- Range with step: [from,next..to]

-- `oneToN 10 == oneToTen`
oneToN :: Int -> [Int]
oneToN n = [1..n]

-- Pattern matching on lists:
null' :: [Int] -> Bool
null' [] = True
null' xs = False

head' :: [Int] -> Int
head' [] = error "Empty List ALKHAWA"
head' (x:xs) = x

tail' :: [Int] -> [Int]
tail' [] = error "Empty List Mat3ssbnich"
tail' (x:xs) = xs

length' :: [Int] -> Int
length' [] = 0
length' (_x:xs) = 1 + length' xs

singleton :: Int -> [Int]
singleton x = [x]

-- `isSingleton xs` shoould be True if the list contains exactly 1 element.
isSingleton :: [Int] -> Bool
isSingleton l = length l == 1

-- List comprehensions
--   { n ^ 2 | n ∈ N, condition }

-- `squares l` returns the list of the squares of the elements of xs.
-- squares [2, 3, 5] == [4, 9, 25]
squares :: [Int] -> [Int]
squares l = [i^2 | i <- l]

-- `evens xs` keeps the even elements of xs.
-- evens [5, 8, 10, 13, 16] == [8, 10, 16]
evens :: [Int] -> [Int]
evens l = [i | i <- l, even i]

-- `sums` computes all sums of an element of l1 with an element of l2.
-- sums [10, 20] [1,3,5] == [11,13,15,21,23,25]
sums :: [Int] -> [Int] -> [Int]
sums l1 l2 = [e1 + e2 | e1 <- l1, e2 <-l2]

-- `countEven xs` should be the number of even elements in xs.
-- countEvens [5, 8, 10, 13, 16] == 3
countEvens :: [Int] -> Int
countEvens l = length (evens l)

-- `sumOfSquares n` should be the sum of the first n square numbers.
sumOfSquares :: Int -> Int
sumOfSquares n = sum (squares [1..n])

-- `isSquare n` should be True if n is a square number.
--isSquare :: Int -> Bool
isSquare n = n `elem` squares [1..n]

-- `divides` should check if `n` is a multiple of `d`
divides' :: Int -> Int -> Bool
divides' d n =  n `rem` d == 0

-- `divisors n` should be the lists of the divisors of n.
-- divisors 28 == [1,2,4,7,14,28]
divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], divides' d n]

-- `powersOf2 n` should consists of the first n powers of 2.
-- powersOf2 6 == [1,2,4,8,16,32]
powersOf2 :: Int -> [Int]
powersOf2 n = 1:[2 ^ x| x <- [1..(n-1)]]


allNumbers :: [Integer]
allNumbers = [0..]

-- Define a function  alternate :: [a] -> [a] -> [a]
-- creates a new list by alternating between elements
-- of the two input lists.
-- The input lists can be infinite.

alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate _ [] = []
alternate (x:xs) (y:ys)= x:y:alternate xs ys
-- alternate [1,2,3] [10,11,12] == [1,10,2,11,3,12]
-- take 10 (alternate [0,0..] [0..]) == [0,0,0,1,0,2,0,3,0,4]

-- 5- Record types

data Point3D -- < data type
  = Point3D  -- < data constructor
  { point_x :: Int
  , point_y :: Int
  , point_z :: Int 
  }
  deriving (Show) -- < This line says that "show :: Point3D -> String" should be automatically defined.

-- We automatically have a constructor
--   Point3D :: Int -> Int -> Int -> Point3D
-- and projections
--   point_x :: Point3D -> Int
--   point_y :: Point3D -> Int
--   point_z :: Point3D -> Int

zeroPoint :: Point3D
zeroPoint = Point3D 0 0 0

zeroPoint' :: Point3D
zeroPoint' = Point3D { point_x = 0, point_y = 0, point_z = 0 }

otherPoint :: Point3D
otherPoint = zeroPoint { point_x = 2 }
-- otherPoint == Point3D 2 0 0

-- It is possible to pattern match on values of record types.
dotProduct :: Point3D -> Point3D -> Int
dotProduct (Point3D x y z) (Point3D x' y' z') = x*x' + y*y' + z*z'

-- Define the following functions.
pointToTuple :: Point3D -> (Int,Int,Int)
pointToTuple (Point3D x y z)= (x,y,z)

tupleToPoint :: (Int,Int,Int) -> Point3D
tupleToPoint (x,y,z) = (Point3D x y z)

-- Records can also have fields with different types.
data File = File 
          { filename :: String 
          , filesize :: Int
          }
          deriving (Show, Eq)

files :: [File]
files = [ File "file1" 10 
        , File "file2" 1024
        , File "file3" 31
        ]

-- Write a function that checks whether a file with the given filename exists in the list of files.
fileExists :: String -> [File] -> Bool
fileExists str fs = length [file | file <- fs, str == filename file] > 0

-- fileExists "file1" files
-- not (fileExists "file4" files)

data Time = Time 
            { hours   :: Int,
              minutes :: Int 
            }
            deriving (Show)

-- Define a function that advances the time by 1 minute.
nextMinute :: Time -> Time
nextMinute (Time h 59) | h == 23 = Time 0 0
                       | otherwise = Time (h+1) 0
nextMinute (Time h m)  = Time h (m+1)

-- nextMinute (Time 12 13) == Time 12 14
-- nextMinute (Time 3 54) == Time 3 55
-- nextMinute (Time 17 59) == Time 18 0
-- nextMinute (Time 23 59) == Time 0 0

-- Define a list of all valid times.
allTimes :: [Time]
allTimes = [(Time h m) | h <- [0..23], m <- [0..59]]

-- length allTimes == 24 * 60

--------------------------------------------------------------------------------
-- Algebraic Data Types

-- Data types can have different constructors.

data Bool' 
  = True'
  | False'

not' :: Bool' -> Bool'
not' True'  = False'
not' False' = True'

-- Define the following functions. They should behave in the same way as show and (==).

showBool' :: Bool -> String
showBool' True = "True"
showBool' False = "False"

eqBool' :: Bool' -> Bool' -> Bool
eqBool' True'  True'  = True
eqBool' False' False' = True
eqBool'  _      _      = False

-- Define the functions `and` and `or` for Bool'

and :: Bool' -> Bool' -> Bool'
and True' True'   = True'
and False' False' = True'
and _      _      = False'

or :: Bool' -> Bool' -> Bool'
or False'  False' = True'
or _       _      = False'

-- Define conversion functions between Bool and Bool'

bool'ToBool :: Bool' -> Bool
bool'ToBool True' = True
bool'ToBool False'= False

boolToBool' :: Bool -> Bool'
boolToBool' True = True'
boolToBool' False = False'

-- boolToBool' False == False'

data Color 
  = Red
  | Green
  | Blue

-- Define show and eq functions for the type Color.

showColor :: Color -> String
showColor Red = "Red"
showColor Green = "Green"
showColor Blue = "Blue"

eqColor :: Color -> Color -> Bool
eqColor Red Red     = True
eqColor Blue Blue   = True
eqColor Green Green = True
eqColor _     _     = False

data Op
  = Plus
  | Minus
  | Mult
  deriving (Show, Eq)

-- Define a function that applies a binary operation to two integers.
applyOperation :: Int -> Op -> Int -> Int
applyOperation x Plus y = x + y
applyOperation x Minus y = x - y
applyOperation x Mult y = x * y

--------------------------------------------------------------------------------

-- Define a data type for days of the week (Monday .. Sunday)

data Day = Monday| Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

-- Define a function isWeekend that checks whether a day is part of the weekend.

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- Define a function that returns the next day of the week.

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Sunday = Monday
nextDay _      = error "you're from mars"

--------------------------------------------------------------------------------

-- The constructors of data types can contain values.

data MaybeInt = Just' Int
              | Nothing'
              deriving (Eq, Show)

-- Just'    :: Int -> MaybeInt
-- Nothing' :: MaybeInt

-- The values of type MaybeInt are either
--   Just' n   where n is an integer.
--   Nothing'

-- Define a function safeDiv that performs division.
-- Instead of dividing by zero, it should return Nothing'.
safeDiv :: Int -> Int -> MaybeInt
safeDiv _n 0 = Nothing'
safeDiv n d = Just' (n `div` d)

-- data Maybe a = Just a | Nothing

-- Write a function getFile that finds a file with the given filename in the list of files.
-- If the file exists, it should return `Just thefile`.
-- If the file does not exist, it should return `Nothing`.
getFile :: String -> [File] -> Maybe File
getFile _str [] = Nothing
getFile str (f:fs) | str == filename f = Just f
                   | otherwise = getFile str fs

-- getFile "file1" == Just (File "file1" 10)
-- getFile "file4" == Nothing

--------------------------------------------------------------------------------

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos [] = []
swapEvenOddPos (x1:x2:xs) = x2:x1:swapEvenOddPos xs

swapEvenOddPos' xs = let
    odds  = [ x | (i, x) <- zip [1..] xs , odd i  ]
    evens = [ x | (i, x) <- zip [1..] xs , even i ]
  in
  concat [ [l1, l2] | (l1, l2) <- zip evens odds ]


-- 6- Recursive functions and definitions.

-- Definitions in Haskell can be recursive: 
--  a definition can be used in its own body.

fact :: Int -> Int
fact n 
  | n <= 0 = 1
  | otherwise = n * fact (n-1)

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

-- Evaluating `loop` will loop!
loop :: Int
loop = loop + 1

-- Recursive functions without base cases will loop!
fibLoop :: Int -> Int
fibLoop n = fibLoop (n-1) + fibLoop (n-2)

list :: [Int]
list = 1 : list

-- Exercises:

-- Redefine the power function (^) using recursion
pow :: Int -> Int -> Int
pow b 0 = 1
pow b p = b * pow b (p-1)
-- pow 2 10 == 1024
-- pow 3 7  == 2187

-- Redefine the Haskell expression [x .. y] using recursion.
countTo :: Int -> Int -> [Int]
countTo from to | from < to = from : countTo (from + 1) to
                | otherwise  = [to]
-- countTo 1 5 == [1,2,3,4,5]

-- Redefine the function `replicate`.
-- `replicate n x` returns a list that contains n times the element x.
replicate' :: Int -> a -> [a]
replicate' 0 _= []
replicate' n c= c: replicate' (n-1) c
-- replicate' 5 'a' == "aaaaa"

-- Use recursion to define a function firstNSquares that returns the first n square numbers.
-- Hint: use a helper function.
firstNSquares :: Int -> [Int]
firstNSquares n = firstNSquaresHelper n 1

firstNSquaresHelper :: Int -> Int -> [Int]
firstNSquaresHelper n current
  | n <= 0 = []                    -- Base case: stop recursion when n <= 0
  | otherwise = (current^2) : firstNSquaresHelper (n - 1) (current + 1)

-- firstNSquares 5 == [1,4,9,16,25]

-- Use recursion to define the function `binarySearchSqrt`.
-- `binarySearchSqrt low high x` should return the square root of x, 
--   if it is in the interval low .. high.
binarySearchSqrt :: Int -> Int -> Int -> Int
binarySearchSqrt low high x = undefined

-- binarySearchSqrt 4  4  16 == 4
-- binarySearchSqrt 20 30 16 == 20
-- binarySearchSqrt 1  10 16 == 4


-- Use binarySearchSqrt to define the square root function on integers.
sqrtInt :: Int -> Int
sqrtInt = undefined

-- Recursion on lists
-- Functions on lists can be defined by recursion

copyList :: [a] -> [a]
copyList []     = []
copyList (x:xs) = x : copyList xs

-- Define the following functions by recursion:
length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs) | even x    = x: filterEven xs
                  | otherwise = filterEven xs

elem' :: Int -> [Int] -> Bool
elem' n [] = False
elem' n (x:xs) | n == x = True
               | otherwise = elem' n xs


sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x+sum' xs
-- sum' [1,2,3] == 6
-- sum' [1,2,3,4] == 10

product' :: [Integer] -> Integer
product' []    = 1
product' (x:xs)= x*product' xs
-- product' [1,2,3] == 6
-- product' [1,2,3,4] == 24

--------------------------------------------------------------------------------

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x:append xs ys
-- append [1,2,3] [4,5,6] == [1,2,3,4,5,6]

reverse' :: [a] -> [a]
reverse' ls = reverseHelper ls []
  where
    reverseHelper []     acc = acc
    reverseHelper (x:xs) acc = reverseHelper xs (x:acc)

-- reverse'  [1,2,3,4] == [4,3,2,1]

-- Bonus: Linear time implementation of reverse'
--  (Typing  length (reverse' [1..20000])  in ghci should be fast.)

--------------------------------------------------------------------------------
-- Splitting strings

takeUntil :: Char -> String -> String
takeUntil c [] = []
takeUntil c (x:xs) | c /= x = x: takeUntil c xs
                   | otherwise = []
-- takeUntil 'X' "123X4X5" == "123"

dropUntil :: Char -> String -> String
dropUntil c [] = []
dropUntil c (x:xs) | c /= x = dropUntil c xs
                   | otherwise = xs
-- dropUntil 'X' "123X4X5" == "4X5"

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delimiter cs
-- splitOn 'X' "123X4X5" == ["123", "4", "5"]

words' :: String -> [String]
words' xs = splitOn ' ' xs
-- words' "The quick brown fox jumps over the lazy dog." == ["The","quick","brown","fox","jumps","over","the","lazy","dog."]

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x]= x
unwords' (x:xs) = x ++ " " ++ unwords xs
-- unwords' ["The","quick","brown","fox","jumps","over","the","lazy","dog."] == "The quick brown fox jumps over the lazy dog."

-- Insertion sort

-- Define a function insert' that inserts an element at the correct position in a sorted list.
insert' :: Int -> [Int] -> [Int]
insert' n [] = [n]
insert' n (x:xs) | n <= x = n:x:xs
                | otherwise = x: insert' n xs
-- insert' 1 [1,2,3] == [1,1,2,3]
-- insert' 2 [1,2,3] == [1,2,2,3]
-- insert' 3 [1,2,3] == [1,2,3,3]
-- insert' 4 [1,2,3] == [1,2,3,4]

-- Use the function insert' to define a sorting function.
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert' x (insertionSort xs)


--------------------------------------------------------------------------------
-- Merge sort (https://en.wikipedia.org/wiki/Merge_sort#/media/File:Merge_sort_algorithm_diagram.svg)

-- Define a function splitList that splits a list l into two sublists l1 and l2 such that:
-- - abs (length l1 - length l2) <= 1
-- - l  is a permutation of  l1 ++ l2

splitList :: [Int] -> ([Int], [Int])
splitList l = (l1, l2)
  where
    midIndex = (length l + 1) `div` 2
    l1 = take midIndex l
    l2 = drop midIndex l

-- Define a function mergeList that merges two *sorted* lists.
-- If l1 and l2 are two sorted lists, then  mergeLists l1 l2  
-- should be a sorted list containing the elements of l1 and l2.

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys) 
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- Use splitList and mergeLists to implement mergeSort.
mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys1, ys2) = splitList xs
      ys1_sorted = mergeSort ys1
      ys2_sorted = mergeSort ys2
  in mergeLists ys1_sorted ys2_sorted

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Redefine the function `take n`, which takes the first n elements of a list.
-- take' 0 [1, 2, 3] == []
-- take' 2 [1, 2, 3] == [1, 2]
take' :: Int -> [a] -> [a]
take' 0  _   = []
take' n_ []  = []
take' n (x:xs) = x: take' (n-1) xs

-- Redefine the function `drop n`, which drops the first n elements of a list.
-- drop' 0 [1, 2, 3] == [1, 2, 3]
-- drop' 2 [1, 2, 3] == [3]
drop' :: Int -> [a] -> [a]
drop' n []= []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- Redefine the function `splitAt`, which splits a list at a given index.
-- splitAt' 1 [1,2,3] == ([1], [2,3])
-- splitAt' 2 [1,2,3] == ([1,2], [3])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' at xs = (take' at xs, drop' at xs)

-- Redefine the function zip.
--   zip [1,2,3] [4,5,6] == [(1,4), (2,5), (3,6)]
--   zip [1] [4,5,6] == [(1,4)]
zip' :: [a] -> [a] -> [(a, a)]
zip' _  [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- Redefine the function unzip. unzip is a partial inverse of zip.
--   unzip [(1,4), (2,5), (3,6)] == ([1,2,3], [4,5,6])
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([], [])
unzip' ((x, y) : rest) =
    let (xs, ys) = unzip' rest
    in (x : xs, y : ys)

-- Define the function tails', which should return all suffixes of a string.
-- tails' "Hello" == ["Hello", "ello", "llo", "lo", "o", ""]
-- Note: Remember that String = [Char]!
--tails' :: String -> [String]
tails' [] = [""]
tails' (x:xs) =  (x:xs) : tails' xs

-- Hard exercise: Try to define `inits` similarly!
-- inits "cheese" == ["","c","ch","che","chee","chees","cheese"]
inits :: String -> [String]
inits xs = reverse(tails' xs)

inits' [] = [[]]
inits' (x:xs) = [] : map (x:) (inits' xs)

-- `isSuffixOf' p s` should test whether p is a suffix of s.
-- Examples:
--   ∙ "ear"  `isSuffixOf'` "appear" == True
--   ∙ "asdf" `isSuffixOf'` "qwerty" == False
isSuffixOf' :: String -> String -> Bool
isSuffixOf' str [] = False
isSuffixOf' str (x:xs) | str == xs = True
                      | otherwise = isSuffixOf' str xs

-- `isPrefixOf' p s` should test whether p is a prefix of s.
-- Examples:
--   ∙ "app"  `isPrefixOf'` "apple"  == True
--   ∙ "asdf" `isPrefixOf'` "qwerty" == False
isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True 
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
  | x == y = isPrefixOf' xs ys
  | otherwise = False

-- `isPrefixOf' p s` should test whether p is an infix of s.
-- Examples:
--   ∙ "row"  `isInfixOf` "brown"  == True
--   ∙ "asdf" `isInfixOf` "qwerty" == False
--isInfixOf :: String -> String -> Bool
--isInfixOf inf str = or [ inf `isPrefixOf'` suf | suf <- tails' str ]



-- 7- Lists perations

-- Lists can implement the interface of a set.
-- (There is also a module Data.Set).

-- `deleteElem e xs` deletes the first occurence of `e` in `xs`.
-- Examples:
--   deleteElem 2 [1,2,3] == [1,3]
--   deleteElem 2 [1,1,2,3,2] == [1,1,3,2]
--   deleteElem 0 [1] == [1]
deleteElem :: Int -> [Int] -> [Int]
deleteElem  n [] = []
deleteElem  n (x:xs) | n == x = xs
                     | otherwise = x: deleteElem n xs

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Int -> [Int] -> [Int]
deleteAll _ [] = []
deleteAll e (x:xs)
  | e == x = deleteAll e xs
  | otherwise = x : deleteAll e xs

deleteAll' n xs = [e | e <- xs, e /= n]
-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]

deleteLast :: Int -> [Int] -> [Int]
deleteLast = undefined

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the
-- first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] == [1,2,3]
nub' :: [Int] -> [Int]
nub' []     = []
nub' (x:xs) = x : nub' (deleteAll x xs)

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect' :: [Int] -> [Int] -> [Int]
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) ys | x `elem` ys = x : intersect' xs ys
                    | otherwise = intersect' xs ys

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a xs = [i | (i,x) <- zip [0..] xs, x == a]

-- Lists of pairs can implement the interface of a map. (association lists)

-- Examples:
--  lookup "key" [] = Nothing
--  lookup "key" [("key", 34)] = Just 34
--  lookup "key2" [("key1", 2), ("key2", 3), ("key3", 10)] = Just 3
lookup' :: Eq k => k -> [(k, v)] -> Maybe k
lookup' k [] = Nothing
lookup' k ((key,val):xs) | k == key = Just k
                         | otherwise = lookup' k xs

-- 8- HOF

-- Doubling each element of a list using map
doubleList :: [Int] -> [Int]
doubleList xs = map (\x -> x * 2) xs
-- Selecting even numbers from a list using filter
evenList :: [Int] -> [Int]
evenList xs = filter (\x -> x `mod` 2 == 0) xs
-- Summing a list using foldr
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs
-- Checking if all elements in the list are even using all
allEven :: [Int] -> Bool
allEven xs = all even xs
-- Checking if any element in the list is negative using any
hasNegative :: [Int] -> Bool
hasNegative xs = any (< 0) xs

-- map' (+1) [1,4,8] == [2,5,9]
-- map' (*2) [1,4,8] == [2,8,16]
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- filter' even [1,4,8] == [4,8]
-- filter' odd  [1,4,8] == [1]
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x = x: filter' f xs
                 | otherwise = filter' f xs

-- Use any to redefine the function `elem :: Eq a => a -> [a] -> Bool`.
elem'' :: Eq a => a -> [a] -> Bool
elem'' n xs = any (==n) xs
-- elem' 3 [1,2,3,4] == True
-- elem' 6 [1,2,3,4] == False

-- Redefine the functions `any` and `all`
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) | p x = True
              | otherwise = any' p xs
-- any' even [1,2,3] == True
-- any' even [1,5,3] == False

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) | p x = all' p xs
              | otherwise = False
-- all' even [1,2,3] == False
-- all' odd [1,5,3]  == True

--------------------------------------------------------------------------------

-- Define higher-order functions with the following types:
ex1 :: ((a -> a) -> c) -> c
ex1 f = f id

ex2 :: (b -> c) -> (a -> b) -> a -> c
ex2 f g x = f (g x)

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x,y)= f x y

--------------------------------------------------------------------------------
-- Folds:

-- Redefine sum, product using foldl or foldr:
sum'' :: Num a => [a] -> [a]
sum'' xs = [foldr (+) 0 xs]

product'' :: Num a => [a] -> [a]
product'' xs= [foldr (*) 1 xs]


-- Implementaions of foldl and foldr:
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' op e []     = e
foldr'' op e (x:xs) = op x (foldr'' op e xs)

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' op e [] = e
foldl'' op e (x:xs) = foldl'' op (op e x) xs


-- Redefine reverse using foldl.
reverse'' :: [a] -> [a]
reverse'' list = foldl (\acc x -> x : acc) [] list

-- Other exercises:

-- Define a function `filterWords` that filters the words
--   of a sentence using a predicate `p :: String -> Bool`.
-- Hint: use the functions `words` and `unwords`.
-- Examples:
--   filterWords (\w -> not (null w) && head w == 'a') "a b c ab bc de ax aaab" 
--     == "a ab ax aaab"
filterWords :: (String -> Bool) -> String -> String
filterWords pred str = unwords[word | word <- words str, pred(word)]

-- Define a function `mapNested` that applies a given 
--  function to all elements of a list of lists.
-- Examples:
--  mapNested (+1) [[1],[2,3],[4,5,6]] == [[2],[3,4],[5,6,7]]
mapNested :: (a -> b) -> [[a]] -> [[b]]
mapNested f [] = []
mapNested f (x:xs)= [f e | e <- x] : mapNested f xs

-- Define a function `filterMap` that combines filter and map.
--  The evaluation of `filterMap f xs` should evaluate `f x` 
--  for every element `x` of the list `xs`, and
--  - if `f x = Just y`, include `y` in the output list,
--  - if `f x = Nothing`, do not add any element to the output list.
-- Examples:
--  filterMap (\n -> if even n then Just (n`div`2) else Nothing) [1..10] 
--    == [1..5]
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) = case f x of
  Just y -> y : filterMap f xs
  Nothing -> filterMap f xs


-- Define a function `filterIndices :: (Int -> Bool) -> [a] -> [a]` that
--  keeps the elements of a list at indices that satisfy some predicate.
-- Examples:
--  filterIndices even [1,9,1,2,4,6,7,1] == [1,1,4,7]
--  filterIndices odd  [1,9,1,2,4,6,7,1] == [9,2,6,1]
-- Hint: use `zip [0..]`.
filterIndices :: (Int -> Bool) -> [a] -> [a]
filterIndices pred xs = [x | (x, i) <- zip xs [0..], pred i]

--------------------------------------------------------------------------------

-- Bonus exercises: Without using pattern matching or recursion, 
--  define foldr using foldl and foldl using foldr.
foldr''' :: (a -> b -> b) -> b -> [a] -> b
foldr''' = undefined -- use foldl

foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' = undefined -- use foldr

-- 9- Maybe

--data Maybe' a 
--  = Nothing 
--  | Just a
--  deriving (Show, Eq, Ord)

-- Define a function that returns the maximum element of a list.
-- It should return Nothing when the list is empty.
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum []= Nothing
safeMaximum (x:xs) = helper x xs
    where
       helper max [] = Just max
       helper max (e:xs) | e > max    = helper e xs
                        | otherwise = Just max

-- Define a function that returns the greatest
-- of two pairs (x1,x2), (y1,y2), where
--   (x1,x2) <= (y1,y2)  if  (x1 <= y1) and (x2 <= y2)
-- The function should return Nothing when the
-- pairs cannot be compared.
maxPairs :: (Ord a, Ord b) => (a,b) -> (a,b) -> Maybe (a,b)
maxPairs (x,y) (w,z) | x >= w && y >= z = Just (x,y)
                     | x <= w && y <= z = Just (w,z)
                     | otherwise = Nothing
-- Examples:
--  maxPairs (0,0) (1,1)  = Just (1,1)
--  maxPairs (10,0) (5,0) = Just (10,0)
--  maxPairs (1,2) (2,1)  = Nothing

--------------------------------------------------------------------------------
-- Data types

-- Define a datatype Color. 
-- It should have two constructors `NamedColor` and `RGB`.
-- The constructor `NamedColor` should have a `String` valued parameter, indicating the name of the color.
-- The constructor `RGB` should have three parameters of type `Int`.

-- Derive the instances for `Eq` and `Show`.

{-
color1 :: Color
color1 = NamedColor "blue"
color2 :: Color
color2 = RGB 255 0 255

tests = [ show color1 == "NamedColor \"blue\""
        , show color2 == "RGB 255 0 255"
        , color1 /= color2
        ]
-}

--------------------------------------------------------------------------------

-- Define a function `partialSum` that computes the partial sums of a list (the list of sums of prefixes of the input list).
-- Examples:
--  partialSums [] == [0]
--  partialSums [1,2,3,4,5] == [0,1,3,6,10,15]
--  take 100 (partialSums [1..]) 
--      == take 100 [ i*(i+1)`div`2 | i <- [0..]]
partialSums :: Num a => [a] -> a
partialSums = undefined
--partialSums [] = [0]  -- Base case: empty list, return [0]
--partialSums xs = 0 : [sum (take n xs) | n <- [1..length xs]]
--partialSums = foldl (\acc x -> acc ++ [x + last acc]) [0]

-- Define a function that checks whether a 
-- string is a substring of another string.
isSubstring :: String -> String -> Bool
isSubstring sub str = isInfixOf sub str
