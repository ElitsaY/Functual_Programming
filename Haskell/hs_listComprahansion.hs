import Prelude
import Data.List

-- брой делители на число с list_comprehansion 
getDivisors:: Int -> Int
getDivisors x = length [y | y <- [1..x] , x `mod` y == 0]

sumOfDivisors :: Int -> Int
sumOfDivisors x = sum [d | d <- [1..x], x `mod` d == 0]

isPrime:: Int -> Bool
isPrime x = getDivisors x == 0

prime :: Integral a => a -> Bool
prime n = null [ x | x<-[2..sqn], n `mod` x == 0 ]
  where sqn = floor $ sqrt $ fromIntegral n

--descartes - декартово произведение на два списъка

decartes :: [a]->[a]->[(a, a)]
decartes xs ys = [(x, y) | x <- xs , y <- ys]

-- Да се генерира безкрайния списък от прости числа по метода на ситото на Ератостен.
eratosten = sieve [2..]
    where sieve (x : xs) = x : sieve [ y | y <- xs, x `mod` y /= 0]

-- Да се генерира безкраен списък, който съдържа всички наредени двойки от естествени числа.
-- 1:1 2:1
pairs = [ (x,diag-x) | diag<-[0..], x<-[0..diag] ]

--Да се напише функция compress, която по списък от стойности връща списък от наредени двойки от
-- вида (<стойност>, <брой последователни срещания>):

-- compress :: [a] -> 
compress xs = nub $ [(x, length $ filter (==x) xs) | x <- xs]

-- Да се напише функция makeSet, която по даден списък връща всички негови уникални елементи (редът им няма значение):
--makeSet [1,1,2,3,3,3,4,2,2,2,1,1] -> [1,2,3,4]
--makeSet "abba" -> "ab"

makeSet:: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x : xs) = x : makeSet [ y | y <- xs, y /= x]

--Да се напише функция histogram, която за всяка уникална стойност от даден списък 
--връща списък от наредени двойки от вида (<стойност>, <общ брой срещания>):
--histogram [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,4),(2,4),(3,3),(4,1)]

histogram :: Eq a => [a] -> [(a, Int)]
histogram x =  makeSet [(y, length $ filter (==y) x) | y <- x]

--Да се напише функция maxDistance, която получава списък от точки (наредени двойки (Double, Double)) 
--и връща дължината на най-дългата отсечка между някои две от тях.
--maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)] -> 5.0

maxDistance :: [(Double, Double)] -> Double
maxDistance x = sqrt $ maximum [(a - c)* (a - c) + (b - d) * (b - d) | (a, b) <- x , (c, d)<- x ]