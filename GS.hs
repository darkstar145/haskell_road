module GS 

where 

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer 
ld n = ldf 2 n 

ldf :: Integer -> Integer -> Integer 
ldf k n | divides k n = k 
        | k^2 >  n    = n 
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False 
         | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

min' :: Int -> Int -> Int 
min' x y | x <= y    = x
         | otherwise = y

-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- Exercise 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) | x == y = ys
                   | otherwise = y : removeFst x ys

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- srtInts using `let`
srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
  m = mnmInt xs
  in m : (srtInts' (removeFst m xs))
                   
average :: [Int] -> Rational 
average [] = error "empty list" 
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' [] = 0 
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0 
length' (x:xs) = 1 + length' xs

-- Exercise 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
               | otherwise = count c xs

-- Exercise 1.14
copy :: Char -> Int -> String
copy c 0 = []
copy c n = c : copy c (n-1)

-- To blowup a string, copy the first character n=1 times, then n=2, then n...
blowup :: String -> String
blowup xs = blowup' xs 1

blowup' :: String -> Int -> String
blowup' [] n = []
blowup' (x:xs) n = (copy x n) ++ blowup' xs (n+1) 

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys 

factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n

primes0 :: [Integer]
primes0 = filter prime0 [2..]    

ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p 
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ldp n == n

a = 3
b = 4 
f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

g :: Integer -> Integer 
g 0     = 1
g x = 2 * g (x-1)

h1 :: Integer -> Integer 
h1 0 = 0
h1 x = 2 * (h1 x) 

h2 :: Integer -> Integer 
h2 0 = 0
h2 x = h2 (x+1) 
