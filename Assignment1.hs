import Test.QuickCheck

{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power negative argument!"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k    | k == 0 = 1
                  |otherwise = k+1 


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k 
   | k < 0 = error "power negative argument!"
power1 n k = product [n | x <- [1..k]]

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k 
   | k < 0 = error "power negative argument!"
power2 n 0 = 1
power2 n k  | odd k =  n * power2 n (k-1)
            | otherwise = power2 (n * n) (k `div` 2)

-- D -------------------------
{- 
<Describe your test cases here>
We are intrested in testing a few different cases. 
We wan't to look at what happens when we feed the function negative numbers (which we will fix in prop_powers')
As well as feed it both odd and even numbers to make sure that the handling gets done correctly
This is done with list comprehensions
-}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (n^k == power n k) && (n^k == power1 n k) && (n^k == power2 n k)


-- Using soem and clauses to make sure all of these compile to true (which they should if my implementations are correct-
-- Use the prop_powers function together with a list comprehension of values to determine if my functions holds up for different values

powerTest :: Bool
powerTest = and [prop_powers n k | n <- [-20.. 20], k <- [-20..20]]

-- Confused what to do here but from what I understand this is what needs fixing
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (n^k' == power n k') && (n^k' == power1 n k') && (n^k' == power2 n k')
   where k' = abs k

powerTest' :: Bool
powerTest' = and [prop_powers' n k | n <- [-20.. 20], k <- [-20..20]]