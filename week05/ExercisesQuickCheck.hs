module ExercisesSpec where

import Test.QuickCheck
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

-- and'
and' [] = True
and' (b : bs) = b && and' bs

prop_And xs = and' xs == and xs
  where types = xs::[Bool]

-- concat'
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

prop_Concat xs = concat' xs == concat xs
  where types = xs::[[Int]]

-- replicate
prop_Replicate =
  forAll (choose (0, 9)) $ \x ->
  forAll (choose (0, 9)) $ \y ->
    replicate x y == replicate (x :: Int) (y :: Int)

-- (!!)
prop_BangBang xs =
  forAll (choose (0, length xs-1)) $ \x ->
    not (null xs) ==> (!!) xs x == (!!) (xs :: [Int]) x

prop_positive (Positive x) = (\a -> a > 0) x == True

-- properties
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

prop_RevId xs = reverse xs == xs
  where types = xs::[Int]

ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

-- conditional properties
-- <condition> ==> <property>
prop_Insert x xs = ordered xs ==> ordered (insert x xs)
  where types = x::Int

-- quantified properties
-- forAll <generator> $ \<pattern> -> <property>
prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
  where types = x::Int

-- counting trivial properties
-- <condition> `trivial` <property>
-- prop_Insert3 x xs = ordered xs ==> null xs `trivial` ordered (insert x xs)
--   where types = x::Int

-- classifying test cases
-- classify <condition> <string>$ <property>
prop_Insert4 x xs =
  ordered xs ==>
    classify (ordered (x:xs)) "at-head"$
    classify (ordered (xs++[x])) "at-tail"$
    ordered (insert x xs)
  where types = x::Int

-- collecting data values
-- collect <expression>$ <property>
prop_Insert5 x xs =
  ordered xs ==> collect (length xs)$
                 ordered (insert x xs)
  where types = x::Int

-- combining observations
prop_Insert6 x xs =
  ordered xs ==>
          collect (length xs)$
          classify (ordered (x:xs)) "at-head"$
          classify (ordered (xs++[x])) "at-tail"$
          ordered (insert x xs)
  where types = x::Int
