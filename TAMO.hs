module TAMO

  where

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> x = True

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y