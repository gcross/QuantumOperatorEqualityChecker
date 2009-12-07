-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.3565:@thin Transformers.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.3587:<< Language extensions >>
-- @-node:gcross.20091204093401.3587:<< Language extensions >>
-- @nl

module Quantum.Function.Transformers where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3583:<< Import needed modules >>
import Control.Applicative
import Control.Applicative.Infix

import Data.Complex

import Quantum.Function
-- @-node:gcross.20091204093401.3583:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3579:Types
-- @+node:gcross.20091204093401.3581:FunctionTransformer
type FunctionTransformer domain index a =
        Function domain index a ->
        Function domain index a 
-- @-node:gcross.20091204093401.3581:FunctionTransformer
-- @-node:gcross.20091204093401.3579:Types
-- @+node:gcross.20091204093401.3571:Functions
-- @+node:gcross.20091204093401.3573:*|
infixl 7 *|
(*|) :: (Floating a, Projectable domain index a) =>
     a ->
     FunctionTransformer domain index a ->
     FunctionTransformer domain index a
(*|) value transformer f = ((Constant value) :*: (transformer f))
-- @-node:gcross.20091204093401.3573:*|
-- @+node:gcross.20091206113753.1372:|^
infixl 8 |^
(|^) :: (Floating a, Projectable domain index a) =>
     FunctionTransformer domain index a ->
     Int ->
     FunctionTransformer domain index a
(|^) transformer exponent
  | exponent < 0
    = error "Negative exponents not presently supported."
  | exponent == 0
    = id
  | exponent == 1
    = transformer
  | otherwise
    = transformer . transformer|^(exponent-1)
-- @-node:gcross.20091206113753.1372:|^
-- @+node:gcross.20091204093401.3574:d
d _ (Constant value) = Constant (fromIntegral 0)
d x (Projector y)
    | y == x    = Constant (fromIntegral 1)
    | otherwise = Constant (fromIntegral 0)
d x (f :+: g) = d x f :+: d x g
d x (f :-: g) = d x f :-: d x g
d x (f :*: g) = (d x f :*: g) :+: (f :*: d x g)
d x (f :/: g) = (d x f :/: g) :-: (f :/: (d x g :*: d x g))
d x (Sqrt f) = (Constant (1/2)) :*: d x f :/: (Sqrt f)
d x (Sin f) = Cos f :*: d x f
d x (Cos f) = (Constant (-1)) :*: Sin f :*: d x f
-- @-node:gcross.20091204093401.3574:d
-- @+node:gcross.20091204093401.3575:~~
infixl 6 ~~
(~~) :: (Floating a, Projectable domain index a) =>
        FunctionTransformer domain index a ->
        FunctionTransformer domain index a ->
        FunctionTransformer domain index a
x ~~ y = x.y |-| y.x
-- @-node:gcross.20091204093401.3575:~~
-- @+node:gcross.20091204093401.3588:|+|
infixl 6 |+|
(|+|) :: (Floating a, Projectable domain index a) =>
         FunctionTransformer domain index a ->
         FunctionTransformer domain index a ->
         FunctionTransformer domain index a
(|+|) = liftA2 (:+:)
-- @-node:gcross.20091204093401.3588:|+|
-- @+node:gcross.20091204093401.3591:|-|
infixl 6 |-|
(|-|) :: (Floating a, Projectable domain index a) =>
         FunctionTransformer domain index a ->
         FunctionTransformer domain index a ->
         FunctionTransformer domain index a
(|-|) = liftA2 (:-:)
-- @-node:gcross.20091204093401.3591:|-|
-- @-node:gcross.20091204093401.3571:Functions
-- @-others
-- @-node:gcross.20091204093401.3565:@thin Transformers.hs
-- @-leo
