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
type FunctionTransformer domain index = Function domain index -> Function domain index 
-- @nonl
-- @-node:gcross.20091204093401.3581:FunctionTransformer
-- @-node:gcross.20091204093401.3579:Types
-- @+node:gcross.20091204093401.3571:Functions
-- @+node:gcross.20091204093401.3573:*:
(*|) :: Projectable domain index =>
     Complex Double ->
     FunctionTransformer domain index ->
     FunctionTransformer domain index
(*|) value transformer f = ((Constant value) :*: (transformer f))
-- @-node:gcross.20091204093401.3573:*:
-- @+node:gcross.20091204093401.3574:d
d _ (Constant value) = Constant 0
d x (Projector y)
    | y == x    = Constant 1
    | otherwise = Constant 0
d x (f :+: g) = (d x f) :+: (d x g)
d x (f :-: g) = (d x f) :-: (d x g)
d x (f :*: g) = (d x f :*: g) :+: (f :*: d x g)
-- @-node:gcross.20091204093401.3574:d
-- @+node:gcross.20091204093401.3575:~~
(~~) :: Projectable domain index =>
        FunctionTransformer domain index ->
        FunctionTransformer domain index ->
        FunctionTransformer domain index
x ~~ y = (x.y) |-| (y.x)
-- @-node:gcross.20091204093401.3575:~~
-- @+node:gcross.20091204093401.3588:|+|
(|+|) :: Projectable domain index =>
         FunctionTransformer domain index ->
         FunctionTransformer domain index ->
         FunctionTransformer domain index
(|+|) = liftA2 (:+:)
-- @-node:gcross.20091204093401.3588:|+|
-- @+node:gcross.20091204093401.3591:|-|
(|-|) :: Projectable domain index =>
         FunctionTransformer domain index ->
         FunctionTransformer domain index ->
         FunctionTransformer domain index
(|-|) = liftA2 (:-:)
-- @-node:gcross.20091204093401.3591:|-|
-- @-node:gcross.20091204093401.3571:Functions
-- @-others
-- @-node:gcross.20091204093401.3565:@thin Transformers.hs
-- @-leo
