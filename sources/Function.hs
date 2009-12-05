-- @+leo-ver=4-thin
-- @+node:gcross.20090727161338.1228:@thin Function.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2956:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- @-node:gcross.20091204093401.2956:<< Language extensions >>
-- @nl

module Function where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.1597:<< Import needed modules >>
import Control.Applicative.Infix

import Data.Complex
-- @-node:gcross.20091204093401.1597:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3506:Values
-- @+node:gcross.20091204093401.3507:i
i :: Complex Double
i = 0 :+ 1
-- @-node:gcross.20091204093401.3507:i
-- @-node:gcross.20091204093401.3506:Values
-- @+node:gcross.20091204093401.1598:Classes
-- @+node:gcross.20091204093401.1600:Projectable
class Eq index => Projectable domain index where
    project :: index -> domain -> Complex Double
-- @-node:gcross.20091204093401.1600:Projectable
-- @-node:gcross.20091204093401.1598:Classes
-- @+node:gcross.20091204093401.2964:Instances
-- @+node:gcross.20091204093401.2965:Projectable (Complex Double) ()
instance Projectable (Complex Double) () where
    project () = id
-- @-node:gcross.20091204093401.2965:Projectable (Complex Double) ()
-- @-node:gcross.20091204093401.2964:Instances
-- @+node:gcross.20091204093401.1596:Types
-- @+node:gcross.20091204093401.1599:Function
data Projectable domain index => Function domain index =
    Constant (Complex Double)
  | Projector index
  | (Function domain index) :+: (Function domain index)
  | (Function domain index) :-: (Function domain index)
  | (Function domain index) :*: (Function domain index)
  deriving (Eq,Show)
-- @-node:gcross.20091204093401.1599:Function
-- @+node:gcross.20091204093401.2959:FunctionTransformer
type FunctionTransformer domain index = Function domain index -> Function domain index 
-- @nonl
-- @-node:gcross.20091204093401.2959:FunctionTransformer
-- @-node:gcross.20091204093401.1596:Types
-- @+node:gcross.20091204093401.1601:Functions
-- @+node:gcross.20091204093401.1602:($>)
($>) :: Projectable domain index =>
        Function domain index ->
        domain -> Complex Double
($>) (Constant value) = const value
($>) (Projector index) = project index
($>) (f :+: g) = (f $>) <^(+)^> (g $>)
($>) (f :-: g) = (f $>) <^(-)^> (g $>)
($>) (f :*: g) = (f $>) <^(*)^> (g $>)
-- @-node:gcross.20091204093401.1602:($>)
-- @+node:gcross.20091204093401.3508:c
c :: Projectable domain index =>
     Complex Double ->
     FunctionTransformer domain index
c = (:*:) . Constant
-- @-node:gcross.20091204093401.3508:c
-- @+node:gcross.20091204093401.1604:m
m :: Projectable domain index => index -> FunctionTransformer domain index
m = (:*:) . Projector
-- @-node:gcross.20091204093401.1604:m
-- @+node:gcross.20091204093401.1605:d
d _ (Constant value) = Constant 0
d x (Projector y)
    | y == x    = Constant 1
    | otherwise = Constant 0
d x (f :+: g) = (d x f) :+: (d x g)
d x (f :-: g) = (d x f) :-: (d x g)
d x (f :*: g) = (d x f :*: g) :+: (f :*: d x g)
-- @-node:gcross.20091204093401.1605:d
-- @+node:gcross.20091204093401.3489:][
(~~) :: Projectable domain index =>
        FunctionTransformer domain index ->
        FunctionTransformer domain index ->
        FunctionTransformer domain index
x ~~ y = (x.y) <^(:-:)^> (y.x)
-- @-node:gcross.20091204093401.3489:][
-- @-node:gcross.20091204093401.1601:Functions
-- @-others
-- @-node:gcross.20090727161338.1228:@thin Function.hs
-- @-leo
