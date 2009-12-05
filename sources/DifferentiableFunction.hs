-- @+leo-ver=4-thin
-- @+node:gcross.20090727161338.1228:@thin DifferentiableFunction.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2956:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- @-node:gcross.20091204093401.2956:<< Language extensions >>
-- @nl

module DifferentiableFunction where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.1597:<< Import needed modules >>
import Control.Applicative.Infix

import Data.Complex
-- @-node:gcross.20091204093401.1597:<< Import needed modules >>
-- @nl

-- @+others
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
-- @+node:gcross.20091204093401.1599:DifferentiableFunction
data Projectable domain index => Function domain index =
    Zero
  | One
  | Constant (Complex Double)
  | Projector index
  | (Function domain index) :+: (Function domain index)
  | (Function domain index) :*: (Function domain index)
  deriving (Eq)
-- @-node:gcross.20091204093401.1599:DifferentiableFunction
-- @+node:gcross.20091204093401.2959:FunctionTransformer
type FunctionTransformer domain index = Function domain index -> Function domain index 
-- @nonl
-- @-node:gcross.20091204093401.2959:FunctionTransformer
-- @-node:gcross.20091204093401.1596:Types
-- @+node:gcross.20091204093401.1601:Functions
-- @+node:gcross.20091204093401.2957:|+|
Zero |+| g = g
f |+| Zero = f
f |+| g = f :+: f
-- @-node:gcross.20091204093401.2957:|+|
-- @+node:gcross.20091204093401.2958:|*|
Zero |*| _ = Zero
_ |*| Zero = Zero
f |*| One = f
One |*| g = g
f |*| g = f :*: g
-- @-node:gcross.20091204093401.2958:|*|
-- @+node:gcross.20091204093401.1603:p
p :: Projectable domain index => index -> Function domain index
p = Projector
-- @-node:gcross.20091204093401.1603:p
-- @+node:gcross.20091204093401.1602:($>)
($>) :: Projectable domain index =>
        Function domain index ->
        domain -> Complex Double
($>) Zero = const 0
($>) One = const 1
($>) (Constant value) = const value
($>) (Projector index) = project index
($>) (f :+: g) = (f $>) <^(+)^> (g $>)
($>) (Zero :*: _) = const 0
($>) (_ :*: Zero) = const 0
($>) (f :*: g) = (f $>) <^(*)^> (g $>)
-- @-node:gcross.20091204093401.1602:($>)
-- @+node:gcross.20091204093401.1604:m
m :: Projectable domain index => index -> FunctionTransformer domain index
m = (|*|) . Projector
-- @-node:gcross.20091204093401.1604:m
-- @+node:gcross.20091204093401.1605:d
d x Zero = Zero
d x (Constant value) = Zero
d x (Projector y)
    | y == x    = One
    | otherwise = Zero
d x (f :+: g) = (d x f) |+| (d x g)
d x (f :*: g) = (d x f |*| g) |+| (f |*| d x g)
-- @-node:gcross.20091204093401.1605:d
-- @-node:gcross.20091204093401.1601:Functions
-- @-others
-- @-node:gcross.20090727161338.1228:@thin DifferentiableFunction.hs
-- @-leo
