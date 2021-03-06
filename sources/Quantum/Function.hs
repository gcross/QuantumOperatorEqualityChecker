-- @+leo-ver=4-thin
-- @+node:gcross.20090727161338.1228:@thin Function.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2956:<< Language extensions >>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- @-node:gcross.20091204093401.2956:<< Language extensions >>
-- @nl

module Quantum.Function where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.1597:<< Import needed modules >>
import Control.Applicative.Infix

import Data.Complex
-- @-node:gcross.20091204093401.1597:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3506:Values
-- @+node:gcross.20091204093401.3507:i
i :: (Floating a, RealFloat a) => Complex a
i = (fromIntegral 0) :+ (fromIntegral 1)
-- @-node:gcross.20091204093401.3507:i
-- @-node:gcross.20091204093401.3506:Values
-- @+node:gcross.20091204093401.1598:Classes
-- @+node:gcross.20091204093401.1600:Projectable
class Eq index => Projectable domain index a where
    project :: index -> domain -> a
-- @-node:gcross.20091204093401.1600:Projectable
-- @-node:gcross.20091204093401.1598:Classes
-- @+node:gcross.20091204093401.2964:Instances
-- @+node:gcross.20091204093401.2965:Projectable (Complex Double) ()
instance Projectable a () a where
    project () = id
-- @-node:gcross.20091204093401.2965:Projectable (Complex Double) ()
-- @-node:gcross.20091204093401.2964:Instances
-- @+node:gcross.20091204093401.1596:Types
-- @+node:gcross.20091204093401.1599:Function
infixl 7 :*:, :/:
infixl 6 :+:, :-:
data (Floating result, Projectable domain index result) =>
     Function domain index result =
    Constant result
  | Projector index
  | (Function domain index result) :+: (Function domain index result)
  | (Function domain index result) :-: (Function domain index result)
  | (Function domain index result) :*: (Function domain index result)
  | (Function domain index result) :/: (Function domain index result)
  | Sqrt (Function domain index result)
  | Sin (Function domain index result)
  | Cos (Function domain index result)
  deriving (Eq,Show)
-- @-node:gcross.20091204093401.1599:Function
-- @-node:gcross.20091204093401.1596:Types
-- @+node:gcross.20091204093401.3576:Functions
-- @+node:gcross.20091204093401.3578:($>)
infixl 5 $>

($>) :: (Floating result, Projectable domain index result) =>
        Function domain index result ->
        domain -> result
($>) (Constant value) = const value
($>) (Projector index) = project index
($>) (f :+: g) = (f $>) <^(+)^> (g $>)
($>) (f :-: g) = (f $>) <^(-)^> (g $>)
($>) (f :*: g) = (f $>) <^(*)^> (g $>)
($>) (f :/: g) = (f $>) <^(/)^> (g $>)
($>) (Sqrt f) = sqrt . (f $>)
($>) (Sin f) = sin . (f $>)
($>) (Cos f) = cos . (f $>)
-- @-node:gcross.20091204093401.3578:($>)
-- @-node:gcross.20091204093401.3576:Functions
-- @-others
-- @-node:gcross.20090727161338.1228:@thin Function.hs
-- @-leo
