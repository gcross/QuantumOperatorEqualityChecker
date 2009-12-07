-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.3427:@thin Euclidean3D.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.3471:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- @nonl
-- @-node:gcross.20091204093401.3471:<< Language extensions >>
-- @nl

module Quantum.Euclidean3D where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3472:<< Import needed modules >>
import Data.Complex

import Quantum.Function
import Quantum.Function.Transformers
-- @-node:gcross.20091204093401.3472:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3465:Instances
-- @+node:gcross.20091204093401.3469:Projectable (,,) XYZ
instance Projectable (ThreeDimensions result) XYZ result where
    project X (value,_,_) = value
    project Y (_,value,_) = value
    project Z (_,_,value) = value
-- @-node:gcross.20091204093401.3469:Projectable (,,) XYZ
-- @-node:gcross.20091204093401.3465:Instances
-- @+node:gcross.20091204093401.3475:Types
-- @+node:gcross.20091204093401.3476:XYZ
data XYZ = X | Y | Z deriving (Eq,Show,Enum,Bounded)
-- @-node:gcross.20091204093401.3476:XYZ
-- @+node:gcross.20091206113753.1497:ThreeDimensions
type ThreeDimensions result = (result,result,result)
-- @-node:gcross.20091206113753.1497:ThreeDimensions
-- @+node:gcross.20091206192830.1384:QuantumOperator
type QuantumOperator = FunctionTransformer (ThreeDimensions (Complex Double)) XYZ (Complex Double)
-- @-node:gcross.20091206192830.1384:QuantumOperator
-- @-node:gcross.20091204093401.3475:Types
-- @+node:gcross.20091204093401.3477:Operators
-- @+node:gcross.20091204093401.3478:r_
r_ :: Floating result => XYZ -> FunctionTransformer (ThreeDimensions result) XYZ result
r_ x = (Projector x :*:)

r_x,r_y,r_z :: QuantumOperator
r_x = r_ X
r_y = r_ Y
r_z = r_ Z

-- @-node:gcross.20091204093401.3478:r_
-- @+node:gcross.20091204093401.3479:p_
p_ :: (Floating a, RealFloat a) =>
      XYZ -> FunctionTransformer (ThreeDimensions (Complex a)) XYZ (Complex a)
p_ j = (-i) *| d j

p_x,p_y,p_z :: QuantumOperator
p_x = p_ X
p_y = p_ Y
p_z = p_ Z
-- @-node:gcross.20091204093401.3479:p_
-- @-node:gcross.20091204093401.3477:Operators
-- @-others
-- @-node:gcross.20091204093401.3427:@thin Euclidean3D.hs
-- @-leo
