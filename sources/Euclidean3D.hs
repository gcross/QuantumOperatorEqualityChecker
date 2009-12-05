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

module Euclidean3D where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3472:<< Import needed modules >>
import Data.Complex

import Function
-- @-node:gcross.20091204093401.3472:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3465:Instances
-- @+node:gcross.20091204093401.3469:Projectable (,,) XYZ
instance Projectable ThreeDimensions XYZ where
    project X (value,_,_) = value
    project Y (_,value,_) = value
    project Z (_,_,value) = value
-- @-node:gcross.20091204093401.3469:Projectable (,,) XYZ
-- @-node:gcross.20091204093401.3465:Instances
-- @+node:gcross.20091204093401.3475:Types
-- @+node:gcross.20091204093401.3476:XYZ
data XYZ = X | Y | Z deriving (Eq,Show,Enum,Bounded)
-- @-node:gcross.20091204093401.3476:XYZ
-- @+node:gcross.20091204093401.3485:ThreeDimensions
type ThreeDimensions = (Complex Double,Complex Double,Complex Double)
-- @nonl
-- @-node:gcross.20091204093401.3485:ThreeDimensions
-- @-node:gcross.20091204093401.3475:Types
-- @+node:gcross.20091204093401.3477:Operators
-- @+node:gcross.20091204093401.3478:r_
r_ :: XYZ -> FunctionTransformer ThreeDimensions XYZ
r_ x = (Projector x :*:)

r_x = r_ X
r_y = r_ Y
r_z = r_ Z
-- @-node:gcross.20091204093401.3478:r_
-- @+node:gcross.20091204093401.3479:p_
p_ :: XYZ -> FunctionTransformer ThreeDimensions XYZ
p_ j = c(-i) . d j

p_x = p_ X
p_y = p_ Y
p_z = p_ Z
-- @-node:gcross.20091204093401.3479:p_
-- @-node:gcross.20091204093401.3477:Operators
-- @-others
-- @-node:gcross.20091204093401.3427:@thin Euclidean3D.hs
-- @-leo
