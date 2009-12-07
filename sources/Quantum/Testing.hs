-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.3596:@thin Testing.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.3599:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- @-node:gcross.20091204093401.3599:<< Language extensions >>
-- @nl

module Quantum.Testing where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.3600:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Arrow
import Control.Monad

import Data.Complex

import Debug.Trace

import Quantum.Euclidean3D
import Quantum.Function
import Quantum.Function.Transformers

import System.Random

import Test.HUnit
import Test.QuickCheck
-- @-node:gcross.20091204093401.3600:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3628:Classes
-- @+node:gcross.20091204093401.3629:AlmostEq
infix 4 ~=, /~
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = min (abs (x-y)) (abs (x-y) / (abs (x+y) + 1e-100)) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)

x /~ y = not (x ~= y)
-- @-node:gcross.20091204093401.3629:AlmostEq
-- @-node:gcross.20091204093401.3628:Classes
-- @+node:gcross.20091204093401.3622:Instances
-- @+node:gcross.20091204093401.3624:Random XYZ
instance Random XYZ where
    randomR (lo,hi) = first toEnum . randomR (fromEnum lo,fromEnum hi)
    random = randomR (X,Z)
-- @-node:gcross.20091204093401.3624:Random XYZ
-- @+node:gcross.20091204093401.3625:Random ()
instance Random () where
    randomR _ g = ((),g)
    random g = ((),g)
-- @-node:gcross.20091204093401.3625:Random ()
-- @+node:gcross.20091204093401.3655:Random (Complex a)
instance (Random a, RealFloat a) => Random (Complex a) where
    randomR (lo_r :+ lo_i,hi_r :+ hi_i) g0 =
        let (r,g1) = randomR (lo_r,hi_r) g0
            (i,g2) = randomR (lo_i,hi_i) g1
        in (r :+ i,g2)
    random g0 =
        let (r,g1) = random g0
            (i,g2) = random g1
        in (r :+ i,g2)
-- @-node:gcross.20091204093401.3655:Random (Complex a)
-- @-node:gcross.20091204093401.3622:Instances
-- @+node:gcross.20091204093401.3615:Generators
-- @+node:gcross.20091206183506.1344:XYZ
instance Arbitrary XYZ where
    arbitrary = elements [X,Y,Z]
-- @-node:gcross.20091206183506.1344:XYZ
-- @+node:gcross.20091204093401.3616:Complex Double
instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = liftM (:+ 0) arbitrary
-- @-node:gcross.20091204093401.3616:Complex Double
-- @+node:gcross.20091204093401.3617:Function
instance (Floating result
         ,Bounded index
         ,Random result
         ,Random index
         ,Projectable domain index result
         ,Arbitrary domain
         ,Arbitrary result
         )
    => Arbitrary (Function domain index result)
  where
    arbitrary = sized $ \size ->
        if size <= 1 
            then oneof
                [fmap Constant arbitrary
                ,elements [Sin,Cos,id] >>= \constructor ->
                    fmap (constructor . Projector) (choose (minBound,maxBound))
                ]
            else oneof
                [elements [(:+:),(:-:),(:*:)]
                    >>= \constructor ->
                        liftM2 constructor 
                            (resize (size `div` 2) arbitrary)
                            (resize (size `div` 2) arbitrary)
                ]
-- @-node:gcross.20091204093401.3617:Function
-- @-node:gcross.20091204093401.3615:Generators
-- @+node:gcross.20091204093401.3633:Functions
-- @+node:gcross.20091204093401.3634:echo
echo x = trace (show x) x
-- @-node:gcross.20091204093401.3634:echo
-- @+node:gcross.20091204093401.3635:assertAlmostEqual
assertAlmostEqual :: (Show a, AlmostEq a) => String -> a -> a -> Assertion
assertAlmostEqual message x y
    | x ~= y     = return ()
    | otherwise  = assertFailure $ message ++ " (" ++ show x ++ " /~ " ++ show y ++ ")"
-- @-node:gcross.20091204093401.3635:assertAlmostEqual
-- @-node:gcross.20091204093401.3633:Functions
-- @-others
-- @-node:gcross.20091204093401.3596:@thin Testing.hs
-- @-leo
