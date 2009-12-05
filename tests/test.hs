-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.2948:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2949:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091204093401.2949:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.2950:<< Import needed modules >>
import Control.Arrow
import Control.Monad

import Data.Complex
import Data.List

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import DifferentiableFunction
-- @-node:gcross.20091204093401.2950:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.2951:Functions
-- @+node:gcross.20091204093401.2952:echo
echo x = trace (show x) x
-- @-node:gcross.20091204093401.2952:echo
-- @+node:gcross.20091204093401.2979:simple
simple :: Projectable (Complex Double) () =>
          Function (Complex Double) () ->
          Function (Complex Double) ()
simple = id
-- @-node:gcross.20091204093401.2979:simple
-- @-node:gcross.20091204093401.2951:Functions
-- @+node:gcross.20091204093401.2971:Generators
-- @+node:gcross.20091204093401.2972:Complex Double
instance Arbitrary (Complex Double) where
    arbitrary = liftM2 (:+) arbitrary arbitrary
-- @-node:gcross.20091204093401.2972:Complex Double
-- @-node:gcross.20091204093401.2971:Generators
-- @+node:gcross.20091204093401.3387:Classes
-- @+node:gcross.20091204093401.3388:AlmostEq
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = abs (x-y) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)

x /~ y = not (x ~= y)
-- @-node:gcross.20091204093401.3388:AlmostEq
-- @-node:gcross.20091204093401.3387:Classes
-- @+node:gcross.20091204093401.3391:Helpers
-- @+node:gcross.20091204093401.3392:assertAlmostEqual
assertAlmostEqual :: (Show a, AlmostEq a) => String -> a -> a -> Assertion
assertAlmostEqual message x y
    | x ~= y     = return ()
    | otherwise  = assertFailure $ message ++ " (" ++ show x ++ " /~ " ++ show y ++ ")"
-- @-node:gcross.20091204093401.3392:assertAlmostEqual
-- @-node:gcross.20091204093401.3391:Helpers
-- @+node:gcross.20091204093401.2962:Types
-- @+node:gcross.20091204093401.2963:TF
type TF = Function (Complex Double) ()
-- @-node:gcross.20091204093401.2963:TF
-- @+node:gcross.20091204093401.2977:CFn
type CFn = Complex Double -> Complex Double
-- @nonl
-- @-node:gcross.20091204093401.2977:CFn
-- @-node:gcross.20091204093401.2962:Types
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091204093401.2953:<< Tests >>
    -- @+others
    -- @+node:gcross.20091204093401.2954:DifferentiableFunction
    [testGroup "DifferentiableFunction"
        -- @    @+others
        -- @+node:gcross.20091204093401.2955:($>)
        [testGroup "($>)"
            -- @    @+others
            -- @+node:gcross.20091204093401.2961:Zero
            [testProperty "Zero $> x == 0" $ (== 0) . (simple Zero $>)
            -- @-node:gcross.20091204093401.2961:Zero
            -- @+node:gcross.20091204093401.2974:One
            ,testProperty "One $> x == 1" $ (== 1) . (simple One $>)

            -- @-node:gcross.20091204093401.2974:One
            -- @+node:gcross.20091204093401.2976:Constant
            ,testProperty "Constant c $> x == c" $
                \c x -> simple (Constant c) $> x == c
            -- @-node:gcross.20091204093401.2976:Constant
            -- @+node:gcross.20091204093401.2978:Sum over constants
            ,testProperty "Sum over constants" $
                \c1 c2 x -> (simple (Constant c1 :+: Constant c2) $> x) ~= (c1 + c2)
            -- @-node:gcross.20091204093401.2978:Sum over constants
            -- @+node:gcross.20091204093401.3394:Multiplication over constants
            ,testProperty "Multiplication over constants" $
                \c1 c2 x -> (simple (Constant c1 :*: Constant c2) $> x) ~= (c1 * c2)
            -- @-node:gcross.20091204093401.3394:Multiplication over constants
            -- @+node:gcross.20091204093401.3396:Product over sums
            ,testProperty "Product over sums" $
                \c1 c2 c3 c4 x -> (simple (
                                     (Constant c1 :+: Constant c2)
                                        :*:
                                     (Constant c3 :+: Constant c4)
                                   ) $> x) ~= ((c1 + c2) * (c3 + c4))
            -- @-node:gcross.20091204093401.3396:Product over sums
            -- @+node:gcross.20091204093401.3398:Sum over products
            ,testProperty "Sum over products" $
                \c1 c2 c3 c4 x -> (simple (
                                     (Constant c1 :*: Constant c2)
                                        :+:
                                     (Constant c3 :*: Constant c4)
                                   ) $> x) ~= ((c1 * c2) + (c3 * c4))
            -- @-node:gcross.20091204093401.3398:Sum over products
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20091204093401.2955:($>)
        -- @-others
        ]
    -- @-node:gcross.20091204093401.2954:DifferentiableFunction
    -- @-others
    -- @-node:gcross.20091204093401.2953:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091204093401.2948:@thin test.hs
-- @-leo
