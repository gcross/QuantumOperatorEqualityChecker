-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.2948:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2949:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- @nonl
-- @-node:gcross.20091204093401.2949:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.2950:<< Import needed modules >>
import Control.Arrow
import Control.Monad

import Data.Complex
import Data.List

import Debug.Trace

import System.Random

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Euclidean3D
import Function
-- @-node:gcross.20091204093401.2950:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.2951:Functions
-- @+node:gcross.20091204093401.2952:echo
echo x = trace (show x) x
-- @-node:gcross.20091204093401.2952:echo
-- @+node:gcross.20091204093401.3418:assertAlmostEqual
assertAlmostEqual :: (Show a, AlmostEq a) => String -> a -> a -> Assertion
assertAlmostEqual message x y
    | x ~= y     = return ()
    | otherwise  = assertFailure $ message ++ " (" ++ show x ++ " /~ " ++ show y ++ ")"
-- @-node:gcross.20091204093401.3418:assertAlmostEqual
-- @-node:gcross.20091204093401.2951:Functions
-- @+node:gcross.20091204093401.2971:Generators
-- @+node:gcross.20091204093401.2972:Complex Double
instance Arbitrary (Complex Double) where
    arbitrary = liftM2 (:+) arbitrary arbitrary
-- @-node:gcross.20091204093401.2972:Complex Double
-- @+node:gcross.20091204093401.3406:Function
-- @+at
--  instance (Bounded index,
--            Random index,
--            Projectable domain index,
--            Arbitrary domain)
--      => Arbitrary (Function domain index)
--    where
--      arbitrary = sized $ \size ->
--          case size of
--              0 -> return (Constant 0)
--              1 -> oneof
--                  [fmap Constant arbitrary
--                  ,fmap Projector (choose (minBound,maxBound))
--                  ]
--              _ -> do
--                  left_size <- choose (0,size)
--                  let right_size = size - left_size
--                  constructor <- elements [(:+:),(:*:)]
--                  liftM2 constructor
--                         (resize left_size arbitrary)
--                         (resize right_size arbitrary)
-- @-at
-- @@c
-- @-node:gcross.20091204093401.3406:Function
-- @+node:gcross.20091204093401.3501:Function
instance (Bounded index,
          Random index,
          Projectable domain index,
          Arbitrary domain)
    => Arbitrary (Function domain index)
  where
    arbitrary = sized $ \size ->
        if size <= 1 
            then oneof
                [fmap Constant arbitrary
                ,fmap Projector (choose (minBound,maxBound))
                ]
            else elements [(:+:),(:*:)]
                 >>= \constructor -> 
                    liftM2 constructor 
                       (resize (size `div` 2) arbitrary)
                       (resize (size `div` 2) arbitrary)

-- @-node:gcross.20091204093401.3501:Function
-- @-node:gcross.20091204093401.2971:Generators
-- @+node:gcross.20091204093401.3480:Instances
-- @+node:gcross.20091204093401.3482:Arbitrary XYZ
instance Arbitrary XYZ where
    arbitrary = elements [X,Y,Z]
-- @-node:gcross.20091204093401.3482:Arbitrary XYZ
-- @+node:gcross.20091204093401.3484:Random XYZ
instance Random XYZ where
    randomR (lo,hi) = first toEnum . randomR (fromEnum lo,fromEnum hi)
    random = randomR (X,Z)
-- @-node:gcross.20091204093401.3484:Random XYZ
-- @+node:gcross.20091204093401.3487:Random ()
instance Random () where
    randomR _ g = ((),g)
    random g = ((),g)
-- @-node:gcross.20091204093401.3487:Random ()
-- @-node:gcross.20091204093401.3480:Instances
-- @+node:gcross.20091204093401.3387:Classes
-- @+node:gcross.20091204093401.3388:AlmostEq
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = abs (x-y) / (abs (x+y) + 1e-100) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)

x /~ y = not (x ~= y)
-- @-node:gcross.20091204093401.3388:AlmostEq
-- @-node:gcross.20091204093401.3387:Classes
-- @+node:gcross.20091204093401.3391:Helpers
-- @+node:gcross.20091204093401.3416:simple
simple :: Projectable (Complex Double) () => FunctionTransformer (Complex Double) ()
simple = id
-- @-node:gcross.20091204093401.3416:simple
-- @+node:gcross.20091204093401.3419:_3d
_3d :: Projectable (Complex Double,Complex Double,Complex Double) XYZ =>
       FunctionTransformer (Complex Double,Complex Double,Complex Double) XYZ
_3d = id
-- @-node:gcross.20091204093401.3419:_3d
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
    -- @+node:gcross.20091204093401.3509:Euclidean3D
    [testGroup "Euclidean3D"
        -- @    @+others
        -- @+node:gcross.20091204093401.3514:Commutation relations
        ,testGroup "Commutation relations"
            -- @    @+others
            -- @+node:gcross.20091204093401.3515:[r_i,r_j] == 0
            [testProperty "[r_i,r_j] == 0" $
                \i j f v@(x,y,z) -> ((r_ i . r_ j) f $> v) ~= ((r_ j . r_ i) f $> v)
            -- @-node:gcross.20091204093401.3515:[r_i,r_j] == 0
            -- @+node:gcross.20091204093401.3516:[p_i,p_j] == 0
            ,testProperty "[p_i,p_j] == 0" $
                \i j f v@(x,y,z) -> ((p_ i . p_ j) f $> v) ~= ((p_ j . p_ i) f $> v)
            -- @-node:gcross.20091204093401.3516:[p_i,p_j] == 0
            -- @+node:gcross.20091204093401.3517:[r_i,p_j] == 0
            ,testProperty "[r_j,p_j] == i" $
                \j f v@(x,y,z) -> ((r_ j ~~ p_ j) f $> v) ~= (i * f $> v)
            -- @nonl
            -- @-node:gcross.20091204093401.3517:[r_i,p_j] == 0
            -- @-others
            ]
        -- @-node:gcross.20091204093401.3514:Commutation relations
        -- @-others
        ]
    -- @-node:gcross.20091204093401.3509:Euclidean3D
    -- @+node:gcross.20091204093401.2954:Function
    [testGroup "Function"
        -- @    @+others
        -- @+node:gcross.20091204093401.2955:($>)
        [testGroup "($>)"
            -- @    @+others
            -- @+node:gcross.20091204093401.2976:Constant
            [testProperty "Constant c $> x == c" $
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
            -- @+node:gcross.20091204093401.3407:Zero plus anything
            ,testProperty "(Constant 0 :+: f) $> x == 0" $
                \f x -> simple (Constant 0 :+: f) $> x == f $> x
            -- @-node:gcross.20091204093401.3407:Zero plus anything
            -- @+node:gcross.20091204093401.3413:Zero times anything
            ,testProperty "(Constant 0 :*: f) $> x == 0" $
                \f x -> simple (Constant 0 :*: f) $> x == 0
            -- @-node:gcross.20091204093401.3413:Zero times anything
            -- @+node:gcross.20091204093401.3411:One times anything
            ,testProperty "(Constant 1 :*: f) $> x == 0" $
                \f x -> simple (Constant 1 :*: f) $> x == f $> x
            -- @-node:gcross.20091204093401.3411:One times anything
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20091204093401.2955:($>)
        -- @+node:gcross.20091204093401.3399:d
        ,testGroup "d"
            -- @    @+others
            -- @+node:gcross.20091204093401.3405:Constant
            [testProperty "d (Constant x) == Constant 0" $
                \c -> d () (Constant c) == simple (Constant 0)
            -- @-node:gcross.20091204093401.3405:Constant
            -- @+node:gcross.20091204093401.3414:Linearity
            ,testProperty "d (f :+: g) = d f :+: d g" $
                \f g x -> _3d (d x (f :+: g)) == d x f :+: d x g
            -- @-node:gcross.20091204093401.3414:Linearity
            -- @+node:gcross.20091204093401.3426:Products
            ,testProperty "d (f :*: g) = d f :+: d g" $
                \f g x -> _3d (d x (f :*: g)) == (d x f :*: g) :+: (f :*: d x g)
            -- @nonl
            -- @-node:gcross.20091204093401.3426:Products
            -- @-others
            ]
        -- @-node:gcross.20091204093401.3399:d
        -- @-others
        ]
    -- @-node:gcross.20091204093401.2954:Function
    -- @-others
    -- @-node:gcross.20091204093401.2953:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091204093401.2948:@thin test.hs
-- @-leo
