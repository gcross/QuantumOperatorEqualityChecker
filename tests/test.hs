-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.2948:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2949:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091204093401.2949:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.2950:<< Import needed modules >>
import Data.Complex
import Data.List

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Quantum.Euclidean3D
import Quantum.Function
import Quantum.Function.Transformers
import Quantum.Testing
-- @-node:gcross.20091204093401.2950:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.3645:Helpers
-- @+node:gcross.20091204093401.3646:simple
simple :: Projectable (Complex Double) () (Complex Double) =>
          FunctionTransformer (Complex Double) () (Complex Double)
simple = id
-- @-node:gcross.20091204093401.3646:simple
-- @+node:gcross.20091204093401.3647:_3d
_3d :: Projectable (ThreeDimensions (Complex Double)) XYZ (Complex Double) =>
       FunctionTransformer (ThreeDimensions (Complex Double)) XYZ (Complex Double)
_3d = id
-- @-node:gcross.20091204093401.3647:_3d
-- @+node:gcross.20091206113753.1499:_3dt
_3dt :: Projectable (ThreeDimensions (Complex Double)) XYZ (Complex Double) =>
       FunctionTransformer (ThreeDimensions (Complex Double)) XYZ (Complex Double) ->
       FunctionTransformer (ThreeDimensions (Complex Double)) XYZ (Complex Double)
_3dt = id
-- @-node:gcross.20091206113753.1499:_3dt
-- @-node:gcross.20091204093401.3645:Helpers
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091204093401.2953:<< Tests >>
    -- @+others
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
                \c1 c2 x -> simple (Constant c1 :+: Constant c2) $> x ~= c1 + c2
            -- @-node:gcross.20091204093401.2978:Sum over constants
            -- @+node:gcross.20091204093401.3394:Multiplication over constants
            ,testProperty "Multiplication over constants" $
                \c1 c2 x -> simple (Constant c1 :*: Constant c2) $> x ~= c1 * c2
            -- @-node:gcross.20091204093401.3394:Multiplication over constants
            -- @+node:gcross.20091204093401.3396:Product over sums
            ,testProperty "Product over sums" $
                \c1 c2 c3 c4 x -> simple (
                                     (Constant c1 :+: Constant c2)
                                        :*:
                                     (Constant c3 :+: Constant c4)
                                   ) $> x ~= (c1 + c2) * (c3 + c4)
            -- @-node:gcross.20091204093401.3396:Product over sums
            -- @+node:gcross.20091204093401.3398:Sum over products
            ,testProperty "Sum over products" $
                \c1 c2 c3 c4 x -> simple (
                                     (Constant c1 :*: Constant c2)
                                        :+:
                                     (Constant c3 :*: Constant c4)
                                   ) $> x ~= (c1 * c2) + (c3 * c4)
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
        -- @-others
        ]
    -- @-node:gcross.20091204093401.2954:Function
    -- @+node:gcross.20091204093401.3589:Function.Transformer
    ,testGroup "Function.Transformer"
        -- @    @+others
        -- @+node:gcross.20091204093401.3399:d
        [testGroup "d"
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
    -- @-node:gcross.20091204093401.3589:Function.Transformer
    -- @+node:gcross.20091204093401.3509:Euclidean3D
    ,testGroup "Euclidean3D"
        -- @    @+others
        -- @+node:gcross.20091204093401.3514:Commutation relations
        [testGroup "Commutation relations"
            -- @    @+others
            -- @+node:gcross.20091204093401.3515:[r_i,r_j] == 0
            [testProperty "[r_i,r_j] == 0" $
                \i j f v@(x,y,z) -> _3dt (r_ i . r_ j) f $> v ~= (r_ j . r_ i) f $> v
            -- @-node:gcross.20091204093401.3515:[r_i,r_j] == 0
            -- @+node:gcross.20091204093401.3516:[p_i,p_j] == 0
            ,testProperty "[p_i,p_j] == 0" $
                \i j f v@(x,y,z) -> _3dt (p_ i . p_ j) f $> v ~= (p_ j . p_ i) f $> v
            -- @-node:gcross.20091204093401.3516:[p_i,p_j] == 0
            -- @+node:gcross.20091204093401.3517:[r_i,p_j] == 0
            ,testProperty "[r_j,p_j] == i" $
                \j f v@(x,y,z) -> _3dt (r_ j ~~ p_ j) f $> v ~= i * (f $> v)

            -- @-node:gcross.20091204093401.3517:[r_i,p_j] == 0
            -- @-others
            ]
        -- @-node:gcross.20091204093401.3514:Commutation relations
        -- @-others
        ]
    -- @-node:gcross.20091204093401.3509:Euclidean3D
    -- @-others
    -- @-node:gcross.20091204093401.2953:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091204093401.2948:@thin test.hs
-- @-leo
