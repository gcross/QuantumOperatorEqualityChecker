-- @+leo-ver=4-thin
-- @+node:gcross.20091204093401.2935:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091204093401.2936:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091204093401.2936:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091204093401.2937:<< Import needed modules >>
import Blueprint.Tools.GHC.Simple
-- @-node:gcross.20091204093401.2937:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091204093401.2938:main
main = defaultMain [("","sources")]
                   (Just ([("","tests")],
                    ["HUnit == 1.*"
                    ,"QuickCheck == 2.*"
                    ,"test-framework == 0.2.*"
                    ,"test-framework-hunit == 0.2.*"
                    ,"test-framework-quickcheck2 == 0.2.*"
                    ,"random == 1.*"
                    ]
                   ))
-- @-node:gcross.20091204093401.2938:main
-- @-others
-- @-node:gcross.20091204093401.2935:@thin Setup.hs
-- @-leo
